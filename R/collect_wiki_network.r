# =============================================================================
# Collect the Computer Networking hyperlink graph from Wikipedia
# Output: data/nodes.csv, data/edges.csv, data/net_graph.rds (igraph object)
# =============================================================================

# install.packages(c("tidyverse", "httr2", "igraph"))
library(tidyverse)
library(httr2)
library(igraph)

# ---- 0. Settings -------------------------------------------------------------
API  <- "https://en.wikipedia.org/w/api.php"
# Wikimedia asks every client to identify itself.
UA   <- "COSC421Project (eyakovle@student.ubc.ca)"

SEED_ARTICLE   <- "Computer network"
SEED_CATEGORIES <- c(
  "Category:Network protocols",
  "Category:Routing protocols",
  "Category:Wireless networking"
)
CAT_DEPTH    <- 0   # 0 = direct members only; 1 = also members of subcategories
MIN_INLINKS  <- 3   # a page linked from the seed article is kept only if it is
                    # also linked from at least this many category articles
CACHE_DIR    <- "cache"
OUT_DIR      <- "data"
dir.create(CACHE_DIR, showWarnings = FALSE)
dir.create(OUT_DIR,   showWarnings = FALSE)

# ---- 1. Generic API helper (handles 'continue' pagination) -------------------
wiki_query <- function(params) {
  out  <- list()
  cont <- list()
  repeat {
    resp <- request(API) |>
      req_user_agent(UA) |>
      req_url_query(!!!c(list(action = "query", format = "json",
                              formatversion = 2, maxlag = 5),
                         params, cont)) |>
      req_throttle(rate = 5) |>                # max ~5 requests / second
      req_retry(max_tries = 5) |>
      req_perform() |>
      resp_body_json()
    if (!is.null(resp$error)) stop(resp$error$info)
    out <- c(out, list(resp$query))
    if (is.null(resp$continue)) break
    cont <- resp$continue
  }
  out
}

# Simple on-disk cache so a re-run doesn't re-crawl everything
cached <- function(name, expr) {
  f <- file.path(CACHE_DIR, paste0(name, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  res <- force(expr)
  saveRDS(res, f)
  res
}

# ---- 2. Category members (with optional recursion into subcategories) --------
get_category_members <- function(cat) {
  res <- wiki_query(list(list = "categorymembers", cmtitle = cat,
                         cmtype = "page|subcat", cmlimit = "max"))
  members <- res |> map("categorymembers") |> list_flatten()
  if (length(members) == 0) return(tibble(title = character(), ns = integer()))
  bind_rows(members) |> select(title, ns)
}

crawl_categories <- function(cats, depth) {
  seen_cats <- character()
  pages     <- tibble(title = character(), source_cat = character())
  frontier  <- cats
  for (d in 0:depth) {
    next_frontier <- character()
    for (cat in setdiff(frontier, seen_cats)) {
      m <- get_category_members(cat)
      if (nrow(m) == 0) warning("No members found for ", cat, " (check spelling)")
      pages <- bind_rows(pages,
                         m |> filter(ns == 0) |> transmute(title, source_cat = cat))
      next_frontier <- c(next_frontier, m$title[m$ns == 14])  # ns 14 = category
      seen_cats <- c(seen_cats, cat)
    }
    frontier <- unique(next_frontier)
  }
  distinct(pages, title, .keep_all = TRUE)
}

# ---- 3. Outgoing article links for many titles (50 titles per request) -------
get_links <- function(titles) {
  batches <- split(titles, ceiling(seq_along(titles) / 50))
  map(batches, \(b) {
    res <- wiki_query(list(prop = "links", titles = paste(b, collapse = "|"),
                           plnamespace = 0, pllimit = "max", redirects = 1))
    res |> map("pages") |> list_flatten() |>
      map(\(p) if (length(p$links) > 0)
        tibble(from = p$title, to = map_chr(p$links, "title"))) |>
      list_rbind()
  }, .progress = "Fetching links") |>
    list_rbind() |>
    distinct()
}

# ---- 4. Redirect map: alias title -> canonical title -------------------------
# Article A may link to "TCP" while the node is "Transmission Control Protocol".
get_redirect_map <- function(titles) {
  batches <- split(titles, ceiling(seq_along(titles) / 50))
  map(batches, \(b) {
    res <- wiki_query(list(prop = "redirects", titles = paste(b, collapse = "|"),
                           rdnamespace = 0, rdlimit = "max"))
    res |> map("pages") |> list_flatten() |>
      map(\(p) if (length(p$redirects) > 0)
        tibble(alias = map_chr(p$redirects, "title"), canonical = p$title)) |>
      list_rbind()
  }, .progress = "Fetching redirects") |>
    list_rbind() |>
    distinct(alias, .keep_all = TRUE)
}

# ---- 5. Build the node set ---------------------------------------------------
# Drop navigation pages that would act as artificial hubs
is_nav_page <- function(t) str_detect(t, "^(List|Lists|Outline|Glossary|Index|Timeline|Comparison) of ")

cat_pages <- cached("cat_pages", crawl_categories(SEED_CATEGORIES, CAT_DEPTH)) |>
  filter(!is_nav_page(title))

core <- union(cat_pages$title, SEED_ARTICLE)

# Links out of the core articles
core_links <- cached("core_links", get_links(core))

# Depth-1 neighbours of the seed article
seed_nbrs <- core_links |> filter(from == SEED_ARTICLE) |> pull(to)

# Keep seed neighbours that the networking articles themselves link to often.
# This removes off-topic links (countries, people, years) from the seed page.
extra <- core_links |>
  filter(to %in% seed_nbrs, !to %in% core, from != SEED_ARTICLE) |>
  count(to, name = "inlinks_from_core") |>
  filter(inlinks_from_core >= MIN_INLINKS, !is_nav_page(to)) |>
  pull(to)

extra_links <- cached("extra_links", get_links(extra))

node_titles <- union(core, extra)

# ---- 6. Clean the edge list --------------------------------------------------
redirects <- cached("redirects", get_redirect_map(node_titles))

edges <- bind_rows(core_links, extra_links) |>
  left_join(redirects, by = c("to" = "alias")) |>
  mutate(to = coalesce(canonical, to)) |>
  select(from, to) |>
  filter(from %in% node_titles, to %in% node_titles, from != to) |>
  distinct()

nodes <- tibble(title = node_titles) |>
  left_join(cat_pages, by = "title") |>
  mutate(origin = case_when(title == SEED_ARTICLE ~ "seed",
                            !is.na(source_cat)    ~ "category",
                            TRUE                  ~ "seed_link")) |>
  # keep only nodes that ended up with at least one edge
  filter(title %in% c(edges$from, edges$to))

# ---- 7. Save and sanity-check ------------------------------------------------
write_csv(nodes, file.path(OUT_DIR, "nodes.csv"))
write_csv(edges, file.path(OUT_DIR, "edges.csv"))

g <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)
saveRDS(g, file.path(OUT_DIR, "net_graph.rds"))

cat("Nodes:", vcount(g), " Edges:", ecount(g),
    " Density:", signif(edge_density(g), 3), "\n")
nodes |> count(origin)
tibble(article = V(g)$name, in_deg = degree(g, mode = "in")) |>
  slice_max(in_deg, n = 10)
