# =============================================================================
# Supplementary data: page topics, age, edit activity, pageviews, link counts
# Run AFTER collect_wiki_network.r (it reads data/nodes.csv). Run it once,
# separately from the report; the report only reads the files it writes.
#
# Output (all in data/):
#   page_categories.csv   title, category          -> used to assign a domain
#   page_meta.csv         title, length, created, edits_12m, last_edit
#   edits_12m.csv         title, timestamp         -> every edit in the last 12 months
#   pageviews_monthly.csv title, month, views      -> reader popularity over time
#   total_links.csv       title, all_links         -> links to ANY Wikipedia article (RQ4)
#   wiki_stats.csv        total articles on English Wikipedia (RQ4 baseline)
#
# Time: about 3 requests per article; ~950 articles takes roughly 10-15 minutes.
# Progress is cached in cache/meta_*, so an interrupted run resumes where it stopped.
# =============================================================================

# install.packages(c("tidyverse", "httr2", "here"))
library(tidyverse)
library(httr2)
library(here)   # here() = the project folder, wherever this script is run from

API       <- "https://en.wikipedia.org/w/api.php"
PV_API    <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/user"
UA        <- "COSC421Project (kangnade@gmail.com)"
CACHE_DIR <- here("cache")
OUT_DIR   <- here("data")
PV_START  <- "20160101"                                             # pageviews start
PV_END    <- format(floor_date(Sys.Date(), "month") - 1, "%Y%m%d")  # end of last full month
SINCE     <- Sys.time() - as.difftime(365, units = "days")          # "last 12 months"
CHUNK     <- 200    # articles per cache file
# Same pages the report removes (citation identifiers, copyright notice)
EXCLUDE_PATTERN <- "\\(identifier\\)$|^Copyright status of works"
dir.create(CACHE_DIR, showWarnings = FALSE)

nodes  <- read_csv(file.path(OUT_DIR, "nodes.csv"), show_col_types = FALSE)
titles <- nodes$title[!str_detect(nodes$title, EXCLUDE_PATTERN)]
cat("Articles to process:", length(titles), "\n")

# ---- Helpers -----------------------------------------------------------------
# Same idea as in collect_wiki_network.r, plus:
#   max_pages: stop following 'continue' after this many pages
#   maxlag   : if Wikipedia's servers are busy, wait and retry instead of failing
wiki_query <- function(params, max_pages = Inf) {
  out <- list(); cont <- list(); pages <- 0
  repeat {
    resp <- request(API) |>
      req_user_agent(UA) |>
      req_url_query(!!!c(list(action = "query", format = "json",
                              formatversion = 2, maxlag = 5), params, cont)) |>
      req_throttle(rate = 5) |>
      req_retry(max_tries = 5) |>
      req_perform() |>
      resp_body_json()
    if (!is.null(resp$error)) {
      if (identical(resp$error$code, "maxlag")) { Sys.sleep(5); next }
      stop(resp$error$info)
    }
    out <- c(out, list(resp$query)); pages <- pages + 1
    if (is.null(resp$continue) || pages >= max_pages) break
    cont <- resp$continue
  }
  out
}

# Run fun() on titles in chunks; each chunk is cached so a re-run resumes
cached_chunks <- function(name, titles, fun) {
  chunks <- split(titles, ceiling(seq_along(titles) / CHUNK))
  imap(chunks, \(ch, i) {
    f <- file.path(CACHE_DIR, sprintf("meta_%s_%03d.rds", name, as.integer(i)))
    if (file.exists(f)) return(readRDS(f))
    message(sprintf("%s: chunk %s of %d", name, i, length(chunks)))
    res <- fun(ch)
    saveRDS(res, f)
    res
  }) |> list_rbind()
}
batches_of_50 <- \(ts) split(ts, ceiling(seq_along(ts) / 50))

# ---- 0. Resolve redirects ----------------------------------------------------
# Some titles in nodes.csv are redirects (an old or alternative name). A redirect
# page has no categories, edits or real pageviews of its own, so every query
# below uses the article the redirect points to ("canonical"). Results are saved
# under the title used in nodes.csv so they join straight onto the network.
resolve_titles <- function(ts) {
  batches_of_50(ts) |> map(\(b) {
    res  <- wiki_query(list(titles = paste(b, collapse = "|"), redirects = 1))
    # compact() drops replies with no "normalized"/"redirects" list (most batches)
    norm <- res |> map("normalized") |> compact() |> list_flatten()
    rdir <- res |> map("redirects")  |> compact() |> list_flatten()
    tibble(title = b) |>
      left_join(tibble(title = map_chr(norm, "from", .default = NA), norm = map_chr(norm, "to", .default = NA)) |>
                  distinct(title, .keep_all = TRUE), by = "title") |>
      mutate(norm = coalesce(norm, title)) |>
      left_join(tibble(norm = map_chr(rdir, "from", .default = NA), canonical = map_chr(rdir, "to", .default = NA)) |>
                  distinct(norm, .keep_all = TRUE), by = "norm") |>
      transmute(title, canonical = coalesce(canonical, norm))
  }) |> list_rbind()
}
lookup     <- cached_chunks("resolve", titles, resolve_titles)
canonical  <- unique(lookup$canonical)
cat("Redirect titles resolved:", sum(lookup$title != lookup$canonical), "\n")
# Attach results (keyed by canonical title) back to the nodes.csv titles
to_node_titles <- \(df) lookup |> inner_join(df, by = c("canonical" = "title"),
                                             relationship = "many-to-many") |>
  select(-canonical)

# ---- 1. Categories and page length (50 titles per request) -------------------
# clshow = "!hidden" drops maintenance categories such as
# "Articles with short description".
get_categories_info <- function(ts) {
  batches_of_50(ts) |>
    map(\(b) {
      res   <- wiki_query(list(prop = "categories|info", titles = paste(b, collapse = "|"),
                               clshow = "!hidden", cllimit = "max"))
      pages <- res |> map("pages") |> list_flatten()
      bind_rows(
        map(pages, \(p) if (length(p$categories) > 0)
          tibble(title = p$title, type = "cat",
                 category = str_remove(map_chr(p$categories, "title"), "^Category:"))) |>
          list_rbind(),
        map(pages, \(p) if (!is.null(p$length))
          tibble(title = p$title, type = "info", length = p$length)) |>
          list_rbind())
    }) |> list_rbind()
}
cat_info <- cached_chunks("catinfo", canonical, get_categories_info)
page_categories <- cat_info |> filter(type == "cat")  |> distinct(title, category) |> to_node_titles()
page_length     <- cat_info |> filter(type == "info") |> distinct(title, length)

# ---- 2. Total number of article links (for RQ4) ------------------------------
# Counts links to ANY Wikipedia article, not just to articles in this network.
get_link_counts <- function(ts) {
  batches_of_50(ts) |>
    map(\(b) {
      res <- wiki_query(list(prop = "links", titles = paste(b, collapse = "|"),
                             plnamespace = 0, pllimit = "max"))
      res |> map("pages") |> list_flatten() |>
        map(\(p) tibble(title = p$title, n = length(p$links %||% list()))) |>
        list_rbind()
    }, .progress = "Link counts") |>
    list_rbind() |>
    group_by(title) |> summarise(all_links = sum(n), .groups = "drop")
}
total_links <- cached_chunks("links", canonical, get_link_counts) |>
  group_by(title) |> summarise(all_links = sum(all_links), .groups = "drop") |>
  to_node_titles()

# ---- 3. Creation date and edits in the last 12 months (one article at a time) -
get_revision_stats <- function(title) {
  # First revision = creation date. max_pages = 1 stops after the first answer.
  first <- wiki_query(list(prop = "revisions", titles = title, rvprop = "timestamp",
                           rvlimit = 1, rvdir = "newer"), max_pages = 1)
  created <- first[[1]]$pages[[1]]$revisions[[1]]$timestamp %||% NA_character_
  # All edits from now back to SINCE (newest first)
  recent <- wiki_query(list(prop = "revisions", titles = title, rvprop = "timestamp",
                            rvlimit = "max",
                            rvend = format(SINCE, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
  ts <- recent |> map(\(q) map_chr(q$pages[[1]]$revisions %||% list(), "timestamp")) |>
    unlist()
  bind_rows(
    tibble(title, type = "meta", created, edits_12m = length(ts),
           last_edit = if (length(ts)) max(ts) else NA_character_),
    tibble(title = rep(title, length(ts)), type = rep("edit", length(ts)), timestamp = ts))
}
rev_stats <- cached_chunks("revisions", canonical,
                           \(ch) map(ch, get_revision_stats, .progress = "Revisions") |>
                             list_rbind())

# ---- 4. Monthly pageviews (Wikimedia REST API, one article at a time) --------
# Agent "user" excludes known bots. 404 means no recorded views -> skipped.
get_pageviews <- function(title) {
  url  <- paste(PV_API, URLencode(str_replace_all(title, " ", "_"), reserved = TRUE),
                "monthly", PV_START, PV_END, sep = "/")
  resp <- request(url) |>
    req_user_agent(UA) |>
    req_throttle(rate = 10) |>
    req_retry(max_tries = 5) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  if (resp_status(resp) != 200) return(NULL)
  items <- resp_body_json(resp)$items
  tibble(title,
         month = as.Date(substr(map_chr(items, "timestamp"), 1, 8), "%Y%m%d"),
         views = map_dbl(items, "views"))
}
pageviews <- cached_chunks("pageviews", canonical,
                           \(ch) map(ch, get_pageviews, .progress = "Pageviews") |>
                             list_rbind()) |>
  to_node_titles()

# ---- 5. Size of English Wikipedia (baseline for RQ4) -------------------------
stats <- wiki_query(list(meta = "siteinfo", siprop = "statistics"))[[1]]$statistics
wiki_stats <- tibble(articles = stats$articles, retrieved = Sys.Date())

# ---- 6. Save -----------------------------------------------------------------
page_meta <- rev_stats |> filter(type == "meta") |>
  select(title, created, edits_12m, last_edit) |>
  left_join(page_length, by = "title") |>
  to_node_titles() |>
  mutate(created = ymd_hms(created), last_edit = ymd_hms(last_edit))
edits_12m <- rev_stats |> filter(type == "edit") |>
  select(title, timestamp) |>
  to_node_titles() |>
  mutate(timestamp = ymd_hms(timestamp))

write_csv(page_categories, file.path(OUT_DIR, "page_categories.csv"))
write_csv(page_meta,       file.path(OUT_DIR, "page_meta.csv"))
write_csv(edits_12m,       file.path(OUT_DIR, "edits_12m.csv"))
write_csv(pageviews,       file.path(OUT_DIR, "pageviews_monthly.csv"))
write_csv(total_links,     file.path(OUT_DIR, "total_links.csv"))
write_csv(wiki_stats,      file.path(OUT_DIR, "wiki_stats.csv"))

cat("Done.\n",
    "Articles with categories:", n_distinct(page_categories$title), "\n",
    "Articles with pageviews: ", n_distinct(pageviews$title), "\n",
    "Articles with link counts:", n_distinct(total_links$title), "\n",
    "Edits in last 12 months: ", nrow(edits_12m), "\n")
