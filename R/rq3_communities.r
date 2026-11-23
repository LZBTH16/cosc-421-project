# =============================================================================
# rq3_communities.r - RQ3: Does the network split into meaningful communities?
# Outputs: tab_rq3_*, fig_communities, fig_community_graph, results/rq3_membership.csv
# =============================================================================
source(here::here("R", "rq0_setup.r"))
set_rq("rq3")   # outputs go to results/rq3/, paper/figures/rq3/, paper/generated/rq3/

# =============================================================================
# RQ3: communities (full network, then category articles only)
# =============================================================================
community_checks <- function(gu_x, gd_x) {
  set.seed(2026)
  louv <- cluster_louvain(gu_x)
  size_rank <- rank(-tabulate(membership(louv)), ties.method = "first")
  memb <- setNames(size_rank[membership(louv)], V(gu_x)$name)
  Q_null <- map_dbl(1:5, \(s) { set.seed(s)
    modularity(cluster_louvain(rewire(gu_x, keeping_degseq(niter = 10 * ecount(gu_x))))) })
  runs <- map(1:10, \(s) { set.seed(s); membership(cluster_louvain(gu_x)) })
  im   <- cluster_infomap(gd_x)
  groups <- V(g)$subfield[match(names(memb), V(g)$name)]
  stats <- c(
    `Articles`                        = vcount(gu_x),
    `Communities (5+ articles)`       = sum(tabulate(memb) >= 5),
    `Modularity Q`                    = modularity(gu_x, memb),
    `Q of rewired networks (mean)`    = mean(Q_null),
    `NMI between Louvain runs (mean)` = mean(combn(10, 2, \(p) compare(runs[[p[1]]], runs[[p[2]]], "nmi"))),
    `Infomap communities`             = length(unique(membership(im))),
    `NMI Louvain vs Infomap`          = compare(memb, membership(im)[match(names(memb), V(gd_x)$name)], "nmi"),
    `NMI vs subfield`                 = compare(memb, as.integer(factor(groups)), "nmi"))
  list(memb = memb, stats = stats)
}
full_res <- community_checks(gu_giant, g_giant)

cat_names <- V(g)$name[V(g)$subfield %in% category_subfields]
gc_d <- induced_subgraph(g, cat_names)
cc   <- components(gc_d, mode = "weak")
gc_d <- induced_subgraph(gc_d, which(cc$membership == which.max(cc$csize)))
gc_u <- to_undirected(gc_d, mode = "collapse")
cat_res <- community_checks(gc_u, gc_d)

fmt_stat <- \(x) map_chr(x, \(v) if (v == round(v)) fmt_num(v) else sprintf("%.2f", v))
# (saved further down, once the participation coefficients are known)
checks_tbl <- tibble(Statistic = names(full_res$stats), `Full network` = fmt_stat(full_res$stats),
                     `Category articles` = fmt_stat(cat_res$stats))
num("Qfull", full_res$stats[["Modularity Q"]]); num("QfullNull", full_res$stats[["Q of rewired networks (mean)"]])
num("Qcat", cat_res$stats[["Modularity Q"]]);   num("QcatNull", cat_res$stats[["Q of rewired networks (mean)"]])
num("nCommFull", full_res$stats[["Communities (5+ articles)"]])
num("nCommCat", cat_res$stats[["Communities (5+ articles)"]])
num("nmiCatSubfield", cat_res$stats[["NMI vs subfield"]])
num("nmiCatInfomap", cat_res$stats[["NMI Louvain vs Infomap"]])
num("nCatGiant", vcount(gc_u))
num("nmiFullInfomap", fmt_stat(full_res$stats[["NMI Louvain vs Infomap"]]))
num("nmiCatRuns", fmt_stat(cat_res$stats[["NMI between Louvain runs (mean)"]]))

# Full network: which subfields fall into which community
tibble(article = names(full_res$memb), community = full_res$memb) |>
  filter(community %in% which(tabulate(full_res$memb) >= 5)) |>
  mutate(subfield = factor(V(g)$subfield[match(article, V(g)$name)], levels = subfield_levels),
         Community = paste0("C", community)) |>
  count(Community, subfield) |>
  pivot_wider(names_from = subfield, values_from = n, values_fill = 0) |>
  save_table("tab_rq3_full_by_subfield")

# Category articles: community profiles
memb <- cat_res$memb; n_comm <- max(memb)
comm_tbl <- tibble(article = names(memb), community = memb) |>
  left_join(select(cent, article, subfield, domain), by = "article") |>
  mutate(degree_within = degree(gc_u, article))
comm_profile <- comm_tbl |>
  group_by(community) |>
  summarise(size = n(),
            top_articles = paste(short_label(article[order(-degree_within)][1:min(2, n())], 24),
                                 collapse = "; "),
            main_subfield = names(which.max(table(subfield))),
            main_share = max(table(subfield)) / n(), .groups = "drop") |>
  arrange(community)
K <- min(10, sum(comm_profile$size >= 5))
comm_name <- setNames(sprintf("C%d: %s", comm_profile$community,
                              short_label(word(comm_profile$top_articles, 1, sep = "; "), 24)),
                      comm_profile$community)
comm_profile |> filter(community <= K) |>
  transmute(C = paste0("C", community), Size = size, `Main subfield` = sub_short[main_subfield],
            `Share` = main_share, `Most connected articles` = top_articles) |>
  save_table("tab_rq3_communities", digits = 2)
tibble(article = names(memb), community = memb) |>
  filter(community <= K) |>
  mutate(subfield = factor(V(g)$subfield[match(article, V(g)$name)], levels = category_subfields),
         Community = factor(paste0("C", community), levels = paste0("C", seq_len(K)))) |>
  count(Community, subfield, .drop = FALSE) |>
  pivot_wider(names_from = subfield, values_from = n, values_fill = 0) |>
  save_table("tab_rq3_cat_by_subfield")

comm_cols <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
               "#E6AB02", "#A6761D", "#1F78B4", "#E31A1C", "#6A3D9A")[seq_len(K)]
set.seed(42)
lay_c  <- norm_coords(layout_with_fr(gc_u, niter = 1000))
vc     <- memb[V(gc_u)$name]
deg_c  <- degree(gc_u)
hub_ids <- map_int(seq_len(K), \(k) { ids <- which(vc == k); ids[which.max(deg_c[ids])] })
save_fig(\() draw_net(
  gc_u, lay_c, 0.6 + 3 * sqrt(deg_c / max(deg_c)),
  adjustcolor(ifelse(vc <= K, comm_cols[pmin(vc, K)], "grey80"), 0.85),
  hub_ids, paste0("C", seq_len(K)), edge_alpha = 0.12,
  key2 = list(title = "Community (top article)", legend = comm_name[seq_len(K)], col = comm_cols)),
  "fig_communities", 7, 5)

E(gc_u)$weight <- 1
cg <- contract(gc_u, memb[V(gc_u)$name], vertex.attr.comb = "ignore")
cg <- igraph::simplify(cg, remove.loops = TRUE, edge.attr.comb = list(weight = "sum"))
V(cg)$name <- as.character(seq_len(vcount(cg)))
cg_top <- induced_subgraph(cg, as.character(seq_len(K)))
csize  <- comm_profile$size[seq_len(K)]
save_fig(\() circle_graph(cg_top, sprintf("%s (%d)", comm_name[seq_len(K)], csize), csize,
                          vsize = 4 + 10 * sqrt(csize / max(csize)), vcol = comm_cols,
                          ewidth = 0.5 + 8 * E(cg_top)$weight / max(E(cg_top)$weight),
                          arrows = FALSE),
         "fig_community_graph", 6, 6)

# Participation coefficient and betweenness within the categories
adj <- as_adj_list(gc_u); mv <- memb[V(gc_u)$name]
P <- map_dbl(seq_along(adj), \(i) { nb <- as.integer(adj[[i]]); k <- length(nb)
  if (k == 0) 0 else 1 - sum((tabulate(mv[nb], n_comm) / k)^2) }) |> setNames(V(gc_u)$name)
btw_cat <- betweenness(gc_d, directed = TRUE, normalized = TRUE)
cent$community <- unname(memb[cent$article]); cent$participation <- unname(P[cent$article])
cat_bridges <- tibble(Article = short_label(V(gc_u)$name), Community = paste0("C", mv),
                      `Betw. within categories` = btw_cat[V(gc_u)$name],
                      `Participation P` = P) |>
  slice_max(`Betw. within categories`, n = 10, with_ties = FALSE)
save_table(cat_bridges, "tab_rq3_cat_bridges")
# ---- Are the communities real subfields, or just densely linked groups? ------
# Links were used to FIND the communities, so we check them against two signals
# that do not come from the links:
#   1. Wikipedia categories (written by editors). For every pair of articles we
#      check whether they share at least one category, and compare pairs in the
#      SAME community with pairs in DIFFERENT communities. The three categories
#      we crawled and "...stubs" categories are left out, because every member
#      of a subfield has its crawled category by construction. If communities
#      were only "articles that link a lot", same-community pairs would not
#      share categories more often than other pairs (ratio close to 1).
#   2. The domain labels from rq0 (keyword rules on categories and titles):
#      NMI between communities and domains, against 100 random groupings of
#      the same sizes.
seed_cats <- str_remove(category_subfields, "^Category:")
art_cats <- if (has_meta) page_categories |>
  filter(!category %in% seed_cats, !str_detect(category, "stubs$")) |>
  distinct(title, category) else tibble(title = character(), category = character())

category_check <- function(membership) {
  arts <- names(membership)
  ac <- filter(art_cats, title %in% arts)
  inc <- table(factor(ac$title, levels = arts), ac$category)       # article x category
  shares <- (tcrossprod(inc > 0) > 0)[upper.tri(diag(length(arts)))]  # pair shares a category?
  same <- outer(membership, membership, "==")[upper.tri(diag(length(arts)))]
  dom <- as.integer(factor(cent$domain[match(arts, cent$article)]))
  set.seed(2026)
  c(same = mean(shares[same]), diff = mean(shares[!same]),
    ratio = mean(shares[same]) / mean(shares[!same]),
    nmi_dom = compare(membership, dom, "nmi"),
    nmi_null = mean(map_dbl(1:100, \(i) compare(sample(membership), dom, "nmi"))))
}
pct <- \(x) sprintf("%.1f%%", 100 * x)
if (has_meta) {
  chk_full <- category_check(full_res$memb)
  chk_cat  <- category_check(cat_res$memb)
  checks_tbl <- checks_tbl |>
    add_row(Statistic = "Pairs sharing a category: same community",
            `Full network` = pct(chk_full[["same"]]), `Category articles` = pct(chk_cat[["same"]])) |>
    add_row(Statistic = "Pairs sharing a category: different communities",
            `Full network` = pct(chk_full[["diff"]]), `Category articles` = pct(chk_cat[["diff"]])) |>
    add_row(Statistic = "Ratio same / different",
            `Full network` = sprintf("%.1f", chk_full[["ratio"]]), `Category articles` = sprintf("%.1f", chk_cat[["ratio"]])) |>
    add_row(Statistic = "NMI vs domain",
            `Full network` = fmt_stat(chk_full[["nmi_dom"]]), `Category articles` = fmt_stat(chk_cat[["nmi_dom"]])) |>
    add_row(Statistic = "NMI vs domain, random groups (mean of 100)",
            `Full network` = fmt_stat(chk_full[["nmi_null"]]), `Category articles` = fmt_stat(chk_cat[["nmi_null"]]))
  # macros taken from the same formatted cells (the % sign is added in the text)
  num("catSameComm", sprintf("%.1f", 100 * chk_cat[["same"]]))
  num("catDiffComm", sprintf("%.1f", 100 * chk_cat[["diff"]]))
  num("catRatio",    sprintf("%.1f", chk_cat[["ratio"]]))
  num("nmiCatDomain",     fmt_stat(chk_cat[["nmi_dom"]]))
  num("nmiCatDomainNull", fmt_stat(chk_cat[["nmi_null"]]))

  # Table: each category-article community and its most common Wikipedia
  # category, a label chosen by editors rather than by us
  tibble(title = names(cat_res$memb), community = cat_res$memb) |>
    filter(community <= K) |>
    add_count(community, name = "size") |>
    inner_join(art_cats, by = "title", relationship = "many-to-many") |>
    count(community, size, category, name = "n_members") |>
    group_by(community) |>
    slice_max(n_members, n = 1, with_ties = FALSE) |>
    ungroup() |>
    arrange(community) |>
    transmute(C = paste0("C", community), Size = size,
              `Most common category (excl. crawled ones)` = category,
              `Members` = n_members) |>
    save_table("tab_rq3_comm_categories")
}

# Add the participation medians to the checks table, and take the macros from
# the table cells so the text and the table always show the same value
checks_tbl <- checks_tbl |>
  add_row(Statistic = "Median participation P (all articles)", `Full network` = "--",
          `Category articles` = fmt_stat(median(P))) |>
  add_row(Statistic = "Median participation P (top 10 bridges)", `Full network` = "--",
          `Category articles` = fmt_stat(median(cat_bridges$`Participation P`)))
save_table(checks_tbl, "tab_rq3_checks", align = "lrr")
num("medianP", fmt_stat(median(P)))
num("medianPBridges", fmt_stat(median(cat_bridges$`Participation P`)))

write_csv(tibble(article = names(memb), community = memb, community_name = comm_name[as.character(memb)]),
          rq_out("rq3_membership.csv"))

write_numbers()
message("rq3_communities.r done.")
