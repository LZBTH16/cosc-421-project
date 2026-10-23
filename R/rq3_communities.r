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
tibble(Statistic = names(full_res$stats), `Full network` = fmt_stat(full_res$stats),
       `Category articles` = fmt_stat(cat_res$stats)) |>
  save_table("tab_rq3_checks", align = "lrr")
num("Qfull", full_res$stats[["Modularity Q"]]); num("QfullNull", full_res$stats[["Q of rewired networks (mean)"]])
num("Qcat", cat_res$stats[["Modularity Q"]]);   num("QcatNull", cat_res$stats[["Q of rewired networks (mean)"]])
num("nCommFull", full_res$stats[["Communities (5+ articles)"]])
num("nCommCat", cat_res$stats[["Communities (5+ articles)"]])
num("nmiCatSubfield", cat_res$stats[["NMI vs subfield"]])
num("nmiCatInfomap", cat_res$stats[["NMI Louvain vs Infomap"]])
num("nCatGiant", vcount(gc_u))

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
num("medianP", median(P)); num("medianPBridges", median(cat_bridges$`Participation P`))

write_csv(tibble(article = names(memb), community = memb, community_name = comm_name[as.character(memb)]),
          rq_out("rq3_membership.csv"))

write_numbers()
message("rq3_communities.r done.")
