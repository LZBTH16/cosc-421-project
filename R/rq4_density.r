# =============================================================================
# rq4_density.r - RQ4: How densely connected is the computer networking area on Wikipedia?
# Outputs: tab_rq4_*, fig_internal_share (if total_links.csv exists)
# =============================================================================
source(here::here("R", "rq0_setup.r"))
set_rq("rq4")   # outputs go to results/rq4/, paper/figures/rq4/, paper/generated/rq4/

# =============================================================================
# RQ4: density and comparison with random networks / all of Wikipedia
# =============================================================================
comp_s <- components(g, mode = "strong")
dens <- c(
  `Articles (nodes)`                         = n,
  `Links (directed edges)`                   = m,
  `Density`                                  = edge_density(g),
  `Mean in-/out-degree`                      = m / n,
  `Reciprocity`                              = reciprocity(g),
  `Share in largest strong component`        = max(comp_s$csize) / n,
  `Global clustering (transitivity)`         = transitivity(gu, type = "global"),
  `Mean local clustering`                    = transitivity(gu, type = "average"),
  `Mean shortest path (undirected)`          = mean_distance(gu_giant, directed = FALSE),
  `Diameter (undirected)`                    = diameter(gu_giant, directed = FALSE),
  `Degree assortativity`                     = assortativity_degree(g, directed = TRUE))
tibble(Metric = names(dens), Value = dens) |>
  mutate(Value = if_else(Value >= 100, fmt_num(round(Value)), formatC(Value, format = "fg", digits = 3))) |>
  save_table("tab_rq4_density")
num("density", edge_density(g), 4); num("reciprocity", 100 * reciprocity(g), 0)
num("transitivity", transitivity(gu, type = "global"))
num("meanPath", mean_distance(gu_giant, directed = FALSE)); num("diameter", diameter(gu_giant, directed = FALSE))
num("assortativity", assortativity_degree(g, directed = TRUE))

net_stats <- function(gr) {
  gru <- to_undirected(gr, mode = "collapse"); cmp <- components(gru)
  big <- induced_subgraph(gru, which(cmp$membership == which.max(cmp$csize)))
  c(`Clustering` = transitivity(gru, type = "global"),
    `Mean shortest path` = mean_distance(big, directed = FALSE),
    `Reciprocity` = reciprocity(gr))
}
set.seed(2026)
obs   <- net_stats(g)
er    <- rowMeans(replicate(5, net_stats(sample_gnm(n, m, directed = TRUE))))
rewir <- rowMeans(replicate(5, net_stats(rewire(g, keeping_degseq(niter = 10 * m)))))
tibble(Statistic = names(obs), Observed = obs, `Random (ER)` = er, `Degree-preserving` = rewir) |>
  save_table("tab_rq4_null", digits = 3)
sigma <- unname((obs["Clustering"] / er["Clustering"]) / (obs["Mean shortest path"] / er["Mean shortest path"]))
num("sigma", sigma, 1); num("clusteringER", er["Clustering"], 3)

# Comparison with Wikipedia as a whole (needs total link counts)
total_file <- file.path(DATA_DIR, "total_links.csv")
raw_files  <- file.path(CACHE_DIR, c("core_links.rds", "extra_links.rds"))
all_links <- if (file.exists(total_file)) read_csv(total_file, show_col_types = FALSE) else
  if (all(file.exists(raw_files))) map(raw_files, readRDS) |> list_rbind() |>
    distinct(from, to) |> count(from, name = "all_links") |> rename(title = from) else NULL
has_wiki <- !is.null(all_links)
if (has_wiki) {
  link_share <- all_links |>
    inner_join(select(cent, article, out_degree), by = c("title" = "article")) |>
    filter(all_links > 0) |> mutate(internal_share = pmin(out_degree / all_links, 1))
  wiki_file <- file.path(DATA_DIR, "wiki_stats.csv")
  wiki_articles <- if (file.exists(wiki_file)) read_csv(wiki_file, show_col_types = FALSE)$articles[1] else NA
  baseline <- mean(link_share$all_links) / wiki_articles
  tibble(Statistic = c("Mean links per article (all of Wikipedia)", "Mean links within this network",
                       "Median internal link share", "English Wikipedia articles",
                       "Observed density", "Density if links were random over Wikipedia",
                       "Density ratio"),
         Value = c(fmt_num(round(mean(link_share$all_links), 1)), fmt_num(round(m / n, 1)),
                   sprintf("%.1f%%", 100 * median(link_share$internal_share)), fmt_num(wiki_articles),
                   formatC(edge_density(g), format = "g", digits = 3),
                   formatC(baseline, format = "g", digits = 3),
                   fmt_num(round(edge_density(g) / baseline)))) |>
    save_table("tab_rq4_wikipedia")
  num("internalShare", 100 * median(link_share$internal_share), 1)
  num("densityRatio", edge_density(g) / baseline, 0)
  save_fig(\() {
    par(mar = c(4, 4, 0.8, 0.8))
    hist(100 * link_share$internal_share, breaks = 30, col = "steelblue", border = "white",
         xlab = "Links that stay inside the network (%)", ylab = "Articles", main = NULL, las = 1)
    abline(v = 100 * median(link_share$internal_share), lty = 2, lwd = 2)
  }, "fig_internal_share", 6, 3.5)
}


write_numbers()
message("rq4_density.r done.")
