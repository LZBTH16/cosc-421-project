# =============================================================================
# rq2_structural_betweenness.r - RQ2: What articles are structural bridges in terms of betweenness?
# Outputs: tab_rq2_*, fig_betweenness, results/all_centrality_scores.csv
# Run: open the project in RStudio and click "Source" (or run R/run_all.r).
# =============================================================================
source(here::here("R", "rq0_setup.r"))
set_rq("rq2")   # outputs go to results/rq2/, paper/figures/rq2/, paper/generated/rq2/

# =============================================================================
# RQ2: bridges by betweenness; comparison of centrality measures
# =============================================================================
cent <- cent |> mutate(
  betweenness      = betweenness(g, directed = TRUE),
  betweenness_norm = betweenness(g, directed = TRUE, normalized = TRUE),
  eigenvector      = eigen_centrality(g, directed = TRUE)$vector,
  pagerank         = page_rank(g, directed = TRUE)$vector)
clo <- closeness(gu_giant, normalized = TRUE)   # undirected largest component
cent$closeness <- unname(clo[cent$article])
in_rank <- \(a) match(a, arrange(cent, desc(in_degree))$article)

top_btw <- cent |> slice_max(betweenness, n = TOP_N, with_ties = FALSE)
top_btw |> slice_head(n = 10) |>
  transmute(`#` = row_number(), Article = short_label(article), Subfield = sub_short[subfield],
            `Betw. (norm.)` = betweenness_norm, `In-degree rank` = in_rank(article)) |>
  save_table("tab_rq2_top_betweenness")
num("topBridge", top_btw$article[1]); num("topBridgeBtw", top_btw$betweenness_norm[1], 3)

# Bridges beyond what degree predicts: largest positive residuals of
# log(betweenness) ~ log(total degree), among articles with >= 10 links
BRIDGE_MIN_DEGREE <- 10
fit <- lm(log10(betweenness + 1) ~ log10(total_degree + 1), data = filter(cent, betweenness > 0))
cent <- cent |> mutate(bridge_residual = if_else(
  betweenness > 0 & total_degree >= BRIDGE_MIN_DEGREE,
  log10(betweenness + 1) - predict(fit, newdata = cent), NA_real_))
bridges <- cent |> slice_max(bridge_residual, n = 10, with_ties = FALSE)
bridges |>
  transmute(Article = short_label(article), Subfield = sub_short[subfield], Degree = total_degree,
            `Betw. (norm.)` = betweenness_norm, `x expected` = 10^bridge_residual) |>
  save_table("tab_rq2_bridges", digits = c(0, 0, 0, 4, 1))
num("topBeyondBridge", bridges$article[1]); num("topBeyondBridgeX", 10^bridges$bridge_residual[1], 0)

bt_ids <- match(union(top_btw$article[1:10], bridges$article), cent$article)
save_fig(\() numbered_scatter(
  cent$total_degree, cent$betweenness, bt_ids,
  key_labels = sprintf("%s%s", short_label(cent$article[bt_ids], 26),
                       ifelse(cent$article[bt_ids] %in% bridges$article, " *", "")),
  xlab = "Total degree", ylab = "Betweenness", key_title = "Article (* = bridge)"),
  "fig_betweenness", 7, 4.8)

measures <- c(`In-degree` = "in_degree", `Out-degree` = "out_degree", Betweenness = "betweenness",
              Eigenvector = "eigenvector", Closeness = "closeness", PageRank = "pagerank")
map(measures, \(v) cent |> arrange(desc(.data[[v]])) |> slice_head(n = 10) |>
      pull(article) |> short_label(22)) |>
  as_tibble() |> mutate(`#` = row_number(), .before = 1) |>
  save_table("tab_rq2_top10_by_measure")
cor_mat <- cent |> select(all_of(unname(measures))) |>
  cor(method = "spearman", use = "pairwise.complete.obs")
short_measure <- c("In", "Out", "Betw.", "Eigen.", "Close.", "PageRank")
dimnames(cor_mat) <- list(names(measures), short_measure)
as.data.frame(cor_mat) |> rownames_to_column(" ") |> save_table("tab_rq2_correlation", digits = 2)
num("corInBtw", cor_mat["In-degree", "Betw."]); num("corInPR", cor_mat["In-degree", "PageRank"])

write_csv(cent, rq_out("all_centrality_scores.csv"))

write_numbers()
message("rq2_structural_betweenness.r done.")
