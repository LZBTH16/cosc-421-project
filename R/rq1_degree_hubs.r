# =============================================================================
# rq1_degree_hubs.r - RQ1: What articles are the biggest hubs in terms of degree centrality?
# Outputs: tab_rq1_*, fig_network, fig_degree, fig_top_in_degree, fig_in_vs_out, fig_hub_subgraph
# Run: open the project in RStudio and click "Source" (or run R/run_all.r).
# =============================================================================
source(here::here("R", "rq0_setup.r"))
set_rq("rq1")   # outputs go to results/rq1/, paper/figures/rq1/, paper/generated/rq1/

# =============================================================================
# RQ1: hubs by degree centrality
# =============================================================================
top_in  <- cent |> slice_max(in_degree,  n = TOP_N, with_ties = FALSE)
top_out <- cent |> slice_max(out_degree, n = TOP_N, with_ties = FALSE)

top_in |> slice_head(n = 10) |>
  transmute(`#` = row_number(), Article = short_label(article), Subfield = sub_short[subfield],
            `In` = in_degree, `In (norm.)` = in_norm, `Out` = out_degree) |>
  save_table("tab_rq1_top_in")
top_out |> slice_head(n = 10) |>
  transmute(`#` = row_number(), Article = short_label(article), Subfield = sub_short[subfield],
            `Out` = out_degree, `In` = in_degree) |>
  save_table("tab_rq1_top_out")

in_sorted <- sort(cent$in_degree, decreasing = TRUE)
num("meanDegree", m / n, 1)
num("medianIn", median(cent$in_degree)); num("medianOut", median(cent$out_degree))
num("maxIn", max(cent$in_degree));       num("maxOut", max(cent$out_degree))
num("topHub", top_in$article[1]);        num("topHubIn", top_in$in_degree[1])
num("secondHub", top_in$article[2]);     num("secondHubIn", top_in$in_degree[2])
num("shareTopTenIn", 100 * sum(in_sorted[1:10]) / m, 1)   # percent of all links
num("spearmanInOut", cor(cent$in_degree, cent$out_degree, method = "spearman"))

# Figure: whole network coloured by subfield, top 10 hubs numbered
set.seed(42)
lay     <- norm_coords(if (n > 3000) layout_with_drl(g) else layout_with_fr(g, niter = 1000))
top10   <- match(top_in$article[1:10], V(g)$name)
size_in <- 0.3 + 2.4 * sqrt(cent$in_degree / max(cent$in_degree))
sub_present <- intersect(subfield_levels, cent$subfield)
save_fig(\() draw_net(
  g, lay, size_in, adjustcolor(subfield_cols[cent$subfield], 0.75), top10, 1:10,
  key1 = list(title = "Top 10 hubs (in-degree)",
              legend = sprintf("%2d. %s (%d)", 1:10, short_label(top_in$article[1:10], 26),
                               top_in$in_degree[1:10])),
  key2 = list(title = "Subfield", legend = sub_present, col = subfield_cols[sub_present])),
  "fig_network", 7, 5)

# Figure: degree distribution (CCDF) + in-degree by subfield
cc_in  <- degree_distribution(g, mode = "in",  cumulative = TRUE)
cc_out <- degree_distribution(g, mode = "out", cumulative = TRUE)
k_in <- seq_along(cc_in) - 1; k_out <- seq_along(cc_out) - 1
save_fig(\() {
  layout(matrix(1:2, nrow = 1), widths = c(1, 1.25))
  par(mar = c(4, 4.2, 0.8, 0.8))
  plot(k_in[k_in > 0], cc_in[k_in > 0], log = "xy", pch = 16, cex = 0.5, col = "steelblue",
       xlab = "Degree k", ylab = "P(K >= k)", las = 1, cex.axis = 0.85)
  points(k_out[k_out > 0], cc_out[k_out > 0], pch = 16, cex = 0.5, col = "darkorange")
  legend("bottomleft", c("In-degree", "Out-degree"), col = c("steelblue", "darkorange"),
         pch = 16, bty = "n", cex = 0.8)
  mtext("(a)", side = 3, adj = 0, line = -0.2, font = 2)
  log_boxplot(cent$in_degree, cent$subfield, subfield_levels[1:4], subfield_cols, "In-degree")
  mtext("(b)", side = 3, adj = 0, line = -0.2, font = 2)
  layout(1)
}, "fig_degree", 7, 3)

# Figure (appendix): top 20 by in-degree, coloured by subfield
save_fig(\() hbar(top_in$in_degree, top_in$article, "In-degree",
                  col = subfield_cols[top_in$subfield],
                  key = subfield_cols[intersect(subfield_levels, top_in$subfield)]),
         "fig_top_in_degree", 7, 4.5)

# Figure (appendix): in- vs out-degree
io_ids <- match(union(top_in$article[1:10], top_out$article[1:10]), cent$article)
save_fig(\() numbered_scatter(
  cent$out_degree, cent$in_degree, io_ids,
  key_labels = sprintf("%s (%d/%d)", short_label(cent$article[io_ids], 24),
                       cent$in_degree[io_ids], cent$out_degree[io_ids]),
  xlab = "Out-degree", ylab = "In-degree", key_title = "Article (in/out)"),
  "fig_in_vs_out", 7, 4.2)

# Figure (appendix): links among the top 20 hubs
hub_sub <- induced_subgraph(g, V(g)[name %in% top_in$article])
hub_in  <- degree(g, V(hub_sub)$name, mode = "in")
save_fig(\() circle_graph(hub_sub, sprintf("%s (%d)", short_label(V(hub_sub)$name, 26), hub_in),
                          hub_in, vsize = 4 + 6 * hub_in / max(hub_in),
                          vcol = subfield_cols[V(hub_sub)$subfield]),
         "fig_hub_subgraph", 6, 6)


write_numbers()
message("rq1_degree_hubs.r done.")
