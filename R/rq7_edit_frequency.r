# =============================================================================
# rq7_edit_frequency.r - RQ7: Are central articles edited more often?
#   * Spearman correlation between edits in the last 12 months and each
#     centrality measure (and with pageviews and page length)
#   * hubs vs all other articles (Wilcoxon rank-sum test)
#   * regression: does centrality still predict edits once popularity
#     (pageviews) and page size are taken into account?
#   * edits by in-degree quintile, by domain, and per month
# Needs the supplementary data from R/collect_page_metadata.R.
# Outputs: tab_rq7_*, fig_rq7_*, paper/generated/numbers_rq7_edit_frequency.tex
# Run: open the project in RStudio and click "Source" (or run R/run_all.r).
# =============================================================================
source(here::here("R", "rq0_setup.r"))
set_rq("rq7")   # outputs go to results/rq7/, paper/figures/rq7/, paper/generated/rq7/

if (!has_meta) {
  message("RQ7 skipped: run R/collect_page_metadata.R first (edit counts come from Wikipedia).")
} else {

HUB_N <- 50

# ---- Data: centrality + edits + pageviews + page length ---------------------
last_month <- max(pageviews$month)
views_12m <- pageviews |> filter(month > last_month %m-% months(12)) |>
  group_by(title) |> summarise(views_12m = sum(views), .groups = "drop")
cent <- cent |>
  mutate(betweenness = betweenness(g, directed = TRUE),
         pagerank    = page_rank(g, directed = TRUE)$vector,
         is_hub      = rank(-in_degree, ties.method = "first") <= HUB_N) |>
  left_join(views_12m, by = c("article" = "title")) |>
  left_join(select(page_meta, title, edits_12m, length), by = c("article" = "title")) |>
  mutate(views_12m = coalesce(views_12m, 0), edits_12m = coalesce(as.numeric(edits_12m), 0))

stat("medianEdits", "Median edits per article (12 months)", median(cent$edits_12m))
stat("meanEdits", "Mean edits per article (12 months)", mean(cent$edits_12m), 1)
stat("shareNotEdited", "Articles not edited at all (%)", 100 * mean(cent$edits_12m == 0), 0)

# ---- 1. Correlations --------------------------------------------------------
cor_with_edits <- function(v) {
  ct <- suppressWarnings(cor.test(cent$edits_12m, cent[[v]], method = "spearman", exact = FALSE))
  c(rho = unname(ct$estimate), p = ct$p.value)
}
vars <- c(`In-degree` = "in_degree", `Out-degree` = "out_degree", Betweenness = "betweenness",
          PageRank = "pagerank", `Pageviews (12 mo)` = "views_12m", `Page length` = "length")
cor_tbl <- map(vars, cor_with_edits) |> bind_rows(.id = "Measure") |>
  transmute(Measure, `Spearman rho` = rho, `p-value` = formatC(p, format = "e", digits = 1))
save_table(cor_tbl, "tab_rq7_correlations", digits = 2)
num("corEditsIn", cor_with_edits("in_degree")[["rho"]])
num("corEditsBtw", cor_with_edits("betweenness")[["rho"]])
num("corEditsViews", cor_with_edits("views_12m")[["rho"]])
num("corEditsLength", cor_with_edits("length")[["rho"]])

# ---- 2. Hubs vs other articles ----------------------------------------------
wt <- wilcox.test(edits_12m ~ is_hub, data = cent, exact = FALSE)
stat("medianEditsHubs", paste0("Median edits, top ", HUB_N, " hubs by in-degree"), median(cent$edits_12m[cent$is_hub]))
stat("medianEditsOthers", "Median edits, all other articles", median(cent$edits_12m[!cent$is_hub]))
stat("wilcoxP", "Wilcoxon test, hubs vs other articles (p)", formatC(wt$p.value, format = "e", digits = 1))
num("hubNEdits", HUB_N)

# ---- 3. Regression: centrality vs edits, controlling for views and size -----
# log(edits + 1) ~ log(in-degree + 1) + log(betweenness + 1) + log(views + 1) + log(length)
# Coefficients are elasticities: a 1% higher in-degree goes with about b% more edits.
model_data <- cent |> filter(!is.na(length), length > 0)
fit <- lm(log1p(edits_12m) ~ log1p(in_degree) + log1p(betweenness) + log1p(views_12m) + log(length),
          data = model_data)
coefs <- summary(fit)$coefficients
tibble(Term = c("Intercept", "log(in-degree + 1)", "log(betweenness + 1)",
                "log(pageviews + 1)", "log(page length)"),
       Estimate = coefs[, 1], `Std. error` = coefs[, 2],
       `p-value` = formatC(coefs[, 4], format = "e", digits = 1)) |>
  save_table("tab_rq7_model", digits = 3)
num("betaIn", coefs["log1p(in_degree)", 1], 3)
num("betaInP", formatC(coefs["log1p(in_degree)", 4], format = "e", digits = 1))
num("betaViews", coefs["log1p(views_12m)", 1], 3)
stat("modelRsq", "Regression R-squared", summary(fit)$r.squared)
stat("modelN", "Regression: number of articles", nrow(model_data))

# ---- 4. Most edited articles --------------------------------------------------
cent |> slice_max(edits_12m, n = 10, with_ties = FALSE) |>
  transmute(Article = short_label(article), Domain = domain, `Edits (12 mo)` = edits_12m,
            `In-degree` = in_degree, `In-degree rank` = rank(-cent$in_degree, ties.method = "min")[match(article, cent$article)]) |>
  save_table("tab_rq7_top_edited")

# ---- 5. Figures ---------------------------------------------------------------
# (a) edits by in-degree quintile, (b) edits per month
cent <- cent |> mutate(quintile = ntile(in_degree, 5))
q_lab <- cent |> group_by(quintile) |>
  summarise(lab = sprintf("Q%d\n(%d-%d)", first(quintile), min(in_degree), max(in_degree)), .groups = "drop")
monthly <- edits_12m |> mutate(month = floor_date(timestamp, "month")) |> count(month) |>
  filter(month > min(month), month < max(month))     # drop the two partial months
save_fig(\() {
  layout(matrix(1:2, nrow = 1), widths = c(1, 1))
  par(mar = c(4.5, 4.2, 0.8, 0.6))
  boxplot(log10(edits_12m + 1) ~ quintile, data = cent, col = "steelblue", yaxt = "n", xaxt = "n",
          xlab = "", ylab = "Edits in last 12 months", outcex = 0.4)
  axis(1, at = seq_len(nrow(q_lab)), labels = q_lab$lab, cex.axis = 0.7, padj = 0.5, gap.axis = -1)
  title(xlab = "In-degree quintile (range)", line = 3.3)
  t <- log_ticks(cent$edits_12m); axis(2, at = log10(t + 1), labels = fmt_num(t), las = 1, cex.axis = 0.8)
  mtext("(a)", side = 3, adj = 0, line = -0.2, font = 2)
  par(mar = c(4.5, 4.2, 0.8, 0.6))
  barplot(monthly$n, names.arg = format(monthly$month, "%b\n%y"), col = "grey50", border = NA,
          las = 1, cex.names = 0.7, ylab = "Edits (all articles)")
  mtext("(b)", side = 3, adj = 0, line = -0.2, font = 2)
  layout(1)
}, "fig_rq7_edits", 7, 3.2)

# Edits against in-degree, top hubs and most edited articles numbered
top_e <- cent |> slice_max(edits_12m, n = 10, with_ties = FALSE)
top_h <- cent |> slice_max(in_degree, n = 10, with_ties = FALSE)
e_ids <- match(union(top_h$article, top_e$article), cent$article)
save_fig(\() numbered_scatter(
  cent$in_degree, cent$edits_12m, e_ids,
  key_labels = short_label(cent$article[e_ids], 26),
  xlab = "In-degree", ylab = "Edits in last 12 months", key_title = "Article",
  point_col = adjustcolor(domain_cols[cent$domain], 0.5)),
  "fig_rq7_edits_vs_indegree", 7, 4.2)

save_fig(\() log_boxplot(cent$edits_12m, cent$domain, domain_levels, domain_cols,
                         "Edits in the last 12 months"), "fig_rq7_edits_domain", 7, 3.6)

write_csv(select(cent, article, subfield, domain, in_degree, betweenness, pagerank,
                 views_12m, length, edits_12m), rq_out("rq7_edits_centrality.csv"))
save_stats()     # tab_rq7_stats: every number quoted in the RQ7 text
write_numbers()
message("rq7_edit_frequency.r done.")
}
