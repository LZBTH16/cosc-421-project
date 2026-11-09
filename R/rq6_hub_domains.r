# =============================================================================
# rq6_hub_domains.r - RQ6: Do the hubs cluster in particular domains?
#   * which domains the top hubs belong to, and whether some domains are
#     over-represented among the hubs (chi-squared test)
#   * whether in-degree differs between domains (Kruskal-Wallis test)
#   * whether links stay inside domains (domain assortativity vs rewired networks)
#   * reader popularity (pageviews) and growth over time by domain
# Needs the supplementary data from R/collect_page_metadata.R (domains come
# from Wikipedia categories; without them the domain labels are unreliable).
# Outputs: tab_rq6_*, fig_rq6_*, paper/generated/numbers_rq6_hub_domains.tex
# =============================================================================
source(here::here("R", "rq0_setup.r"))
set_rq("rq6")   # outputs go to results/rq6/, paper/figures/rq6/, paper/generated/rq6/

if (!has_meta) {
  message("RQ6 skipped: run R/collect_page_metadata.R first (domains need Wikipedia categories).")
} else {

HUB_N <- 50      # "hubs" = the 50 articles with the highest in-degree
dom_present <- intersect(domain_levels, cent$domain)
cent <- cent |> mutate(is_hub = rank(-in_degree, ties.method = "first") <= HUB_N)

# ---- 1. Domains of the top 20 hubs -----------------------------------------
cent |> slice_max(in_degree, n = TOP_N, with_ties = FALSE) |>
  transmute(`#` = row_number(), Article = short_label(article), Domain = domain,
            `In-degree` = in_degree) |>
  save_table("tab_rq6_top_hubs_domain")

# ---- 2. Over-representation of domains among the hubs ----------------------
# ratio > 1: the domain makes up a bigger share of the hubs than of all articles
dom_compare <- cent |>
  group_by(domain) |>
  summarise(all_n = n(), hub_n = sum(is_hub), .groups = "drop") |>
  mutate(share_all = all_n / sum(all_n), share_hubs = hub_n / sum(hub_n),
         ratio = share_hubs / share_all) |>
  arrange(desc(ratio))
dom_compare |>
  transmute(Domain = domain, Articles = all_n, Hubs = hub_n, `Share all` = share_all,
            `Share hubs` = share_hubs, Ratio = ratio) |>
  save_table("tab_rq6_domain_hubs", digits = 2)

# Are hubs distributed over domains differently from all articles?
# (simulated p-value, because some domains have very few hubs)
set.seed(2026)
chi <- chisq.test(table(cent$domain, cent$is_hub), simulate.p.value = TRUE, B = 10000)
stat("chiHubsP", "Chi-squared test, domains of hubs vs all articles (simulated p)",
     if (chi$p.value <= 1 / 10001) "< 0.001" else formatC(chi$p.value, format = "g", digits = 2))
num("hubN", HUB_N)
top_dom <- dom_compare |> filter(hub_n >= 3) |> slice_max(ratio, n = 1)
num("topHubDomain", top_dom$domain); num("topHubDomainRatio", top_dom$ratio, 2)  # same rounding as the table

# ---- 3. Does in-degree differ between domains? -----------------------------
kw <- kruskal.test(in_degree ~ factor(domain), data = cent)
stat("kwP", "Kruskal-Wallis test, in-degree by domain (p)", formatC(kw$p.value, format = "e", digits = 1))
cent |> group_by(Domain = domain) |>
  summarise(Articles = n(), `Median in` = median(in_degree), `Mean in` = mean(in_degree),
            `Max in` = max(in_degree), .groups = "drop") |>
  arrange(desc(`Median in`)) |>
  save_table("tab_rq6_indegree_domain", digits = 1)
save_fig(\() log_boxplot(cent$in_degree, cent$domain, domain_levels, domain_cols, "In-degree"),
         "fig_rq6_indegree_domain", 7, 3.6)

# ---- 4. Do links stay inside domains? (domain assortativity) ----------------
# r > 0: articles link to articles of the same domain more than chance would.
# Compared with degree-preserving rewired networks.
dom_factor <- as.integer(factor(V(g)$domain))
r_obs  <- assortativity_nominal(g, dom_factor, directed = TRUE)
r_null <- map_dbl(1:5, \(s) { set.seed(s)
  assortativity_nominal(rewire(g, keeping_degseq(niter = 10 * m)), dom_factor, directed = TRUE) })
stat("domAssort", "Domain assortativity r (observed)", r_obs)
stat("domAssortNull", "Domain assortativity r (rewired networks, mean of 5)", mean(r_null))
same_domain <- mean(V(g)$domain[head_of(g, E(g))] == V(g)$domain[tail_of(g, E(g))])
stat("sameDomainLinks", "Links between two articles of the same domain (%)", 100 * same_domain, 0)

# ---- 5. Reader popularity (pageviews over the last 12 months) ---------------
last_month <- max(pageviews$month)
views_12m <- pageviews |> filter(month > last_month %m-% months(12)) |>
  group_by(title) |> summarise(views_12m = sum(views), .groups = "drop")
cent <- cent |> left_join(views_12m, by = c("article" = "title")) |>
  mutate(views_12m = coalesce(views_12m, 0))
stat("corInViews", "Spearman rho, in-degree vs pageviews (12 months)",
     cor(cent$in_degree, cent$views_12m, method = "spearman"))
cent |> group_by(Domain = domain) |>
  summarise(Articles = n(), `Median views` = median(views_12m),
            `View share` = sum(views_12m), `Median in` = median(in_degree), .groups = "drop") |>
  mutate(`View share` = `View share` / sum(`View share`)) |>
  arrange(desc(`Median views`)) |>
  save_table("tab_rq6_popularity", digits = c(0, 0, 0, 3, 0))

top_in    <- cent |> slice_max(in_degree, n = 10, with_ties = FALSE)
top_views <- cent |> slice_max(views_12m, n = 10, with_ties = FALSE)
pv_ids <- match(union(top_in$article, top_views$article), cent$article)
save_fig(\() numbered_scatter(
  cent$in_degree, cent$views_12m, pv_ids,
  key_labels = short_label(cent$article[pv_ids], 26),
  xlab = "In-degree", ylab = "Pageviews (last 12 months)", key_title = "Article",
  point_col = adjustcolor(domain_cols[cent$domain], 0.5)),
  "fig_rq6_views", 7, 4.2)

# ---- 6. Growth over time ---------------------------------------------------
created <- page_meta |> select(title, created) |>
  inner_join(select(cent, article, domain, in_degree, is_hub), by = c("title" = "article")) |>
  filter(!is.na(created)) |> mutate(year = year(created))
stat("corYearIn", "Spearman rho, creation year vs in-degree",
     cor(created$year, created$in_degree, method = "spearman"))
stat("medianYearHubs", paste0("Median creation year, top ", HUB_N, " hubs"),
     as.character(median(created$year[created$is_hub])))
stat("medianYearAll", "Median creation year, all articles", as.character(median(created$year)))
years <- seq(min(created$year), max(created$year))
cum_mat <- created |> count(domain, year) |>
  complete(domain, year = years, fill = list(n = 0)) |> arrange(domain, year) |>
  group_by(domain) |> mutate(cum = cumsum(n)) |> ungroup() |>
  select(domain, year, cum) |> pivot_wider(names_from = domain, values_from = cum)
yearly <- pageviews |>
  inner_join(select(cent, article, domain), by = c("title" = "article")) |>
  mutate(year = year(month)) |>
  group_by(year) |> filter(n_distinct(month(month)) == 12) |> ungroup() |>
  group_by(domain, year) |> summarise(views = sum(views), .groups = "drop") |>
  group_by(domain) |> mutate(index = 100 * views / first(views)) |> ungroup()
write_csv(yearly, rq_out("rq6_views_by_domain_year.csv"))
idx_mat <- yearly |> select(domain, year, index) |>
  pivot_wider(names_from = domain, values_from = index) |> arrange(year)
save_fig(\() {
  layout(matrix(1:3, nrow = 1), widths = c(1, 1, 0.75))
  par(mar = c(4, 4.2, 0.8, 0.6)); dn <- names(cum_mat)[-1]
  matplot(cum_mat$year, as.matrix(cum_mat[, -1]), type = "l", lty = 1, lwd = 2,
          col = domain_cols[dn], las = 1, xlab = "Year created", ylab = "Articles (cumulative)")
  mtext("(a)", side = 3, adj = 0, line = -0.2, font = 2)
  dn2 <- names(idx_mat)[-1]
  matplot(idx_mat$year, as.matrix(idx_mat[, -1]), type = "l", lty = 1, lwd = 2,
          col = domain_cols[dn2], las = 1, xlab = "Year", ylab = "Pageviews (first year = 100)")
  abline(h = 100, lty = 3, col = "grey40")
  mtext("(b)", side = 3, adj = 0, line = -0.2, font = 2)
  par(mar = c(4, 0, 0.8, 0)); plot.new()
  legend("left", dn, col = domain_cols[dn], lwd = 2, bty = "n", cex = 0.75)
  layout(1)
}, "fig_rq6_growth", 7, 3.2)

# ---- 7. Domains inside the RQ3 communities -------------
memb_file <- file.path(OUT_DIR, "rq3", "rq3_membership.csv")
if (file.exists(memb_file)) {
  comm <- read_csv(memb_file, show_col_types = FALSE) |>
    left_join(select(cent, article, domain), by = "article") |>
    filter(community <= 10, community %in% which(tabulate(community) >= 5))
  comp_mat <- comm |> mutate(domain = factor(domain, levels = domain_levels)) |>
    count(community, domain, .drop = FALSE) |>
    group_by(community) |> mutate(p = n / sum(n)) |> ungroup() |> select(-n) |>
    pivot_wider(names_from = community, values_from = p) |>
    filter(if_any(-domain, \(x) x > 0)) |> arrange(domain)
  save_fig(\() {
    layout(matrix(1:2, nrow = 1), widths = c(3, 1.3))
    par(mar = c(4, 4.2, 0.8, 0.5))
    barplot(as.matrix(comp_mat[, -1]), col = domain_cols[as.character(comp_mat$domain)],
            border = "white", names.arg = paste0("C", colnames(comp_mat)[-1]), las = 1,
            ylab = "Share of articles", xlab = "Community (RQ3)")
    par(mar = c(4, 0, 0.8, 0.5)); plot.new()
    legend("left", legend = rev(as.character(comp_mat$domain)),
           fill = rev(domain_cols[as.character(comp_mat$domain)]), border = NA, bty = "n", cex = 0.7)
    layout(1)
  }, "fig_rq6_community_domains", 7, 3.4)
}

write_csv(select(cent, article, subfield, domain, in_degree, is_hub, views_12m),
          rq_out("rq6_article_domains.csv"))
save_stats()     # tab_rq6_stats: every number quoted in the RQ6 text
write_numbers()
message("rq6_hub_domains.r done.")
}
