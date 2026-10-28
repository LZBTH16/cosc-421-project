# =============================================================================
# rq5_robustness.r - RQ5: How robust is the network to losing its hubs?
# Outputs: tab_rq5_robustness, fig_robustness, results/rq5_removal_curves.csv
# =============================================================================
source(here::here("R", "rq0_setup.r"))
set_rq("rq5")   # outputs go to results/rq5/, paper/figures/rq5/, paper/generated/rq5/

# =============================================================================
# RQ5: robustness to losing hubs (S = largest component, E = efficiency)
# =============================================================================
fracs <- seq(0, 0.5, by = 0.01)
n0 <- vcount(gu); E0 <- global_efficiency(gu)
measure <- function(gr) c(S = if (vcount(gr) == 0) 0 else max(components(gr)$csize) / n0,
                          E = if (vcount(gr) < 2) 0 else global_efficiency(gr) / E0)
static_run <- function(order_names)
  sapply(fracs, \(f) measure(delete_vertices(gu, head(order_names, round(f * n0)))))
adaptive_run <- function(score_fun) {
  gr <- gu; out <- matrix(0, 2, length(fracs), dimnames = list(c("S", "E"), NULL))
  out[, 1] <- measure(gr)
  for (i in seq_along(fracs)[-1]) {
    k <- round(fracs[i] * n0) - round(fracs[i - 1] * n0)
    gr <- delete_vertices(gr, names(sort(score_fun(gr), decreasing = TRUE))[seq_len(k)])
    out[, i] <- measure(gr)
  }
  out
}
# Betweenness is needed for the removal order
cent$betweenness <- betweenness(g, directed = TRUE)
by_score <- \(v) cent$article[order(-cent[[v]])]
cat("Running robustness simulation (about a minute)...\n")
set.seed(2026)
random_runs <- replicate(10, static_run(sample(V(gu)$name)))
runs <- list(in_degree       = static_run(by_score("in_degree")),
             total_degree    = static_run(by_score("total_degree")),
             betweenness     = static_run(by_score("betweenness")),
             adaptive_degree = adaptive_run(\(gr) setNames(degree(gr), V(gr)$name)))
if (n * m < 2e8)   # one full betweenness computation per step
  runs$adaptive_betweenness <- adaptive_run(\(gr) setNames(betweenness(gr), V(gr)$name))
curves <- bind_rows(
  imap(runs, \(r, s) tibble(strategy = s, fraction_removed = fracs, S = r["S", ], E = r["E", ])) |>
    list_rbind(),
  tibble(strategy = "random", fraction_removed = fracs,
         S = apply(random_runs["S", , ], 1, mean), E = apply(random_runs["E", , ], 1, mean),
         S_min = apply(random_runs["S", , ], 1, min), S_max = apply(random_runs["S", , ], 1, max),
         E_min = apply(random_runs["E", , ], 1, min), E_max = apply(random_runs["E", , ], 1, max)))
write_csv(curves, rq_out("rq5_removal_curves.csv"))

strat <- c(in_degree = "In-degree", total_degree = "Total degree", betweenness = "Betweenness",
           adaptive_degree = "Adaptive degree", adaptive_betweenness = "Adaptive betweenness")
strat <- strat[names(strat) %in% curves$strategy]
strat_cols <- c(in_degree = "#D55E00", total_degree = "#E69F00", betweenness = "#0072B2",
                adaptive_degree = "#CC79A7", adaptive_betweenness = "#009E73")[names(strat)]
rnd <- filter(curves, strategy == "random")
robust_panel <- function(metric, ylab, upper = FALSE, tag) {
  par(mar = c(4, 4.2, 0.8, 0.6))
  plot(NA, xlim = c(0, 50), ylim = c(0, 1.02), las = 1, xlab = "Articles removed (%)",
       ylab = ylab, cex.axis = 0.85)
  polygon(c(fracs, rev(fracs)) * 100, c(rnd[[paste0(metric, "_min")]], rev(rnd[[paste0(metric, "_max")]])),
          col = adjustcolor("grey50", 0.25), border = NA)
  lines(fracs * 100, rnd[[metric]], lwd = 2, col = "grey30")
  for (s in names(strat)) lines(fracs * 100, curves[[metric]][curves$strategy == s], lwd = 2, col = strat_cols[s])
  if (upper) lines(c(0, 50), c(1, 0.5), lty = 2, col = "grey55")
  mtext(tag, side = 3, adj = 0, line = -0.2, font = 2)
}
save_fig(\() {
  layout(matrix(1:2, nrow = 1))
  robust_panel("S", "Largest connected piece (S)", upper = TRUE, "(a)")
  legend("bottomleft", c("Upper limit", "Random", strat), col = c("grey55", "grey30", strat_cols),
         lty = c(2, rep(1, length(strat) + 1)), lwd = c(1, rep(2, length(strat) + 1)),
         bty = "n", cex = 0.7)
  robust_panel("E", "Efficiency (E, relative)", tag = "(b)")
  layout(1)
}, "fig_robustness", 7, 3.4)

at <- \(v, f) v[which.min(abs(fracs - f))]
robust_tbl <- curves |> group_by(strategy) |>
  summarise(R_S = mean(S), R_E = mean(E), E5 = at(E, 0.05), E10 = at(E, 0.10),
            S10 = at(S, 0.10), .groups = "drop") |>
  mutate(Strategy = c(random = "Random", strat)[strategy]) |>
  arrange(match(strategy, c("random", names(strat))))
robust_tbl |> transmute(Strategy, `R (S)` = R_S, `R (E)` = R_E, `E after 5%` = E5,
                        `E after 10%` = E10, `S after 10%` = S10) |>
  save_table("tab_rq5_robustness")
worst <- robust_tbl |> filter(strategy != "random") |> slice_min(R_E, n = 1)
num("RErandom", robust_tbl$R_E[robust_tbl$strategy == "random"])
num("REworst", worst$R_E); num("REworstStrategy", str_to_lower(worst$Strategy))
num("EfivePctRandom", 100 * robust_tbl$E5[robust_tbl$strategy == "random"], 0)
num("EfivePctInDegree", 100 * robust_tbl$E5[robust_tbl$strategy == "in_degree"], 0)
num("EtenPctWorst", 100 * worst$E10, 0)


write_numbers()
message("rq5_robustness.r done.")
