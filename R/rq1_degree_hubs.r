# =============================================================================
# RQ1: Which articles are the biggest hubs in terms of degree centrality?
# Input : data/net_graph.rds  (built by collect_wiki_network.R)
# Output: results/rq1_degree_centrality.csv and results/rq1_*.png
#         (every plot is also displayed on screen)
# Analysis and network plots use igraph; simple charts use base R graphics.
# =============================================================================

# install.packages(c("igraph", "dplyr", "readr", "wordcloud"))
library(igraph)
library(dplyr)
library(readr)
library(wordcloud)   # only for wordlayout(): places text labels without overlap

TOP_N   <- 20
OUT_DIR <- "results"
dir.create(OUT_DIR, showWarnings = FALSE)

# ---- Plot helpers ------------------------------------------------------------
# Draw a plot on screen, then draw it again straight into a PNG file.
# Redrawing (instead of dev.copy) lets label positions be recalculated for the
# file's size, so labels that fit on screen also fit in the saved image.
show_and_save <- function(draw, file, width = 10, height = 8) {
  draw()
  png(file.path(OUT_DIR, file), width = width, height = height,
      units = "in", res = 300)
  draw()
  invisible(dev.off())
}

# Shorten very long article titles, e.g. "Dynamic Host Configuration Pro…"
short_label <- function(x, max_chars = 32) {
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 1), "…"), x)
}

# Text with a white outline ("halo") so it stays readable on top of edges/points
halo_text <- function(x, y, labels, cex = 0.7, col = "black", halo = "white", ...) {
  dx <- 0.12 * strwidth("M", cex = cex)
  dy <- 0.12 * strheight("M", cex = cex)
  for (a in seq(0, 2 * pi, length.out = 17)[-17])
    text(x + cos(a) * dx, y + sin(a) * dy, labels, cex = cex, col = halo, ...)
  text(x, y, labels, cex = cex, col = col, ...)
}

g <- readRDS("data/net_graph.rds")
n <- vcount(g)

# ---- 1. Degree centrality with igraph ----------------------------------------
# In-degree  = number of networking articles linking TO this article
#              -> "foundational" concepts others build on
# Out-degree = number of networking articles this article links OUT to
#              -> overview / survey-style articles
# normalized = TRUE divides by (n - 1): the share of all other articles reached
V(g)$in_degree  <- degree(g, mode = "in")
V(g)$out_degree <- degree(g, mode = "out")
V(g)$total_degree <- degree(g, mode = "all")      # in + out
V(g)$in_norm    <- degree(g, mode = "in",  normalized = TRUE)
V(g)$out_norm   <- degree(g, mode = "out", normalized = TRUE)

# Collect the vertex attributes into a table for ranking and saving
# igraph:: is needed because dplyr also has a function called as_data_frame()
deg <- igraph::as_data_frame(g, what = "vertices") |>
  as_tibble() |>
  rename(article = name) |>
  mutate(in_rank    = min_rank(desc(in_degree)),
         out_rank   = min_rank(desc(out_degree)),
         total_rank = min_rank(desc(total_degree))) |>
  arrange(in_rank)

write_csv(deg, file.path(OUT_DIR, "rq1_degree_centrality.csv"))

# ---- 2. Top-N hub tables -----------------------------------------------------
top_in    <- deg |> slice_max(in_degree,    n = TOP_N, with_ties = FALSE)
top_out   <- deg |> slice_max(out_degree,   n = TOP_N, with_ties = FALSE)
top_total <- deg |> slice_max(total_degree, n = TOP_N, with_ties = FALSE)

cat("\n== Top", TOP_N, "by IN-degree (most-referenced concepts) ==\n")
print(select(top_in, in_rank, article, in_degree, in_norm, out_degree), n = TOP_N)
cat("\n== Top", TOP_N, "by OUT-degree (articles referencing the most concepts) ==\n")
print(select(top_out, out_rank, article, out_degree, out_norm, in_degree), n = TOP_N)
cat("\n== Top", TOP_N, "by TOTAL degree ==\n")
print(select(top_total, total_rank, article, total_degree, in_degree, out_degree), n = TOP_N)

# ---- 3. Summary statistics ---------------------------------------------------
m <- ecount(g)
in_sorted <- sort(V(g)$in_degree, decreasing = TRUE)
cat("\n== Degree summary ==\n")
cat("Nodes:", n, " Edges:", m, "\n")
cat("Mean in/out degree (m/n):", round(mean(V(g)$in_degree), 2), "\n")
cat("Median in-degree:", median(V(g)$in_degree),
    "  Max in-degree:", max(V(g)$in_degree), "\n")
cat("Median out-degree:", median(V(g)$out_degree),
    "  Max out-degree:", max(V(g)$out_degree), "\n")
cat("Share of all in-links received by top 10 articles:",
    round(sum(in_sorted[1:10]) / m, 3), "\n")
cat("Share received by top 1% of articles:",
    round(sum(in_sorted[1:ceiling(0.01 * n)]) / m, 3), "\n")
cat("Spearman correlation, in- vs out-degree:",
    round(cor(V(g)$in_degree, V(g)$out_degree, method = "spearman"), 3), "\n")

overlap <- intersect(top_in$article, top_out$article)
cat("\nIn BOTH top", TOP_N, "in- and out-degree lists (", length(overlap), "):\n",
    paste(overlap, collapse = "; "), "\n")


# ---- 4. Network plots with igraph --------------------------------------------
# 4a. Whole network: node size = in-degree. The top 10 hubs are drawn on top,
#     numbered 1-10, and named in a key beside the network. Writing full names
#     inside a dense network always overlaps, so a numbered key is used instead.
set.seed(42)
lay   <- norm_coords(if (n > 1000) layout_with_drl(g) else layout_with_fr(g))
top10 <- match(top_in$article[1:10], V(g)$name)        # vertex ids, rank order
size_in <- 0.4 + 3.5 * sqrt(V(g)$in_degree / max(V(g)$in_degree))

draw_network <- function() {
  layout(matrix(1:2, nrow = 1), widths = c(3, 1.25))
  par(mar = c(0, 0, 2, 0))
  plot(g, layout = lay, rescale = FALSE, xlim = c(-1, 1), ylim = c(-1, 1),
       vertex.size = size_in, vertex.color = adjustcolor("steelblue", 0.7),
       vertex.frame.color = NA, vertex.label = NA,
       edge.arrow.size = 0, edge.width = 0.4,
       edge.color = adjustcolor("grey60", alpha.f = 0.12),
       main = "Computer networking articles (node size = in-degree)")
  # Redraw the top 10 hubs on top so other nodes cannot hide them
  hubs <- induced_subgraph(g, top10)
  plot(hubs, layout = lay[top10, , drop = FALSE], rescale = FALSE, add = TRUE,
       vertex.size = pmax(size_in[top10], 4.5), vertex.color = "tomato",
       vertex.frame.color = "white",
       vertex.label = 1:10, vertex.label.color = "white",
       vertex.label.font = 2, vertex.label.cex = 0.75,
       vertex.label.family = "sans",
       edge.color = NA, edge.arrow.size = 0)
  # Key: number -> article name and in-degree
  par(mar = c(0, 0, 2, 1))
  plot.new()
  legend("left", bty = "n", cex = 0.8, y.intersp = 1.4,
         title = "Top 10 hubs (in-degree)", title.font = 2,
         legend = sprintf("%2d. %s (%d)", 1:10,
                          short_label(top_in$article[1:10], 34),
                          top_in$in_degree[1:10]))
  layout(1)
}
show_and_save(draw_network, "rq1_network_hubs.png", width = 13, height = 9)

# 4b. The top-N hubs and the links between them, arranged on a circle
#     (ordered by in-degree, clockwise from the top). Labels point outward
#     like clock numbers, so they never overlap each other or the nodes.
hub_sub <- induced_subgraph(g, V(g)[name %in% top_in$article])

draw_hub_subgraph <- function(cex = 0.75) {
  k   <- vcount(hub_sub)
  ord <- order(V(hub_sub)$in_degree, decreasing = TRUE)
  ang <- numeric(k)
  ang[ord] <- pi / 2 - 2 * pi * (seq_len(k) - 1) / k
  hlay <- cbind(cos(ang), sin(ang))
  lab  <- sprintf("%s (%d)", short_label(V(hub_sub)$name), V(hub_sub)$in_degree)

  par(mar = c(0.5, 0.5, 2.5, 0.5))
  plot.new()
  # Make the plotting window wide enough to fit the longest label
  label_in <- max(strwidth(lab, units = "inches", cex = cex))
  plot_in  <- min(par("pin"))
  L <- 1.12 / max(1 - 2 * label_in / plot_in, 0.35) + 0.05
  plot.window(xlim = c(-L, L), ylim = c(-L, L), asp = 1)
  title(main = paste("Links among the top", k, "in-degree hubs"))

  plot(hub_sub, layout = hlay, rescale = FALSE, add = TRUE,
       vertex.size = 4 + 6 * V(hub_sub)$in_degree / max(V(hub_sub)$in_degree),
       vertex.color = "tomato", vertex.frame.color = "white", vertex.label = NA,
       edge.arrow.size = 0.35, edge.arrow.width = 0.8, edge.curved = 0.15,
       edge.color = adjustcolor("grey40", alpha.f = 0.45))

  # Radial labels: text on the left half is flipped so it is never upside down
  deg   <- ang * 180 / pi
  right <- cos(ang) >= -1e-9
  for (i in seq_len(k))
    text(1.12 * cos(ang[i]), 1.12 * sin(ang[i]), lab[i], cex = cex,
         srt = if (right[i]) deg[i] else deg[i] + 180,
         adj = if (right[i]) 0 else 1)
}
show_and_save(draw_hub_subgraph, "rq1_hub_subgraph.png", width = 10, height = 10)

# ---- 5. Degree distribution with igraph --------------------------------------
# degree_distribution(cumulative = TRUE) gives P(K >= k) for k = 0, 1, 2, ...
# On log-log axes a long, roughly straight tail means a few heavily linked hubs
# and many barely linked articles (heavy-tailed distribution).
cc_in  <- degree_distribution(g, mode = "in",  cumulative = TRUE)
cc_out <- degree_distribution(g, mode = "out", cumulative = TRUE)
k_in   <- seq_along(cc_in)  - 1
k_out  <- seq_along(cc_out) - 1

draw_ccdf <- function() {
  par(mar = c(4.5, 4.5, 3, 1))
  plot(k_in[k_in > 0], cc_in[k_in > 0], log = "xy", pch = 16, cex = 0.7,
       col = "steelblue", xlab = "Degree k (log scale)",
       ylab = "P(K ≥ k) (log scale)",
       main = "Degree distribution (complementary CDF)")
  points(k_out[k_out > 0], cc_out[k_out > 0], pch = 16, cex = 0.7,
         col = "darkorange")
  legend("bottomleft", c("In-degree", "Out-degree"),
         col = c("steelblue", "darkorange"), pch = 16, bty = "n")
}
show_and_save(draw_ccdf, "rq1_degree_ccdf.png", width = 7, height = 5)

# ---- 6. Bar chart of top hubs and in- vs out-degree (base R) -----------------
# 6a. Left margin is sized to the longest article name, so no name is cut off
draw_bars <- function(cex = 0.8) {
  names_b <- rev(short_label(top_in$article, 40))
  vals    <- rev(top_in$in_degree)
  left_in <- max(strwidth(names_b, units = "inches", cex = cex)) + 0.3
  par(mai = c(0.8, left_in, 0.6, 0.4))
  bp <- barplot(vals, names.arg = names_b, horiz = TRUE, las = 1,
                cex.names = cex, col = "steelblue", border = NA,
                xlim = c(0, max(vals) * 1.12), xlab = "In-degree",
                main = paste("Top", TOP_N, "articles by in-degree"))
  text(vals, bp, vals, pos = 4, cex = cex * 0.9, xpd = TRUE)   # value labels
}
show_and_save(draw_bars, "rq1_top_in_degree.png", width = 9, height = 7)

# 6b. In- vs out-degree. The labelled articles (top 10 by in-degree and top 10
#     by out-degree) get a short number next to their point and are named in a
#     key on the right; full names would pile up where the points cluster.
#     Points are plotted on log10(degree + 1); wordlayout() keeps the numbers
#     apart and a thin line links each moved number back to its point.
lab_ids <- match(union(top_in$article[1:10], top_out$article[1:10]), V(g)$name)

draw_scatter <- function(cex = 0.7) {
  x <- log10(V(g)$out_degree + 1)
  y <- log10(V(g)$in_degree + 1)
  layout(matrix(1:2, nrow = 1), widths = c(3, 1.3))
  par(mar = c(4.5, 4.5, 3, 0.5))
  plot(x, y, pch = 16, cex = 0.6, col = adjustcolor("grey30", alpha.f = 0.35),
       axes = FALSE, xlab = "Out-degree (log scale)", ylab = "In-degree (log scale)",
       xlim = range(x) + c(-0.03, 0.08) * diff(range(x)),
       ylim = range(y) + c(-0.03, 0.12) * diff(range(y)),
       main = "In-degree vs out-degree")
  ticks <- c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000)
  axis(1, at = log10(ticks + 1), labels = ticks)
  axis(2, at = log10(ticks + 1), labels = ticks, las = 1)
  box()
  points(x[lab_ids], y[lab_ids], pch = 16, cex = 0.9, col = "tomato")

  nums <- as.character(seq_along(lab_ids))
  # Start each number just above-right of its point so it doesn't cover it
  x0 <- x[lab_ids] + 0.7 * strwidth("00", cex = cex)
  y0 <- y[lab_ids] + 0.9 * strheight("0", cex = cex)
  wl <- wordlayout(x0, y0, paste0(" ", nums, " "),
                   cex = cex * 1.4,
                   xlim = par("usr")[1:2], ylim = par("usr")[3:4])
  lx <- wl[, 1] + wl[, 3] / 2
  ly <- wl[, 2] + wl[, 4] / 2
  segments(x[lab_ids], y[lab_ids], lx, ly, col = "grey50", lwd = 0.6)
  halo_text(lx, ly, nums, cex = cex, font = 2, col = "firebrick")
  points(x[lab_ids], y[lab_ids], pch = 16, cex = 0.9, col = "tomato")  # keep points on top

  # Key: number -> article name, with its in- and out-degree
  par(mar = c(4.5, 0, 3, 1))
  plot.new()
  legend("left", bty = "n", cex = 0.72, y.intersp = 1.25,
         title = "Labelled articles (in / out)", title.font = 2,
         legend = sprintf("%2d. %s (%d / %d)", seq_along(lab_ids),
                          short_label(V(g)$name[lab_ids], 30),
                          V(g)$in_degree[lab_ids], V(g)$out_degree[lab_ids]))
  layout(1)
}
show_and_save(draw_scatter, "rq1_in_vs_out.png", width = 12, height = 7)

cat("\nSaved ranking table and plots to", OUT_DIR, "/\n")
