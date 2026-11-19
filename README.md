# Structural Analysis of the Computer Networking Hyperlink Network on Wikipedia

Network science course project at UBCO for COSC 421. We build a directed hyperlink network of
Wikipedia articles about computer networking and analyze it in R with igraph.

## Research questions
1. What articles are the biggest hubs in terms of degree centrality?
2. What articles are structural bridges in terms of the betweenness?
3. Does the network get split into meaningful communities?
4. How densely connected is the computer networking area overall on Wikipedia?
5. How robust is the network to losing its hubs?
6. Do the hubs cluster in particular domains?
7. Are central articles edited more often?

## Data
- Source: Wikipedia / MediaWiki API
- Seed article: "Computer network", crawled to depth 0 and restricted to "Category:Network protocols", "Category:Routing protocols", "Category:Wireless networking"
- Nodes: Wikipedia articles. Edges: directed hyperlinks
- Collected on: 2026-10-03. Wikipedia changes constantly, so the saved data in
  `data/` is the snapshot all results come from.

## Repository structure
```
.
├── R/          # R scripts: data collection and one script per research question
├── cache/      # cached MediaWiki API responses (so collection can be re-run quickly)
├── data/       # saved network snapshot (net_graph.rds and edge list)
├── results/    # output tables (CSV) for each research question, in results/rqN/
├── paper/      # The figures and tables that the R scripts generate for the report
├── .gitignore
└── README.md
```

## How to run
Requires R (>= 4.1, for the `|>` pipe) and the following packages:

```r
install.packages(c("tidyverse", "igraph", "wordcloud", "knitr", "here", "httr2"))
```

1. **Collect the data (optional).** Run `R/collect_wiki_network.R` to query the MediaWiki API and rebuild `data/net_graph.rds`. API responses are cached in `cache/`. Skip this step to use the snapshot already in `data/` (collected on 2026-10-03), which is what all reported results come from.
2. **Run the analyses.** Run `R/run_all.r` to reproduce every research question, or run a single script (e.g. `R/rq1_degree_hubs.r`). Outputs are written to `results/rqN/`, and figures and table files for the paper are written to `paper/`.

## Team and contributions (DRAFT)
- **Nade Kang**: Led progress on ...
- **Elizabeth Yakovlev Bell**: Led progress on ... GitHub documentation
- **Shared**: Data collection, all research questions, interpretation of results, reviewing each other's work, and the presentation.
- We often pair program, so one person may make the commit for work we did
  together. Shared commits are marked with `Co-authored-by`.

## AI use (DRAFT)
Claude (Anthropic) was used to brainstorm topics, identify data sources, and
help draft the proposal. See the report for the full statement.
