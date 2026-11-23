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

## Team contributions
Nade Kang and Elizabeth Yakovlev Bell contributed jointly to every part of this project. We pair programmed all of the code (data collection, the analysis of RQ1-RQ7, and the generation of figures and tables), and we wrote the paper and prepared the presentation together, reviewing each other's work throughout.

Because we pair programmed, one person often made the commit for work we did together. Commits from these sessions are credited to both of us with `Co-authored-by`.

## AI use Statement
AI assistance (Claude, Anthropic) was used to brainstorm the project topic and data sources, to help script the data collection and graph generation, to connect the R outputs to the LaTeX report, and to help edit the writing. The group reviewed and tested all AI-assisted code and verified that it produced the intended output. The
research questions, analysis decisions, interpretation of results and conclusions are our own.
