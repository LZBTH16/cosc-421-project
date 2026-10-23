# Structural Analysis of the Computer Networking Knowledge Graph on Wikipedia

Network science course project at UBCO for COSC 421. We build a directed hyperlink network of
Wikipedia articles about computer networking and analyze it in R with igraph.

## Research questions
1. Which articles are the biggest hubs?
2. Which articles are structural bridges?
3. Does the network split into meaningful communities?
4. How densely connected is the computer networking area?
5. How robust is the network to losing its hubs?
6. Do the hubs cluster in particular domains?
7. Are central articles edited more often?

## Data
- Source: Wikipedia / MediaWiki API
- Seed article: "Computer network", crawled to depth 0 and restricted to "Category:Network protocols", "Category:Routing protocols", "Category:Wireless networking"
- Nodes: Wikipedia articles. Edges: directed hyperlinks (A links to B)
- Collected on: 2026-10-03. Wikipedia changes constantly, so the saved data in
  `data/` is the snapshot all results come from.

## Repository structure
TO-DO: Fill this in

## How to run
TO-DO: Fill this in

## Team and contributions (DRAFT)
- Nade Kang: 
- Elizabeth Yakovlev: 
- We often pair program, so one person may make the commit for work we did
  together. Shared commits are marked with `Co-authored-by`.

## AI use (DRAFT)
Claude (Anthropic) was used to brainstorm topics, identify data sources, and
help draft the proposal. See the report for the full statement.
