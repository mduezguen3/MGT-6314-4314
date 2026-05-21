# =============================================================================
# MGT 6314/4314 - Understanding Markets with Data Science
# Project: How Online Reviews Influence Product Demand on Amazon
# Team: Mert Duezguen, Hannah Gordy, Tiffany Yie
#
# Script 03: Reviewer Network Analysis & Influence Scoring
# =============================================================================

# --- 1. Packages -------------------------------------------------------------

packages <- c(
  "dplyr", "tidyr", "purrr", "igraph", "ggplot2", "scales", "Matrix"
)
invisible(lapply(packages, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))


# --- 2. Load Data ------------------------------------------------------------

BASE_DIR <- "C:/Users/Mert/OneDrive - Georgia Institute of Technology/Desktop/Amazon Datasets/processed"

reviews <- readRDS(file.path(BASE_DIR, "reviews_clean.rds"))
master  <- readRDS(file.path(BASE_DIR, "master_product.rds"))

reviews_filtered <- reviews %>%
  filter(product_id %in% master$product_id)

message("Reviews used for network: ", nrow(reviews_filtered))


# --- 3. Build Reviewer-Product Edge List ------------------------------------

edges_rev_prod <- reviews_filtered %>%
  select(reviewer_id, product_id) %>%
  distinct()

message("Unique (reviewer, product) edges: ", nrow(edges_rev_prod))


# --- 4. Project to Reviewer-Reviewer Network --------------------------------

reviewer_ids <- unique(edges_rev_prod$reviewer_id)
product_ids  <- unique(edges_rev_prod$product_id)

rev_idx  <- setNames(seq_along(reviewer_ids), reviewer_ids)
prod_idx <- setNames(seq_along(product_ids),  product_ids)

row_i <- rev_idx[edges_rev_prod$reviewer_id]
col_j <- prod_idx[edges_rev_prod$product_id]

incidence <- sparseMatrix(
  i = row_i, j = col_j,
  x = 1,
  dims = c(length(reviewer_ids), length(product_ids)),
  dimnames = list(reviewer_ids, product_ids)
)

message("Computing co-review projection (may take a moment)...")
co_review <- incidence %*% t(incidence)
co_review <- co_review - Diagonal(nrow(co_review))

CO_THRESHOLD <- 2
co_summary   <- summary(co_review)
co_edges     <- co_summary[co_summary$x >= CO_THRESHOLD, ]

message("Reviewer-reviewer edges (threshold >= ", CO_THRESHOLD, "): ", nrow(co_edges))


# --- 5. Build igraph Network -------------------------------------------------

g <- graph_from_edgelist(
  as.matrix(co_edges[, c("i", "j")]),
  directed = FALSE
)

E(g)$weight    <- co_edges$x
V(g)$reviewer_id <- reviewer_ids[as.integer(V(g))]

message("Network: ", vcount(g), " reviewers, ", ecount(g), " edges")


# --- 6. Compute Centrality Measures ------------------------------------------

message("Computing centrality metrics...")

degree_c   <- degree(g,    mode = "all", normalized = TRUE)
strength_c <- strength(g,  mode = "all", weights = E(g)$weight)
pagerank_c <- page_rank(g, weights = E(g)$weight)$vector

reviewer_influence <- tibble(
  reviewer_id       = V(g)$reviewer_id,
  degree_centrality = degree_c,
  strength          = strength_c,
  pagerank          = pagerank_c
) %>%
  mutate(
    degree_norm     = scales::rescale(degree_centrality),
    strength_norm   = scales::rescale(strength),
    pagerank_norm   = scales::rescale(pagerank),
    influence_score = (degree_norm + strength_norm + pagerank_norm) / 3
  )

message("Influence scores computed for ", nrow(reviewer_influence), " reviewers")


# --- 7. Aggregate Influence to Product Level ---------------------------------

product_influence <- reviews_filtered %>%
  select(reviewer_id, product_id) %>%
  left_join(reviewer_influence %>% select(reviewer_id, influence_score,
                                          pagerank, degree_centrality),
            by = "reviewer_id") %>%
  group_by(product_id) %>%
  summarise(
    avg_reviewer_influence = mean(influence_score,   na.rm = TRUE),
    max_reviewer_influence = max(influence_score,    na.rm = TRUE),
    avg_pagerank           = mean(pagerank,           na.rm = TRUE),
    avg_degree_centrality  = mean(degree_centrality, na.rm = TRUE),
    pct_influential        = mean(influence_score > quantile(
      reviewer_influence$influence_score, 0.75,
      na.rm = TRUE), na.rm = TRUE),
    .groups = "drop"
  )

message("Products with influence scores: ", nrow(product_influence))


# --- 8. Network Descriptive Stats --------------------------------------------

message("\n--- Network Summary ---")
message("Nodes (reviewers):       ", vcount(g))
message("Edges:                   ", ecount(g))
message("Average degree:          ", round(mean(degree(g)), 3))
message("Network density:         ", round(graph.density(g), 6))
message("Number of components:    ", components(g)$no)

largest_comp <- induced_subgraph(g, which(components(g)$membership ==
                                            which.max(components(g)$csize)))
message("Largest component nodes: ", vcount(largest_comp))
message("Clustering coefficient:  ",
        round(transitivity(largest_comp, type = "average"), 4))


# --- 9. Visualisations -------------------------------------------------------

fig_dir <- file.path(BASE_DIR, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir)

clean_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey92")
  )

# p1: degree distribution
deg_dist <- tibble(degree = degree(g)) %>%
  count(degree) %>%
  filter(degree > 0)

p1 <- ggplot(deg_dist, aes(x = degree, y = n)) +
  geom_point(alpha = 0.6, size = 2, color = "#2166ac") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_smooth(method = "lm", se = FALSE, color = "#d6604d", linewidth = 1) +
  labs(title    = "Reviewer Network: Degree Distribution (log-log)",
       subtitle = "Linear trend on log-log axes confirms a power-law network",
       x = "Degree (log scale)", y = "Count (log scale)") +
  clean_theme

ggsave(file.path(fig_dir, "network_degree_dist.png"), p1, width = 8, height = 5, dpi = 150)

# p2: PageRank as density plot with color gradient by influence tier
reviewer_influence <- reviewer_influence %>%
  mutate(influence_tier = case_when(
    pagerank_norm >= 0.75 ~ "Top 25%",
    pagerank_norm >= 0.50 ~ "50-75%",
    pagerank_norm >= 0.25 ~ "25-50%",
    TRUE                  ~ "Bottom 25%"
  ))

p2 <- ggplot(reviewer_influence, aes(x = pagerank, fill = influence_tier)) +
  geom_histogram(bins = 60, color = "white", linewidth = 0.2,
                 position = "stack") +
  scale_x_log10(labels = scales::scientific) +
  scale_fill_manual(values = c(
    "Top 25%"    = "#d73027",
    "50-75%"     = "#fc8d59",
    "25-50%"     = "#91bfdb",
    "Bottom 25%" = "#4575b4"
  ), name = "Influence Tier") +
  labs(title    = "Distribution of Reviewer PageRank Scores by Influence Tier",
       subtitle = "Most reviewers have low PageRank; a small elite drives network influence",
       x = "PageRank (log scale)", y = "Count") +
  clean_theme

ggsave(file.path(fig_dir, "reviewer_pagerank_dist.png"), p2, width = 9, height = 5, dpi = 150)

# p3: top 20 reviewers colored by influence score intensity
top20 <- reviewer_influence %>%
  slice_max(influence_score, n = 20) %>%
  arrange(desc(influence_score))

p3 <- ggplot(top20, aes(x = reorder(reviewer_id, influence_score),
                        y = influence_score,
                        fill = influence_score)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "#91bfdb", high = "#d73027",
                      name = "Influence\nScore") +
  labs(title    = "Top 20 Most Influential Reviewers",
       subtitle = "Ranked by composite score of degree, strength, and PageRank",
       x = "Reviewer ID", y = "Composite Influence Score [0,1]") +
  clean_theme

ggsave(file.path(fig_dir, "top_reviewers.png"), p3, width = 9, height = 6, dpi = 150)

message("Network figures saved to: ", fig_dir)


# Summary
# --- Network Summary Output --------------------------------------------------

message("\n===== NETWORK ANALYSIS SUMMARY =====")
message("Nodes (reviewers):       ", vcount(g))
message("Edges:                   ", ecount(g))
message("Average degree:          ", round(mean(degree(g)), 3))
message("Network density:         ", round(graph.density(g), 6))
message("Number of components:    ", components(g)$no)
message("Largest component nodes: ", vcount(largest_comp))
message("Clustering coefficient:  ", round(transitivity(largest_comp, type = "average"), 4))

message("\n--- Influence Score Summary ---")
print(summary(reviewer_influence[, c("degree_centrality", "pagerank", "influence_score")]))

message("\n--- Top 5 Most Influential Reviewers ---")
print(reviewer_influence %>%
        slice_max(influence_score, n = 5) %>%
        select(reviewer_id, degree_centrality, pagerank, influence_score) %>%
        arrange(desc(influence_score)))

message("\n--- Product Influence Summary ---")
print(summary(product_influence[, c("avg_reviewer_influence", "avg_pagerank", "pct_influential")]))

message("\n--- Products by Influential Reviewer Percentage ---")
print(product_influence %>%
        mutate(tier = case_when(
          pct_influential >= 0.75 ~ "75-100% influential reviewers",
          pct_influential >= 0.50 ~ "50-75% influential reviewers",
          pct_influential >= 0.25 ~ "25-50% influential reviewers",
          TRUE                    ~ "0-25% influential reviewers"
        )) %>%
        count(tier) %>%
        arrange(desc(n)))

# --- 10. Save ----------------------------------------------------------------

saveRDS(reviewer_influence, file.path(BASE_DIR, "reviewer_influence.rds"))
saveRDS(product_influence,  file.path(BASE_DIR, "product_influence.rds"))
write.csv(product_influence, file.path(BASE_DIR, "product_influence.csv"), row.names = FALSE)

message("=== Network analysis complete ===")