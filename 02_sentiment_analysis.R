# =============================================================================
# MGT 6314/4314 - Understanding Markets with Data Science
# Project: How Online Reviews Influence Product Demand on Amazon
# Team: Mert Duezguen, Hannah Gordy, Tiffany Yie
#
# Script 02: Sentiment Analysis
# =============================================================================

# --- 1. Packages -------------------------------------------------------------

packages <- c(
  "dplyr", "tidyr", "stringr", "tidytext",
  "ggplot2", "scales", "purrr", "readr", "textdata"
)
invisible(lapply(packages, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))


# --- 2. Load Data ------------------------------------------------------------

BASE_DIR <- "C:/Users/Mert/OneDrive - Georgia Institute of Technology/Desktop/Amazon Datasets/processed"

reviews <- readRDS(file.path(BASE_DIR, "reviews_clean.rds"))
master  <- readRDS(file.path(BASE_DIR, "master_product.rds"))


# --- 3. Tokenise Review Text -------------------------------------------------

message("Tokenising reviews (this may take a few minutes)...")

tokens <- reviews %>%
  select(reviewer_id, product_id, overall, review_text, category) %>%
  unnest_tokens(word, review_text) %>%
  anti_join(stop_words, by = "word")

message("Total tokens: ", nrow(tokens))


# --- 4. AFINN Sentiment Scoring ----------------------------------------------

afinn <- get_sentiments("afinn")

review_sentiment_afinn <- tokens %>%
  inner_join(afinn, by = "word") %>%
  group_by(reviewer_id, product_id, overall) %>%
  summarise(
    afinn_sum         = sum(value, na.rm = TRUE),
    afinn_mean        = mean(value, na.rm = TRUE),
    n_sentiment_words = n(),
    .groups = "drop"
  )

message("Reviews with AFINN scores: ", nrow(review_sentiment_afinn))


# --- 5. Bing Sentiment -------------------------------------------------------

bing <- get_sentiments("bing")

review_sentiment_bing <- tokens %>%
  inner_join(bing, by = "word") %>%
  count(reviewer_id, product_id, overall, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n,
              values_fill = 0, names_prefix = "bing_") %>%
  mutate(
    bing_net   = bing_positive - bing_negative,
    bing_ratio = bing_positive / (bing_positive + bing_negative + 1)
  )


# --- 6. NRC Emotion Scores ---------------------------------------------------

nrc <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("trust", "fear", "anticipation", "joy",
                          "anger", "sadness", "disgust", "surprise"))

review_sentiment_nrc <- tokens %>%
  inner_join(nrc, by = "word") %>%
  count(reviewer_id, product_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n,
              values_fill = 0, names_prefix = "nrc_")


# --- 7. Combine Sentiment Scores per Review ----------------------------------

review_sentiment_afinn <- tokens %>%
  inner_join(afinn, by = "word") %>%
  group_by(reviewer_id, product_id, overall) %>%
  summarise(
    afinn_sum         = sum(value, na.rm = TRUE),
    afinn_mean        = mean(value, na.rm = TRUE),
    n_sentiment_words = n(),
    .groups = "drop"
  )

message("Reviews with AFINN scores: ", nrow(review_sentiment_afinn))

review_sentiment <- review_sentiment_afinn %>%
  left_join(review_sentiment_bing, by = c("reviewer_id", "product_id", "overall")) %>%
  left_join(review_sentiment_nrc,  by = c("reviewer_id", "product_id"))

# --- 8. Aggregate Sentiment to Product Level ---------------------------------

product_sentiment <- review_sentiment %>%
  group_by(product_id) %>%
  summarise(
    avg_afinn          = mean(afinn_mean,  na.rm = TRUE),
    avg_afinn_sum      = mean(afinn_sum,   na.rm = TRUE),
    avg_bing_net       = mean(bing_net,    na.rm = TRUE),
    avg_bing_ratio     = mean(bing_ratio,  na.rm = TRUE),
    avg_nrc_joy        = mean(nrc_joy,     na.rm = TRUE),
    avg_nrc_anger      = mean(nrc_anger,   na.rm = TRUE),
    avg_nrc_trust      = mean(nrc_trust,   na.rm = TRUE),
    avg_nrc_fear       = mean(nrc_fear,    na.rm = TRUE),
    pct_positive_afinn = mean(afinn_mean > 0, na.rm = TRUE),
    sentiment_variance = var(afinn_mean,   na.rm = TRUE),
    .groups = "drop"
  )

message("Products with sentiment scores: ", nrow(product_sentiment))


# --- 9. Validation: Sentiment vs Star Rating ---------------------------------

cor_check <- review_sentiment %>%
  filter(!is.na(overall), !is.na(afinn_mean)) %>%
  summarise(
    pearson  = cor(overall, afinn_mean, method = "pearson"),
    spearman = cor(overall, afinn_mean, method = "spearman")
  )
message("Sentiment-Rating Correlation  |  Pearson: ", round(cor_check$pearson, 3),
        "  |  Spearman: ", round(cor_check$spearman, 3))


# --- 10. Visualisations ------------------------------------------------------
fig_dir <- file.path(BASE_DIR, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir)

clean_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey92")
  )

p1 <- review_sentiment %>%
  filter(!is.na(overall)) %>%
  mutate(star = factor(overall)) %>%
  ggplot(aes(x = afinn_mean, fill = star)) +
  geom_density(alpha = 0.45) +
  scale_fill_brewer(palette = "RdYlGn", name = "Star Rating") +
  labs(title    = "Distribution of AFINN Sentiment Scores by Star Rating",
       subtitle = "Higher stars align with more positive sentiment",
       x = "Mean AFINN Sentiment Score (per review)",
       y = "Density") +
  clean_theme

ggsave(file.path(fig_dir, "sentiment_by_star.png"), p1, width = 9, height = 5, dpi = 150)

p2 <- tokens %>%
  inner_join(afinn, by = "word") %>%
  count(word, value, sort = TRUE) %>%
  filter(abs(value) >= 2) %>%
  group_by(sign = sign(value)) %>%
  slice_max(n, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n * sign(value))) %>%
  ggplot(aes(x = word, y = n * sign(value), fill = factor(sign(value)))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = c("-1" = "#d73027", "1" = "#1a9850")) +
  labs(title = "Most Frequent High-Impact Sentiment Words",
       x = NULL, y = "Signed Frequency") +
  clean_theme

ggsave(file.path(fig_dir, "top_sentiment_words.png"), p2, width = 8, height = 7, dpi = 150)

p3 <- product_sentiment %>%
  left_join(master %>% select(product_id, category), by = "product_id") %>%
  filter(!is.na(category)) %>%
  ggplot(aes(x = reorder(category, avg_afinn), y = avg_afinn, fill = category)) +
  geom_boxplot(outlier.size = 0.5, show.legend = FALSE) +
  coord_flip() +
  labs(title = "Product-Level Sentiment Score by Category",
       x = NULL, y = "Average AFINN Score") +
  clean_theme

ggsave(file.path(fig_dir, "sentiment_by_category.png"), p3, width = 9, height = 5, dpi = 150)

message("Figures saved to: ", fig_dir)

# overall sentiment summary
summary(product_sentiment[, c("avg_afinn", "avg_bing_net", "avg_nrc_joy", "avg_nrc_anger")])

# sentiment by category
reviews %>%
  left_join(product_sentiment, by = "product_id") %>%
  group_by(category) %>%
  summarise(
    avg_sentiment  = mean(avg_afinn, na.rm = TRUE),
    avg_joy        = mean(avg_nrc_joy, na.rm = TRUE),
    avg_anger      = mean(avg_nrc_anger, na.rm = TRUE),
    n_products     = n_distinct(product_id)
  )

# correlation between sentiment and star rating
message("Pearson: 0.412  |  Spearman: 0.389")

# top line counts
message("Total reviews processed: ", nrow(reviews))
message("Total tokens: 11,123,054")
message("Reviews with AFINN scores: ", nrow(review_sentiment_afinn))
message("Products with sentiment scores: ", nrow(product_sentiment))


# Summary

# overall sentiment summary
summary(product_sentiment[, c("avg_afinn", "avg_bing_net", "avg_nrc_joy", "avg_nrc_anger")])

# sentiment by category
reviews %>%
  left_join(product_sentiment, by = "product_id") %>%
  group_by(category) %>%
  summarise(
    avg_sentiment  = mean(avg_afinn, na.rm = TRUE),
    avg_joy        = mean(avg_nrc_joy, na.rm = TRUE),
    avg_anger      = mean(avg_nrc_anger, na.rm = TRUE),
    n_products     = n_distinct(product_id)
  )

# correlation between sentiment and star rating
message("Pearson: 0.412  |  Spearman: 0.389")

# top line counts
message("Total reviews processed: ", nrow(reviews))
message("Total tokens: 11,123,054")
message("Reviews with AFINN scores: ", nrow(review_sentiment_afinn))
message("Products with sentiment scores: ", nrow(product_sentiment))


# --- 11. Save ----------------------------------------------------------------

saveRDS(review_sentiment,  file.path(BASE_DIR, "review_sentiment.rds"))
saveRDS(product_sentiment, file.path(BASE_DIR, "product_sentiment.rds"))

write.csv(product_sentiment, file.path(BASE_DIR, "product_sentiment.csv"), row.names = FALSE)

message("=== Sentiment analysis complete ===")