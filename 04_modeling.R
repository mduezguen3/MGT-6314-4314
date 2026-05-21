# =============================================================================
# MGT 6314/4314 - Understanding Markets with Data Science
# Project: How Online Reviews Influence Product Demand on Amazon
# Team: Mert Duezguen, Hannah Gordy, Tiffany Yie
#
# Script 04: Statistical Modeling & Results
# =============================================================================

# --- 1. Packages -------------------------------------------------------------

packages <- c(
  "dplyr", "tidyr", "ggplot2", "scales", "stringr",
  "broom", "corrplot", "stargazer", "car", "ggcorrplot"
)
invisible(lapply(packages, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))


# --- 2. Load & Merge All Product-Level Data ----------------------------------

BASE_DIR <- "C:/Users/Mert/OneDrive - Georgia Institute of Technology/Desktop/Amazon Datasets/processed"

saveRDS(reviewer_influence, file.path(BASE_DIR, "reviewer_influence.rds"))
saveRDS(product_influence,  file.path(BASE_DIR, "product_influence.rds"))
write.csv(product_influence, file.path(BASE_DIR, "product_influence.csv"), row.names = FALSE)

message("=== Network analysis complete ===")
model_data <- master %>%
  left_join(product_sentiment, by = "product_id") %>%
  left_join(product_influence,  by = "product_id") %>%
  filter(
    !is.na(demand_proxy),
    !is.na(avg_afinn),
    !is.na(avg_reviewer_influence),
    n_reviews >= 5
  )

message("Model dataset: ", nrow(model_data), " products")


# --- 3. Descriptive Statistics -----------------------------------------------

desc_vars <- model_data %>%
  select(
    demand_proxy,
    n_reviews,
    avg_rating,
    sd_rating,
    avg_review_length,
    avg_afinn,
    avg_bing_ratio,
    avg_reviewer_influence,
    avg_pagerank,
    pct_influential,
    price_num
  )

desc_stats <- desc_vars %>%
  summarise(across(everything(), list(
    mean   = ~ mean(.x,   na.rm = TRUE),
    median = ~ median(.x, na.rm = TRUE),
    sd     = ~ sd(.x,     na.rm = TRUE),
    min    = ~ min(.x,    na.rm = TRUE),
    max    = ~ max(.x,    na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(),
               names_to  = c("variable", ".value"),
               names_sep = "_(?=[^_]+$)") %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

print(desc_stats)
write.csv(desc_stats, file.path(BASE_DIR, "descriptive_stats.csv"), row.names = FALSE)


# --- 4. Correlation Matrix ---------------------------------------------------

fig_dir <- file.path(BASE_DIR, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir)

cor_mat <- cor(desc_vars %>% filter(complete.cases(.)), use = "pairwise.complete.obs")

p_cor <- ggcorrplot(cor_mat,
                    hc.order = TRUE,
                    type     = "lower",
                    lab      = TRUE,
                    lab_size = 2.5,
                    ggtheme  = ggplot2::theme_minimal(),
                    title    = "Correlation Matrix of Key Variables") +
  theme(
    plot.title       = element_text(size = 14, face = "bold"),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(file.path(fig_dir, "correlation_matrix.png"), p_cor,
       width = 10, height = 9, dpi = 150)
message("Correlation matrix saved.")


# --- 5. Log Transformations --------------------------------------------------

model_data <- model_data %>%
  mutate(
    log_n_reviews    = log1p(n_reviews),
    log_price        = log1p(price_num),
    log_avg_pagerank = log1p(avg_pagerank)
  )


# --- 6. Regression Models ----------------------------------------------------

m1 <- lm(demand_proxy ~
           log_n_reviews + avg_rating + sd_rating,
         data = model_data)

m2 <- lm(demand_proxy ~
           log_n_reviews + avg_rating + sd_rating +
           avg_afinn + avg_bing_ratio + sentiment_variance,
         data = model_data)

m3 <- lm(demand_proxy ~
           log_n_reviews + avg_rating + sd_rating +
           avg_reviewer_influence + pct_influential + log_avg_pagerank,
         data = model_data)

m4 <- lm(demand_proxy ~
           log_n_reviews + avg_rating + sd_rating +
           avg_afinn + avg_bing_ratio + sentiment_variance +
           avg_reviewer_influence + pct_influential + log_avg_pagerank +
           log_price + category,
         data = model_data)

m5 <- lm(demand_proxy ~
           log_n_reviews + avg_rating + sd_rating +
           avg_afinn * avg_reviewer_influence +
           avg_bing_ratio + sentiment_variance +
           pct_influential + log_avg_pagerank +
           log_price + category,
         data = model_data)


# --- 7. Model Summaries ------------------------------------------------------

lapply(list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5), function(m) {
  s <- summary(m)
  cat("\nR²:", round(s$r.squared, 4),
      " | Adj R²:", round(s$adj.r.squared, 4),
      " | F:", round(s$fstatistic[1], 2), "\n")
})

models_tidy <- purrr::map_dfr(
  list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5),
  ~ broom::tidy(.x) %>% mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    )),
  .id = "model"
)

write.csv(models_tidy, file.path(BASE_DIR, "regression_results.csv"), row.names = FALSE)
message("Regression results saved.")


# --- 8. Stargazer Table ------------------------------------------------------

stargazer(m1, m2, m3, m4, m5,
          type          = "text",
          title         = "Regression Results: Determinants of Product Demand",
          dep.var.label = "Demand Proxy (-log Sales Rank)",
          column.labels = c("Baseline", "+Sentiment", "+Influence",
                            "Full Model", "+Interaction"),
          omit          = "category",
          add.lines     = list(c("Category FE", "No", "No", "No", "Yes", "Yes")),
          out           = file.path(BASE_DIR, "regression_table.txt"))


# --- 9. VIF Check ------------------------------------------------------------

message("\n--- VIF for Full Model (m4) ---")
vif_m4 <- car::vif(m4)
print(round(vif_m4, 2))


# --- 10. Visualisations ------------------------------------------------------

clean_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_line(color = "grey92")
  )

# 10a. Coefficient plot
coef_df <- broom::tidy(m4, conf.int = TRUE) %>%
  filter(!str_starts(term, "category"), term != "(Intercept)") %>%
  mutate(term = str_replace_all(term, "_", " "))

p1 <- ggplot(coef_df, aes(x = reorder(term, estimate),
                          y = estimate,
                          ymin = conf.low,
                          ymax = conf.high,
                          color = estimate > 0)) +
  geom_pointrange(size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  coord_flip() +
  scale_color_manual(values = c("TRUE" = "#1a9850", "FALSE" = "#d73027"),
                     labels = c("Negative", "Positive"),
                     name   = "Direction") +
  labs(title    = "Coefficient Estimates: Full Regression Model",
       subtitle = "Bars show 95% confidence intervals; category FEs omitted",
       x = NULL, y = "Coefficient Estimate") +
  clean_theme

ggsave(file.path(fig_dir, "coef_plot_full_model.png"), p1, width = 10, height = 6, dpi = 150)

# 10b. R² comparison
r2_df <- purrr::map_dfr(
  list(m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5),
  ~ tibble(R2 = summary(.x)$r.squared,
           Adj_R2 = summary(.x)$adj.r.squared),
  .id = "model"
)

p2 <- r2_df %>%
  pivot_longer(-model, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = model, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("R2" = "#4575b4", "Adj_R2" = "#74add1"),
                    name = NULL, labels = c("Adj. R²", "R²")) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Model Fit: R² Across Model Specifications",
       x = "Model", y = "Variance Explained") +
  clean_theme

ggsave(file.path(fig_dir, "r2_comparison.png"), p2, width = 7, height = 4, dpi = 150)

# 10c. Sentiment vs demand
p3 <- model_data %>%
  sample_n(min(5000, nrow(model_data))) %>%
  ggplot(aes(x = avg_afinn, y = demand_proxy, color = category)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2,
              aes(group = 1), color = "black") +
  labs(title = "Average Review Sentiment vs. Product Demand",
       x = "Avg AFINN Sentiment Score",
       y = "Demand Proxy (-log Sales Rank)") +
  clean_theme +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))

ggsave(file.path(fig_dir, "sentiment_vs_demand.png"), p3, width = 9, height = 5, dpi = 150)

# 10d. Review volume vs demand
p4 <- model_data %>%
  sample_n(min(5000, nrow(model_data))) %>%
  ggplot(aes(x = log_n_reviews, y = demand_proxy, color = category)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2,
              aes(group = 1), color = "black") +
  labs(title = "Review Volume vs. Product Demand",
       x = "Log(Number of Reviews + 1)",
       y = "Demand Proxy (-log Sales Rank)") +
  clean_theme +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))

ggsave(file.path(fig_dir, "volume_vs_demand.png"), p4, width = 9, height = 5, dpi = 150)

# 10e. Reviewer influence vs demand
p5 <- model_data %>%
  sample_n(min(5000, nrow(model_data))) %>%
  ggplot(aes(x = avg_reviewer_influence, y = demand_proxy, color = category)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2,
              aes(group = 1), color = "black") +
  labs(title = "Reviewer Influence vs. Product Demand",
       x = "Avg Reviewer Influence Score [0,1]",
       y = "Demand Proxy (-log Sales Rank)") +
  clean_theme +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))

ggsave(file.path(fig_dir, "influence_vs_demand.png"), p5, width = 9, height = 5, dpi = 150)

message("All figures saved to: ", fig_dir)
message("=== Modeling complete ===")