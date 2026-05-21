# =============================================================================
# MGT 6314/4314 - Understanding Markets with Data Science
# Project: How Online Reviews Influence Product Demand on Amazon
# Team: Mert Duezguen, Hannah Gordy, Tiffany Yie
#
# Script 01: Data Loading & Preprocessing
# =============================================================================

# --- 1. Install & Load Packages ----------------------------------------------

packages <- c(
  "jsonlite", "dplyr", "tidyr", "stringr",
  "lubridate", "readr", "purrr"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
invisible(lapply(packages, install_if_missing))
invisible(lapply(packages, library, character.only = TRUE))


# --- 2. Configuration --------------------------------------------------------

REVIEWS_DIR  <- "C:/Users/Mert/OneDrive - Georgia Institute of Technology/Desktop/Amazon Datasets/Reviews"
METADATA_DIR <- "C:/Users/Mert/OneDrive - Georgia Institute of Technology/Desktop/Amazon Datasets/Metadata"

CATEGORIES <- c(
  "Cell_Phones_and_Accessories",
  "Clothing_Shoes_and_Jewelry",
  "Electronics",
  "Home_and_Kitchen"
)

SAMPLE_LINES <- 50000


# --- 3. Helper: Read standard .json.gz (reviews) -----------------------------

read_json_gz <- function(filepath, n_lines = Inf) {
  message("  Reading: ", basename(filepath))
  con   <- gzcon(file(filepath, "rb"))
  lines <- readLines(con, n = n_lines, warn = FALSE)
  close(con)
  
  records <- purrr::map(lines, function(ln) {
    tryCatch(jsonlite::fromJSON(ln, simplifyVector = TRUE),
             error = function(e) NULL)
  })
  dplyr::bind_rows(purrr::compact(records))
}


# --- 4. Helper: Read Python-dict .json.gz (metadata) ------------------------

python_dict_to_json <- function(ln) {
  ln |>
    # single-quoted keys/values -> double-quoted
    stringr::str_replace_all("'([^']*)'", '"\\1"') |>
    # Python True/False/None -> JSON true/false/null
    stringr::str_replace_all("\\bTrue\\b",  "true")  |>
    stringr::str_replace_all("\\bFalse\\b", "false") |>
    stringr::str_replace_all("\\bNone\\b",  "null")
}

read_meta_gz <- function(filepath, n_lines = Inf) {
  message("  Reading: ", basename(filepath))
  con   <- gzcon(file(filepath, "rb"))
  lines <- readLines(con, n = n_lines, warn = FALSE)
  close(con)
  
  records <- purrr::map(lines, function(ln) {
    tryCatch(
      jsonlite::fromJSON(python_dict_to_json(ln), simplifyVector = TRUE),
      error = function(e) NULL
    )
  })
  dplyr::bind_rows(purrr::compact(records))
}


# --- 5. Load Reviews ---------------------------------------------------------

load_reviews <- function(category, n_lines = SAMPLE_LINES) {
  path <- file.path(REVIEWS_DIR, paste0("reviews_", category, ".json.gz"))
  if (!file.exists(path)) {
    message("  SKIPPING (file not found): ", basename(path))
    return(NULL)
  }
  df <- read_json_gz(path, n_lines)
  df$category <- category
  df
}

message("=== Loading Reviews ===")
reviews_raw <- purrr::map_dfr(CATEGORIES, load_reviews)
message("Total review rows loaded: ", nrow(reviews_raw))


# --- 6. Clean Reviews --------------------------------------------------------

reviews <- reviews_raw %>%
  select(
    reviewer_id   = reviewerID,
    product_id    = asin,
    reviewer_name = reviewerName,
    helpful       = helpful,
    review_text   = reviewText,
    overall       = overall,
    summary       = summary,
    unix_time     = unixReviewTime,
    review_time   = reviewTime,
    category
  ) %>%
  mutate(
    helpful_votes     = purrr::map_int(helpful, ~ if (length(.x) >= 1) as.integer(.x[[1]]) else 0L),
    total_votes       = purrr::map_int(helpful, ~ if (length(.x) >= 2) as.integer(.x[[2]]) else 0L),
    helpfulness_ratio = ifelse(total_votes > 0, helpful_votes / total_votes, NA_real_),
    review_date       = as.Date(review_time, format = "%m %d, %Y"),
    review_year       = lubridate::year(review_date),
    overall           = as.numeric(overall)
  ) %>%
  select(-helpful, -review_time) %>%
  filter(
    !is.na(overall),
    !is.na(review_text),
    stringr::str_length(review_text) > 10
  )

message("Clean reviews: ", nrow(reviews))


# --- 7. Load Metadata --------------------------------------------------------
read_meta_gz <- function(filepath, n_lines = Inf) {
  message("  Reading: ", basename(filepath))
  con   <- gzcon(file(filepath, "rb"))
  lines <- readLines(con, n = n_lines, warn = FALSE)
  close(con)
  
  records <- purrr::map(lines, function(ln) {
    tryCatch({
      parsed <- jsonlite::fromJSON(python_dict_to_json(ln), simplifyVector = TRUE)
      # extract salesRank before dropping nested cols
      sr_val <- NA_real_
      if (!is.null(parsed$salesRank) && length(parsed$salesRank) > 0) {
        sr_val <- suppressWarnings(as.numeric(parsed$salesRank[[1]]))
      }
      # drop all nested list columns
      parsed <- parsed[!sapply(parsed, is.list)]
      parsed$sales_rank_num <- sr_val
      as.data.frame(t(unlist(parsed)), stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  })
  dplyr::bind_rows(purrr::compact(records))
}

load_metadata <- function(category, n_lines = SAMPLE_LINES) {
  path <- file.path(METADATA_DIR, paste0("meta_", category, ".json.gz"))
  if (!file.exists(path)) {
    message("  SKIPPING (file not found): ", basename(path))
    return(NULL)
  }
  df <- read_meta_gz(path, n_lines)
  df$category <- category
  df
}

message("=== Loading Metadata ===")
meta_raw <- purrr::map_dfr(CATEGORIES, load_metadata)
message("Total metadata rows loaded: ", nrow(meta_raw))

# --- 8. Clean Metadata -------------------------------------------------------
meta <- meta_raw %>%
  rename_with(tolower) %>%
  mutate(
    sales_rank_num = suppressWarnings(as.numeric(sales_rank_num)),
    price_num      = suppressWarnings(as.numeric(
      stringr::str_replace_all(as.character(price), "[$,]", "")
    ))
  ) %>%
  select(
    product_id = asin,
    any_of(c("title", "brand", "category")),
    price_num,
    sales_rank_num
  ) %>%
  filter(!is.na(product_id)) %>%
  distinct(product_id, .keep_all = TRUE)

message("Clean metadata products: ", nrow(meta))


# --- 9. Aggregate Review Stats per Product -----------------------------------

product_review_stats <- reviews %>%
  group_by(product_id) %>%
  summarise(
    n_reviews          = n(),
    avg_rating         = mean(overall, na.rm = TRUE),
    sd_rating          = sd(overall,   na.rm = TRUE),
    pct_1star          = mean(overall == 1, na.rm = TRUE),
    pct_5star          = mean(overall == 5, na.rm = TRUE),
    avg_helpful_votes  = mean(helpful_votes, na.rm = TRUE),
    avg_helpfulness    = mean(helpfulness_ratio, na.rm = TRUE),
    avg_review_length  = mean(stringr::str_length(review_text), na.rm = TRUE),
    n_unique_reviewers = n_distinct(reviewer_id),
    .groups = "drop"
  )


# --- 10. Build Master Table --------------------------------------------------

master <- product_review_stats %>%
  left_join(meta, by = "product_id") %>%
  filter(!is.na(sales_rank_num)) %>%
  mutate(
    log_sales_rank = log(sales_rank_num),
    log_n_reviews  = log1p(n_reviews),
    demand_proxy   = -log_sales_rank
  )

message("Master dataset rows: ", nrow(master))


# --- 11. Preview & Save ------------------------------------------------------

print(head(master))
glimpse(master)

out_dir <- file.path(
  "C:/Users/Mert/OneDrive - Georgia Institute of Technology/Desktop/Amazon Datasets",
  "processed"
)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

saveRDS(reviews, file.path(out_dir, "reviews_clean.rds"))
saveRDS(meta,    file.path(out_dir, "metadata_clean.rds"))
saveRDS(master,  file.path(out_dir, "master_product.rds"))

write.csv(master, file.path(out_dir, "master_product.csv"), row.names = FALSE)

message("=== Done. Files saved to: ", out_dir)