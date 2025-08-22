packages <- c(
  "tidyverse", "caret", "randomForest", "plotly", "shiny", "rmarkdown",
  "e1071"  # caret dependency for some models
)
new_pkgs <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")

library(tidyverse)
library(caret)
library(randomForest)
library(plotly)


df <- read.csv("dataset.csv", stringsAsFactors = FALSE)
cat("Columns in dataset:\n"); print(names(df))


aqi_col_candidates <- c("AQI Category", "AQI.Category", "AQI_Category", "AQI Value", "AQI.Value")
pm25_candidates <- c("PM2.5 AQI Value", "PM2.5.AQI.Value", "PM25.AQI.Value", "PM2_5_AQI_Value")
city_candidates  <- c("City", "city")
country_candidates <- c("Country", "country")

find_col <- function(cands) { intersect(cands, names(df)) %>% first() }
aqi_col <- find_col(aqi_col_candidates)
pm25_col <- find_col(pm25_candidates)
city_col <- find_col(city_candidates)
country_col <- find_col(country_candidates)


pollutant_candidates <- c("CO AQI Value", "Ozone AQI Value", "NO2 AQI Value", "PM2.5 AQI Value",
                          "CO.AQI.Value", "Ozone.AQI.Value", "NO2.AQI.Value", "PM2.5.AQI.Value")
pollutants <- intersect(pollutant_candidates, names(df))
cat("Detected pollutant columns:\n"); print(pollutants)


N <- nrow(df)
M <- if(!is.null(city_col)) length(unique(df[[city_col]])) else NA
K <- if(!is.null(country_col)) length(unique(df[[country_col]])) else NA


missing_pct_total <- round(mean(is.na(df)) * 100, 3)


unhealthy_labels <- c("Unhealthy", "Very Unhealthy", "Hazardous")

if(!is.null(aqi_col)) {
  aqi_tab <- table(df[[aqi_col]], useNA = "ifany")
  pct_unhealthy <- round(mean(df[[aqi_col]] %in% unhealthy_labels, na.rm = TRUE) * 100, 2)
} else {
  aqi_tab <- NA
  pct_unhealthy <- NA
}


top10_share_pct <- NA
if(!is.null(aqi_col) & !is.null(city_col)) {
  unhealthy_mask <- df[[aqi_col]] %in% unhealthy_labels
  city_unhealthy <- df %>%
    filter(!is.na(.[[city_col]])) %>%
    mutate(unhealthy = unhealthy_mask) %>%
    group_by_at(city_col) %>%
    summarise(unhealthy_count = sum(unhealthy, na.rm = TRUE)) %>%
    arrange(desc(unhealthy_count)) %>%
    slice(1:10)
  total_unhealthy <- sum(unhealthy_mask, na.rm = TRUE)
  top10_share_pct <- if(total_unhealthy > 0) round(sum(city_unhealthy$unhealthy_count) / total_unhealthy * 100, 2) else 0
}


cor_pm25_aqi <- NA
if(!is.null(pm25_col) & !is.null(aqi_col)) {
  df_corr <- df %>% select(all_of(c(pm25_col, aqi_col))) %>% drop_na()
  if(nrow(df_corr) > 2) cor_pm25_aqi <- round(cor(df_corr[[pm25_col]], df_corr[[aqi_col]], use = "complete.obs"), 3)
}


rf_cv_acc <- NA
rf_n_samples <- NA
if(!is.null(aqi_col) & length(pollutants) >= 2) {
  model_df <- df %>% select(all_of(c(pollutants, aqi_col))) %>% drop_na()
  names(model_df)[ncol(model_df)] <- "AQI_CAT"  
  model_df$AQI_CAT <- as.factor(model_df$AQI_CAT)
  if(nrow(model_df) >= 20) {
    set.seed(42)
    ctrl <- trainControl(method = "cv", number = 5)
    rf_mod <- train(AQI_CAT ~ ., data = model_df, method = "rf", trControl = ctrl)
    best_row <- which.max(rf_mod$results$Accuracy)
    rf_cv_acc <- round(rf_mod$results$Accuracy[best_row] * 100, 2)
    rf_n_samples <- nrow(model_df)
  }
}