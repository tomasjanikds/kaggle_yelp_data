################################################################################
#                                                                              #
#                         Prepare Data for Shiny App                           #
#                                                                              #
################################################################################

# Script Description:
# ------------------
# This script prepares the data for use in a Shiny app. It performs data 
# preprocessing and filtering steps to extract relevant information from a Yelp 
# business dataset and create a new processed dataset specifically tailored 
# for the Shiny app.

library(tidyverse)

json_file <- here::here("data", "raw", "yelp_academic_dataset_business.json")
business_raw <- jsonlite::stream_in(textConnection(readLines(json_file, n=30000)), 
                                    flatten = TRUE) %>%
  as_tibble()

coffee_string <- c("coffee", "cafes", "cafe")

business_tbl <- business_raw %>%
  filter(str_detect(categories, 
                    regex(paste(coffee_string, collapse = "|"), 
                          ignore_case = TRUE))) %>%
  select(name, address, city, state, postal_code, latitude, longitude,
         stars, review_count, starts_with("hours")) %>%
  mutate(across(starts_with("hours"), ~ gsub(".*-(\\d+).*", "\\1", .x))) %>%
  mutate(across(starts_with("hours"), ~ ifelse(.x == "0", "24", .x))) %>%
  mutate(across(starts_with("hours"), ~ as.integer(.x))) %>%
  rename_with(., ~ gsub("hours.", "", .x))

saveRDS(business_tbl, here::here("data", "processed", "business_tbl.rds"))
