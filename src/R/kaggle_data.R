## install kaggler package from github
# devtools::install_github("ldurazo/kaggler")

library(tidyverse)
library(readr)
library(kaggler)

# Download data ----
# Should you wich to download datasets, uncomment following lines
# kgl_auth(creds_file = 'kaggle.json')
# 
# response <- kgl_datasets_download_all(owner_dataset = "yelp-dataset/yelp-dataset")
# 
# download.file(response[["url"]], "data/temp.zip", mode="wb")
# unzip_result <- unzip("data/temp.zip", exdir = "data/", overwrite = TRUE)

json_file <- "data/yelp_academic_dataset_business.json"
business_tbl <- jsonlite::stream_in(textConnection(readLines(json_file, n=1000)), 
                                    flatten = TRUE) %>%
  as_tibble()

json_file <- "data/yelp_academic_dataset_review.json"
review_tbl <- jsonlite::stream_in(textConnection(readLines(json_file, n=1000)), 
                                  flatten = TRUE) %>%
  as_tibble()

json_file <- "data/yelp_academic_dataset_checkin.json"
checkin_tbl <- jsonlite::stream_in(textConnection(readLines(json_file, n=1000)), 
                                   flatten = TRUE) %>%
  as_tibble()

json_file <- "data/yelp_academic_dataset_tip.json"
tip_tbl <- jsonlite::stream_in(textConnection(readLines(json_file, n=1000)), 
                               flatten = TRUE) %>%
  as_tibble()

json_file <- "data/yelp_academic_dataset_user.json"
user_tbl <- jsonlite::stream_in(textConnection(readLines(json_file, n=1000)), 
                                flatten = TRUE) %>%
  as_tibble()

# features
map(list(business_tbl, review_tbl, checkin_tbl, tip_tbl, user_tbl),
    ~ glimpse(.x))


## O2 assignmnent
json_file <- "data/yelp_academic_dataset_user.json"
user_tbl <- jsonlite::stream_in(textConnection(readLines(json_file)), 
                                flatten = TRUE) %>%
  as_tibble()


