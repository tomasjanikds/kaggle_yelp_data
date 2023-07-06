################################################################################
#                                                                              #
#                         Download and Read Kaggle Dataset                     #
#                                                                              #
################################################################################

# Script Description:
# ------------------
# This script provides instructions on how to download and read a Kaggle dataset
# into R for further analysis. It outlines the necessary steps and code snippets
# to accomplish the task.

# Kaggle Account and API Setup
# ------------------------------------
# Before downloading the dataset, make sure you have a Kaggle account and have
# set up the Kaggle API. Follow the Kaggle documentation for instructions on
# creating an account and generating an API key.

## install kaggler package from github
# devtools::install_github("ldurazo/kaggler")

library(tidyverse)
library(readr)
library(kaggler)

# Download data ----
# << RUN >> this might take up to 30 mins
# Should you wish to download datasets, uncomment following lines
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