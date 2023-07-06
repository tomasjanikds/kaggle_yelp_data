# LIBRARIES ----

# Data manipulation
library(tidyverse)
library(lubridate)
library(tidyquant)

# EDA
library(DataExplorer)

# ML
library(recipes)
library(h2o)
library(lime)

# Graph
library(tidygraph)
library(ggraph)

# load helper functions
source(here::here("src", "R", "functions.R"))

# 1.0 READ-IN DATA ----
# << RUN >> this will take 10 mins to run
json_file <- here::here("data", "raw", "yelp_academic_dataset_user.json")
user_raw <- jsonlite::stream_in(textConnection(readLines(json_file)), 
                                flatten = TRUE) %>%
  as_tibble()

# 2.0 CLEANSING ----
# No missing values
plot_missing(user_raw)

# Tasks:
# 1. convert date feature
# 2. create two new features: counts of elite statuses and friends

# << RUN >> this will take 10 mins to run
user_full_tbl <- user_raw %>%
  mutate(yelping_since = lubridate::year(ymd_hms(yelping_since)),
         elite = strsplit(elite, ","),
         elite_cnt = map_int(elite, ~ length(.x)),
         friends = strsplit(friends, ","),
         friends_cnt = map_int(friends, ~ length(.x))) %>%
  select(-c(name, elite, friends))

# save full cleansed dataset if needed
# saveRDS(user_full_tbl, here::here("data", "processed", "user_cleansed.rds"))

# if for some reason your session crashes, re-create the dataset
# user_full_tbl <- readRDS(here::here("data", "processed", "user_cleansed.rds"))

# Take only sample customers
set.seed(1234)
user_tbl <- user_full_tbl %>%
  sample_n(size = 2000)

# 3.0 PREPROCESSING -----
rec_obj <- recipe(~ ., data = user_tbl) %>%
  # Scale & Center for relationship analysis
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  prep()

train_tbl <- bake(rec_obj, new_data = user_tbl)

# save sample preprocessed dataset if needed
# saveRDS(train_tbl, here::here("data", "processed", "train_tbl.rds"))

# 4.0 ADJACENCY MATRIX ----
# - User-Item Table
customer_correlation_matrix <- train_tbl %>%
  
  # Transpose data for customer similarity
  gather(key = "FEATURE", value = "value", -user_id, factor_key = TRUE) %>%
  spread(key = user_id, value = value) %>%
  select(-FEATURE) %>%
  
  # Adjacency Matrix - Perform Similarity using Correlation
  as.matrix() %>%
  cor() 

# Feel free to inspect the user-item table
# customer_correlation_matrix %>% 
#   as_tibble(rownames = "user_id")

# 5.0 PRUNING THE ADJACENCY MATRIX ----

# Tasks:
# 1. Remove customer relationships to themselves
# 2. Remove duplicate relationships
diag(customer_correlation_matrix) <- 0
customer_correlation_matrix[upper.tri(customer_correlation_matrix)] <- 0

# Prune relationships
edge_limit <- 0.99
customer_correlation_matrix[customer_correlation_matrix < edge_limit] <- 0

# sum(customer_correlation_matrix > 0)

# Filter relationships to subset of customers that have relationships
customer_correlation_matrix <- customer_correlation_matrix[rowSums(customer_correlation_matrix) > 0, colSums(customer_correlation_matrix) > 0] 

# number of customers with strong relationship
customer_correlation_matrix %>% dim()

customer_correlation_matrix %>% as_tibble(rownames = "user_id")

# Convert to long tibble with From & To column relating customers
customer_relationship_tbl <- customer_correlation_matrix %>%
  as_tibble(rownames = "from") %>%
  gather(key = "to", value = "weight", -from) %>%
  filter(weight > 0)

# inspect the object
customer_relationship_tbl

# 6.0 TBL GRAPH MANIPULATION ----

customer_tbl_graph <- customer_correlation_matrix %>%
  prep_corr_matrix_for_tbl_graph(edge_limit = 0.99) %>%
  as_tbl_graph(directed = FALSE)

customer_tbl_graph

# 6.1 Grouping Nodes (Clustering) ----
grouped_tbl_graph <- customer_tbl_graph %>%
  activate(nodes) %>%
  mutate(neighbors = centrality_degree()) %>%
  
  mutate(group = group_components()) %>%
  
  arrange(desc(neighbors)) %>%
  mutate(group_lump = group %>% as_factor() %>% fct_lump(n = 5))

# Network of customers with relationships in 6 clusters
# << RUN >> this will run 10 mins
grouped_tbl_graph %>%
  ggraph(layout = "kk") +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = group_lump), alpha = 0.5, size = 3) +
  
  theme_graph() +
  scale_color_tq(theme = "light") +
  theme(legend.position = "bottom") +
  labs(title = "Customer Network Detection")

# 7.0 MOST INFLUENTIAL CUSTOMERS ----
# This is the most valuable information for sales/marketing departments
influential_customers_tbl <- grouped_tbl_graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  group_by(group_lump) %>%
  filter(neighbors %in% max(neighbors)) %>%
  ungroup()

# Based on these customers the company can draft special offer
# and they (customers) will spread it among the cluster
influential_customers_tbl

# Inspect the features of the most influential customers
influential_customers_tbl %>%
  left_join(user_tbl, by = c("name" = "user_id")) %>%
  glimpse()

# 8.0 COMMUNITY ANALYSIS ----
# - join communities and inspect key features

user_group_tbl <- user_tbl %>%
  left_join(as_tibble(grouped_tbl_graph), by = c("user_id" = "name")) %>%
  select(group_lump, user_id, everything()) %>%
  filter(!is.na(group_lump))

user_group_tbl %>% glimpse()

# 9.0 DISSIMILARITIES -----

# Inspect the differences between individual groups by feature

# << RUN >> these lines will take 1 min (each)
user_group_tbl %>% plot_density_by(review_count, group_focus = 1)
user_group_tbl %>% plot_density_by(log(review_count), group_focus = 1)

user_group_tbl %>% plot_density_by(yelping_since, group_focus = 1)
user_group_tbl %>% plot_density_by(log(funny), group_focus = 1)

user_group_tbl %>% plot_density_by(average_stars, group_focus = 4)
user_group_tbl %>% plot_density_by(average_stars, group_focus = 5)

# 10.0 Machine Learning model to predict the group association ----

h2o.init()

user_group_h2o <- as.h2o(user_group_tbl)

# Independent features
x <- tibble(nm = names(user_group_h2o)) %>% 
  filter(!nm %in% c("group_lump", "user_id", "neighbors", "group")) %>%
  pull(nm)

# Target feature
y <- "group_lump"

# Run model training
# << RUN >> this will take 5 mins
automl_models <- h2o.automl(
  x = x, 
  y = y,
  training_frame = user_group_h2o,
  stopping_metric = "mean_per_class_error",
  max_runtime_secs = 300
)

# Select the best model
# metric used for best model selection is mean per class error
model_id <- automl_models@leaderboard %>% as_tibble() %>% 
  filter(str_detect(model_id, "DRF")) %>% 
  pull(model_id) %>% .[1]

h2o_model <- h2o.getModel(model_id)


# 11.0 LOCAL (CUSTOMER LEVEL) EXPLANATION ----
# create a local explainer 
lime_result <- lime(user_group_tbl %>% select(-group_lump, -neighbors, -group, -user_id), 
                    model = h2o_model)

# make predicitons
h2o.predict(h2o_model, newdata = as.h2o(user_group_tbl)) %>%
  as_tibble()

# Top 4 most important features contribution
# Why did the model classify the customer into the group?
explain_customer(6)
explain_customer(8)
