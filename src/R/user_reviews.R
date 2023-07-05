# LIBRARIES ----
library(tidyverse)
library(lubridate)
library(tidyquant)
library(DataExplorer)
library(correlationfunnel)
library(recipes)
library(tidygraph)
library(ggraph)

## read-in user data
json_file <- "data/raw/yelp_academic_dataset_user.json"
user_raw <- jsonlite::stream_in(textConnection(readLines(json_file)), 
                                flatten = TRUE) %>%
  as_tibble()

user_raw %>%
  glimpse()

# 2.0 CLEANSING ----
plot_missing(user_raw)

user_full_tbl <- user_raw %>%
  mutate(yelping_since = lubridate::year(ymd_hms(yelping_since)),
         elite = strsplit(elite, ","),
         elite_cnt = map_int(elite, ~ length(.x)),
         friends = strsplit(friends, ","),
         friends_cnt = map_int(friends, ~ length(.x))) %>%
  select(-c(name, elite, friends))

saveRDS(user_full_tbl, here::here("data", "processed", "user_cleansed.rds"))

user_full_tbl <- readRDS(here::here("data", "processed", "user_cleansed.rds"))

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

train_tbl %>% glimpse()

# saveRDS(train_tbl, here::here("data", "processed", "train_tbl.rds"))

# 5.0 ADJACENCY MATRIX ----
# - User-Item Table

customer_correlation_matrix <- train_tbl %>%
  
  # Transpose data for customer similarity
  gather(key = "FEATURE", value = "value", -user_id, factor_key = TRUE) %>%
  spread(key = user_id, value = value) %>%
  select(-FEATURE) %>%
  
  # Adjacency Matrix - Perform Similarity using Correlation
  as.matrix() %>%
  cor() 

# customer_correlation_matrix %>% class()

# customer_correlation_matrix %>% 
#   as_tibble(rownames = "user_id")

# 6.0 PRUNING THE ADJACENCY MATRIX ----

# 6.1 Remove customer relationships to themselves
diag(customer_correlation_matrix) <- 0

# 6.2 Remove duplicate relationships 
customer_correlation_matrix[upper.tri(customer_correlation_matrix)] <- 0

# 6.3 Prune relationships
edge_limit <- 0.99
customer_correlation_matrix[customer_correlation_matrix < edge_limit] <- 0

sum(customer_correlation_matrix > 0)


# 6.4 Filter relationships to subset of customers that have relationships
customer_correlation_matrix <- customer_correlation_matrix[rowSums(customer_correlation_matrix) > 0, colSums(customer_correlation_matrix) > 0] 
customer_correlation_matrix %>% dim()

customer_correlation_matrix %>% as_tibble(rownames = "user_id")


# 6.5 Convert to Long Tibble with From & To Column Relating Customers
customer_relationship_tbl <- customer_correlation_matrix %>%
  as_tibble(rownames = "from") %>%
  gather(key = "to", value = "weight", -from) %>%
  filter(weight > 0)

customer_relationship_tbl

# 7.0 WORKFLOW - Convert to Function for Dynamic Filtering of Edge Limit ----

prep_corr_matrix_for_tbl_graph <- function(correlation_matrix, edge_limit = 0.9999) {
  
  diag(correlation_matrix) <- 0
  
  correlation_matrix[upper.tri(correlation_matrix)] <- 0
  
  correlation_matrix[correlation_matrix < edge_limit] <- 0
  
  correlation_matrix <- correlation_matrix[rowSums(correlation_matrix) > 0, colSums(correlation_matrix) > 0] 
  
  correlation_matrix %>%
    as_tibble(rownames = "from") %>%
    gather(key = "to", value = "weight", -from) %>%
    filter(weight > 0)
  
}

prep_corr_matrix_for_tbl_graph(customer_correlation_matrix, edge_limit = 0.9999)

# 7.0 NETWORK VISUALIZATION ----

customer_correlation_matrix %>%
  
  prep_corr_matrix_for_tbl_graph(edge_limit = 0.99) %>%
  
  as_tbl_graph(directed = FALSE) %>%
  
  ggraph(layout = "kk") +
  geom_edge_link(alpha = 0.5, color = palette_light()["blue"]) +
  geom_node_point(alpha = 0.5, color = palette_light()["blue"]) +
  theme_graph(background = "white")

# 8.0 TBL GRAPH MANIPULATION ----

customer_tbl_graph <- customer_correlation_matrix %>%
  prep_corr_matrix_for_tbl_graph(edge_limit = 0.99) %>%
  as_tbl_graph(directed = FALSE)

customer_tbl_graph

# 8.1 Ranking Nodes - Rank by topological traits ----
customer_tbl_graph %>%
  activate(nodes) %>%
  
  mutate(node_rank = node_rank_traveller()) %>%
  arrange(node_rank)


# 8.2 Centrality - Number of edges going in/out of node ----
customer_tbl_graph %>%
  activate(nodes) %>%
  
  mutate(neighbors = centrality_degree()) %>%
  arrange(desc(neighbors))


# 8.3 Grouping Nodes (Clustering) ----
grouped_tbl_graph <- customer_tbl_graph %>%
  activate(nodes) %>%
  mutate(neighbors = centrality_degree()) %>%
  
  mutate(group = group_components()) %>%
  
  arrange(desc(neighbors)) %>%
  mutate(group_lump = group %>% as_factor() %>% fct_lump(n = 5))


grouped_tbl_graph %>%
  ggraph(layout = "kk") +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = group_lump), alpha = 0.5, size = 3) +
  
  theme_graph() +
  scale_color_tq(theme = "light") +
  theme(legend.position = "bottom") +
  labs(title = "Customer Network Detection")

# 9.0 MOST INFLUENTIAL CUSTOMERS ----
influential_customers_tbl <- grouped_tbl_graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  group_by(group_lump) %>%
  filter(neighbors %in% max(neighbors)) %>%
  ungroup()

influential_customers_tbl

influential_customers_tbl %>%
  left_join(user_tbl, by = c("name" = "user_id")) %>%
  glimpse()

# 10.0 COMMUNITY ANALYSIS ----
# - Join Communities and Inspect Key Features

user_group_tbl <- user_tbl %>%
  left_join(as_tibble(grouped_tbl_graph), by = c("user_id" = "name")) %>%
  select(group_lump, user_id, everything()) %>%
  filter(!is.na(group_lump))

user_group_tbl %>% glimpse()


plot_density_by <- function(data, col, group_focus = 1, ncol = 1) {
  
  col_expr <- enquo(col)
  
  data %>%
    mutate(focus = as.character(group_lump)) %>%
    select(focus, everything()) %>%
    mutate(focus = ifelse(as.character(focus) == as.character(group_focus), 
                          "1", "Other")) %>%
    mutate(focus = as.factor(focus)) %>%
    
    ggplot(aes(!! col_expr, fill = focus)) +
    geom_density(alpha = 0.4) +
    facet_wrap(~ focus, ncol = ncol) +
    scale_fill_tq() +
    theme_tq()
}

# 10.1 Dissimilarities -----

user_group_tbl %>% plot_density_by(review_count, group_focus = 1)
user_group_tbl %>% plot_density_by(log(review_count), group_focus = 1)

user_group_tbl %>% plot_density_by(yelping_since, group_focus = 1)
user_group_tbl %>% plot_density_by(log(funny), group_focus = 1)

user_group_tbl %>% plot_density_by(average_stars, group_focus = 4)
user_group_tbl %>% plot_density_by(average_stars, group_focus = 5)

library(h2o)
library(lime)

# H2O MODEL TRAINING -----

h2o.init()

user_group_h2o <- as.h2o(user_group_tbl)

x <- tibble(nm = names(user_group_h2o)) %>% 
  filter(!nm %in% c("group_lump", "user_id", "neighbors", "group")) %>%
  pull(nm)

y <- "group_lump"

automl_models <- h2o.automl(
  x = x, 
  y = y,
  training_frame = user_group_h2o,
  max_runtime_secs = 300
  # include_algos = "DRF"
)

model_id <- automl_models@leaderboard %>% as_tibble() %>% filter(str_detect(model_id, "DRF")) %>% pull(model_id) %>% .[1]

h2o_model <- h2o.getModel(model_id)


# PERFORM LIME ----

lime_result <- lime(user_group_tbl %>% select(-group_lump, -neighbors, -group, -user_id), 
                    model = h2o_model)

# Function to explain the results
explain_customer <- function(cust_pos) {
  
  explain_result <- lime::explain(
    x = user_group_tbl %>% slice(cust_pos) %>% select(-group_lump, -neighbors, -group, -user_id),
    explainer  = lime_result,
    n_labels   = 1,
    n_features = 4, 
    n_permutations = 5000)
  
  lime::plot_features(explain_result)
}

user_group_tbl

h2o.predict(h2o_model, newdata = as.h2o(user_group_tbl)) %>%
  as_tibble()

explain_customer(6)
explain_customer(7)
