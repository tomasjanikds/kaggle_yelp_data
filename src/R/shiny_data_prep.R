# prepare data for shiny app

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

# first 20 quakes
df.20 <- quakes[1:20,]

getColor <- function(quakes) {
  sapply(quakes$mag, function(mag) {
    if(mag <= 4) {
      "green"
    } else if(mag <= 5) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor2(df.20)
)

leaflet(df.20) %>% addTiles() %>%
  addAwesomeMarkers(~long, ~lat, icon=icons, label=~as.character(mag))

# categories
coffee_string <- c("coffee", "cafes", "cafe")

cats <-  business_raw %>%
  pull(categories) %>%
  unique()

# Filter the data frame based on coffee_string using tidyverse and str_detect()
business_raw %>%
  filter(str_detect(categories, 
                    regex(paste(coffee_string, collapse = "|"), 
                          ignore_case = TRUE))) %>%
  distinct(categories)

getColor2 <- function(data) {
  data %>%
    mutate(color = ifelse(mag <= 4, "green", "orange")) %>%
    pull(color)
}
