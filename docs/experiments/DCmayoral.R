
#library("tmaptools")
#library("rio")
library("sf")
library("leaflet")
library("tidyverse")
library("readr")
library("tigris")
library("stringr") # Remove word from string
library("tidycensus")
library("janitor")

# Cleaning the Data ----


## Original downloading table and map ----
#dccsv <- read_csv("data/June_21_2022_Primary_Election_Certified_Results.csv")

url <- "https://electionresults.dcboe.org/Downloads/Reports/June_21_2022_Primary_Election_Certified_Results.csv"
dccsv <- read_csv(url) # load election districts
dcmapdata <- voting_districts("11") # load tigris shapefile

## Cleaning Original ----

candidates <- c("Muriel E. Bowser", 'Trayon "Washington DC" White', 
                "Robert White")

dc_filter <- dccsv %>% #Filter to Candidates, Ward Number, Precinct, & Votes
  select(PrecinctNumber, WardNumber, Candidate, Votes) 

dc_clean <- dc_filter %>% 
  filter(Candidate %in% candidates)

dc2 <- dc_clean %>% #better way to convert from long to wide
  group_by(PrecinctNumber) %>% # create sum by precinct
  mutate(nvotes = sum(Votes)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Candidate, values_from = Votes) %>% 
  rename(precinct = PrecinctNumber, ward = WardNumber, 
         bowser = `Muriel E. Bowser`, twhite = `Trayon "Washington DC" White`, rwhite = `Robert White`) %>% 
  mutate()

dc2 <- dc2 %>% 
  mutate(across(bowser:rwhite, .fns = ~ .x/nvotes, .names = "{.col}_percent"), # percent of vote
         across(bowser:rwhite, .fns = ~ .x - (nvotes-.x), .names = "{.col}_margin"), # margin of vote
         across(bowser_margin:rwhite_margin, .fns = ~ .x/nvotes, .names = "{.col}pct")) # percent margin

## DC Map data New ----

dcmapclean <- dcmapdata %>%
  mutate(NAME = str_remove(NAME20, "Precinct "),
         precinct = NAME)

dc2$precinct <- as.character(dc2$precinct)

dc_join <- dcmapclean %>% 
  left_join(dc2, by = c("NAME" = "precinct"))

### Figuring out the revealing one color part ----
dc_join <- st_transform(dc_join, "+proj=longlat +datum=WGS84")

f <- function(x){ifelse(rowSums(x)==0, NA, names(x)[max.col(x, "first")])}

dc_candidates <- dc_join %>% 
  mutate(winner = f(across(bowser:rwhite)))

saveRDS(dc_candidates, "data/dc_candidates.RDS")

bowser_df <- dc_candidates[dc_candidates$winner == "bowser",]
twhite_df <- dc_candidates[dc_candidates$winner == "twhite",]
rwhite_df <- dc_candidates[dc_candidates$winner == "rwhite",]

bowser_palette <- colorNumeric(palette = "Greens", 
                              domain = bowser_df$bowser_percent)
twhite_palette <- colorNumeric(palette = "Oranges", 
                               domain = twhite_df$twhite_percent)
rwhite_palette <- colorNumeric(palette = "Blues", 
                               domain = rwhite_df$rwhite_percent)


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = bowser_df,
    fillColor = ~bowser_palette(bowser_df$bowser_percent),
    stroke = TRUE,
    smoothFactor = 0.2,
    fillOpacity = 0.8,
    color = "#666",
    weight = 1,
    popup = paste0("Winner: Muriel Bowser", "<br>",
      "Precinct Name: ", dc_join$precinct, "<br>",
                  "Votes per precinct: ", dc_join$bowser, 2), 
  ) %>%
  addPolygons(
    data = twhite_df,
    fillColor = ~twhite_palette(twhite_df$twhite_percent),
    stroke = TRUE,
    smoothFactor = 0.2,
    fillOpacity = 0.8,
    color = "#666",
    weight = 1,
    popup = paste0("Winner: Trayon White", "<br>",
                   "Precinct Name: ", dc_join$precinct, "<br>",
                   "Votes per precinct: ", dc_join$twhite, 2)
  ) %>% 
addPolygons(
  data = rwhite_df,
  fillColor = ~rwhite_palette(rwhite_df$rwhite_percent),
  stroke = TRUE,
  smoothFactor = 0.2,
  fillOpacity = 0.8,
  color = "#666",
  weight = 1, 
  popup = paste0("Winner: Robert White", "<br>",
                 "Precinct Name: ", dc_join$precinct, "<br>",
                 "Votes per precinct: ", dc_join$rwhite, 2)
)

### Race characteristics ----

#dc_race <- read_csv("data/Demographic Characteristics of DC Wards.csv")

# load acs data
#subject_vars <- load_variables(2021, "acs5", cache = TRUE)

#acs_vars <- c(total = "DP05_0033E", white = "DP05_0037E", black = "DP05_0038E", 
# aian = "DP05_0039E", asian =  "DP05_0044E", nhopi =  "DP05_0052E", other = "DP05_0057E",
# hisplat = "DP05_0070E")

### Retrieve ACS ----

acs <- read_csv("data/ACS_Demographic_Characteristics_DC_Ward.csv") %>% 
  select(NAMELSAD, DP05_0033E, DP05_0037E, DP05_0038E, DP05_0039E, DP05_0044E, 
         DP05_0052E, DP05_0057E, DP05_0070E) 

acs <- read_csv("data/DECENNIALPL2020.P1-2023-04-13T192742.csv")  
  acs_test <- acs

  # clean data
acs_test <- acs_test[,-2]%>% 
    clean_names() %>% 
  head(8)

prec <- paste0(1:144) # narrow to precinct
colnames(acs_test)[2:145] = prec

acs_test <- acs_test %>% # reorganize table
  pivot_longer(!label_grouping, names_to = "precinct", values_to = "count") %>% 
  pivot_wider(names_from = label_grouping, values_from = count) %>% 
  clean_names()
acs_test <- acs_test[,-3]

colnames(acs_test)[c(2:8)] <- c("race_total", "white", "black", "aian",
                           "asian", "nhopi", "other") # rename col

### Merge ACS with Results ----
dcmapclean <- dcmapclean %>% 
  select(INTPTLAT20, INTPTLON20, geometry, precinct)

  #voting_precinct <- read_csv("data/Voting_Precinct_2019.csv")
results <- dccsv %>% 
  select(PrecinctNumber, WardNumber) %>% 
  distinct()

colnames(results)[c(1:2)] <- c("precinct", "ward")  
results$ward <- as.character(results$ward)
results$precinct <- as.character(results$precinct)

acsmap <- left_join(results, acs_test, by = "precinct")
acsmaps_dc <- left_join(dcmapclean, acsmap, by = "precinct") %>% 
  mutate(across(white:other, .fns = ~ .x / race_total, .names = "{col}_pct" ))  

acsmaps_sf <- st_transform(acsmaps_dc, crs = 4326)

saveRDS(acsmaps_sf, "data/acsmaps_sf.RDS")

### Mapping ----
pal_black <- colorNumeric(palette = "Greens", domain = acsmaps_sf$black_pct)
pal_white <- colorNumeric(palette = "Reds", domain = acsmaps_sf$white_pct)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = acsmaps_sf,
              fillColor = ~pal_white(white_pct),
              fillOpacity = 0.8,
              weight = 2, 
              opacity = 1,
              color = "black") %>% 
  addLegend("bottomright", pal = pal_white, values = acsmaps_sf$white_pct, 
            title = "Percent of White Ward residents", opacity = 0.7)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = acsmaps_sf,
              fillColor = ~pal_black(black_pct),
              fillOpacity = 0.8,
              weight = 2, 
              opacity = 1,
              color = "black") %>% 
  addLegend("bottomright", pal = pal_black, values = acsmaps_sf$black_pct, 
            title = "Percent of Black residents by Ward", opacity = 0.7)
