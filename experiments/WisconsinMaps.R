### PA Election Map

library(dplyr) 
library(sf)
library(leaflet)
library(readxl)
library(tigris)
library(janitor)

### Load Data ---


pa_precincts <- voting_districts(state = "42", county = "101", year = 2019) # FIPS for PA and Philly

pa_precinct1 <- pa_precincts %>% 
  select(NAME20, geometry)

url <- "https://vote.phila.gov/files/raw-data/2019_general.csv"
philly_2019 <- read_csv(url)

### Clean Data ---

philly <- philly_2019 %>% 
  clean_names() %>% 
  filter(category == "MAYOR") 

philly <- philly[, c(1,2,4,5,7,3,6)] # Reorders columns so that I don't have to filter list, but is kind of finicky 
philly_votes <- philly[, (1:4)] # selects just the columns I want

philly_votes <- philly_votes %>% # Vote counts per ward
  group_by(ward, division, selection) %>% 
  summarise(nvotes = sum(vote_count)) %>% # total votes from absentee/mail/present
  mutate(total = sum(nvotes)) # total votes per ward

philly_wider <- philly_votes %>% # converts from wide to long
  group_by(ward, division) %>% 
  pivot_wider(
    names_from = selection,
    values_from = nvotes) %>% 
  clean_names() 

names(philly_wider)[4] <- "ciancaglini" 
names(philly_wider)[5] <- "kenney"
names(philly_wider)[6] <- "write_in"

phillywinner <- philly_wider %>% 
  mutate(
    across(ciancaglini:write_in, .fns = ~ .x/total, .names = "{.col}_percent"),
  across(ciancaglini:write_in, .fns = ~ .x - (total-.x), .names = "{.col}_margin"),
   across(ciancaglini_margin:write_in_margin, .fns = ~ .x/total, .names = "{.col}pct"))

#### Create Ward and Precinct #

pa_precincts <- pa_precincts %>% 
  separate(NAME20, into = c("city", "ward", "ward no", "precinct", "precinct no"), sep = " ", remove = FALSE) %>% 
  unite(col = "wardprecinct", c(`ward no`, `precinct no`), sep = " ", remove = TRUE)

phillywinner <- phillywinner %>% 
  unite(col = "wardprecinct", c(ward, division), sep = " ", remove = TRUE)

pa_map <- pa_precincts %>% 
  left_join(phillywinner, by = c("wardprecinct" = "wardprecinct")) # check for bad rows

pa_maps <- st_transform(pa_map, "+proj=longlat +datum=WGS84")

# determine winner
f <- function(x){ifelse(rowSums(x)==0, NA, names(x)[max.col(x, "first")])}

pa_maps <- pa_maps %>% 
  mutate(winner = f(across(ciancaglini:write_in)))

### Visualize ---

kenney_df <- pa_maps[pa_maps$winner == "kenney",]
ciancaglini_df <- pa_maps[pa_maps$winner == "ciancaglini",]

kenney_palette <- colorNumeric(palette = "Blues", 
                               domain = pa_maps$kenney_marginpct)
ciancaglini_palette <- colorNumeric(palette = "Reds", 
                               domain = pa_maps$ciancaglini_marginpct)


leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = ciancaglini_df,
    fillColor = ~ciancaglini_palette(ciancaglini_df$ciancaglini_marginpct),
    stroke = TRUE,
    smoothFactor = 0.2,
    fillOpacity = 0.8,
    color = "#666",
    weight = 1,
    popup = paste0("Winner: Billy Ciancaglini"), 
  ) %>%
  addPolygons(
    data = kenney_df,
    fillColor = ~kenney_palette(kenney_df$kenney_marginpct),
    stroke = TRUE,
    smoothFactor = 0.2,
    fillOpacity = 0.8,
    color = "#666",
    weight = 1,
    popup = paste0("Winner: Jim Kenney"))
 