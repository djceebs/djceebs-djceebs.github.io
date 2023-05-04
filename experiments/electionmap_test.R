# Lifted from: https://www.infoworld.com/article/3586147/how-to-create-an-election-map-in-r.html

library(dplyr) 
library(glue) 
library(scales) 
library(htmltools) 
library(sf)
library(leaflet)
pa_data <- rio::import("pa_2016_data_files/pa_2016_presidential.csv")

pa_geo <- sf::st_read("pa_2016_data_files/PaCounty2020_08/PaCounty2020_08.shp", 
                      stringsAsFactors = FALSE)

names(pa_geo)[2] <- "County" # Changes name of COUNTY20 to County

problems <- anti_join(pa_geo, pa_data, by = "County") # IDs rows that dont match
head(problems[,1:3])

pa_data$County[pa_data$County == "McKEAN"] <- "MCKEAN" # Renames to same county
anti_join(pa_geo, pa_data, by = "County")

pa_map_data <- merge(pa_geo, pa_data, by = "County")
pa_map_data <- st_transform(pa_map_data, "+proj=longlat +datum=WGS84")

min_max_values <- range(pa_map_data$Margin, na.rm = TRUE)

trump_palette <- colorNumeric(palette = "Reds", 
                              domain=c(min_max_values[1], min_max_values[2]))
clinton_palette <- colorNumeric(palette = "Blues", 
                                domain=c(min_max_values[1], min_max_values[[2]]))

trump_df <- pa_map_data[pa_map_data$Winner == "Trump",]
clinton_df <- pa_map_data[pa_map_data$Winner == "Clinton",]

trump_popup <- glue("<strong>{trump_df$County} COUNTY</strong><br />
                    <strong>Winner: Trump</strong><br />
                    Trump: {scales::comma(trump_df$Trump, accuracy = 1)}<br />
                    Clinton: {scales::comma(trump_df$Clinton, accuracy = 1)}<br />
                    Margin: {scales::comma(trump_df$Margin, accuracy = 1)}")  %>%   
  lapply(htmltools::HTML)
clinton_popup <- glue("<strong>{clinton_df$County} COUNTY</strong><br />
                      <strong>Winner: Clinton</strong><br />
                      Clinton: {scales::comma(clinton_df$Clinton, accuracy = 1)}<br />
                      Trump: {scales::comma(clinton_df$Trump, accuracy = 1)}<br />
                      Margin: {scales::comma(clinton_df$Margin, accuracy = 1)}")  %>%   
  lapply(htmltools::HTML)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = trump_df,
    fillColor = ~trump_palette(trump_df$Margin),
    label = trump_popup,
    stroke = TRUE,
    smoothFactor = 0.2,
    fillOpacity = 0.8,
    color = "#666",
    weight = 1
  ) %>%
  addPolygons(
    data = clinton_df,
    fillColor = ~clinton_palette(clinton_df$Margin),
    label = clinton_popup,
    stroke = TRUE,
    smoothFactor = 0.2,
    fillOpacity = 0.8,
    color = "#666",
    weight = 1
  )
