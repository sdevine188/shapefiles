# leaflet shape file mapping
# https://rstudio.github.io/leaflet/shapes.html

library(rgdal)
library(leaflet)
library(stringr)
library(dplyr)

setwd("H:/R/shapefiles/census_us_shape_files/cb_2015_us_state_20m")

# From https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
states <- readOGR("cb_2015_us_state_20m.shp",
                  layer = "cb_2015_us_state_20m", verbose = FALSE)

names(states)
head(states$STATEFP)
head(states$NAME)
dim(states)
str(states)

neStates <- subset(states, states$STUSPS %in% c(
        "CT","ME","MA","NH","RI","VT","NY","NJ","PA"
))

pal <- colorNumeric(
        palette = "Blues",
        domain = as.numeric(as.character(states$STATEFP))
)



leaflet(neStates) %>% addTiles() %>%
        addPolygons(
                stroke = FALSE, fillOpacity = 0.75, smoothFactor = 0.5,
                color = ~ pal(as.numeric(as.character(neStates$STATEFP))),
                popup = ~ neStates$NAME
        )

# test to add new data to shapes
states$test <- seq(1:52)
length(states$test)
head(states$test)

leaflet(neStates) %>% addTiles() %>%
        addPolygons(
                stroke = FALSE, fillOpacity = 0.75, smoothFactor = 0.5,
                color = ~ pal(as.numeric(as.character(neStates$test))),
                popup = ~ str_c(neStates$NAME, neStates$test, sep = "<br/>")
        )
