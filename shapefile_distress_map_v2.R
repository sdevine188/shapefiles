library(ggplot2)
library(rgdal)
library(scales)
library(ggmap)
library(dplyr)
library(RCurl)
library(stringr)
library(datasets)
library(maps)
library(maptools)
library(dplyr)
library(RColorBrewer)
library(readr)


# http://www.r-bloggers.com/things-i-forget-reading-a-shapefile-in-r-with-readogr/

# tutorial at http://www.kevjohnson.org/making-maps-in-r/

# download shapefile for census tract in georgia in 2010
# go to the generalized "GENZ" folder for cartographic boundary files via the FTP 
# http://www2.census.gov/geo/tiger/
# or download via browser https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html

# other tiger products 
# https://www.census.gov/geo/maps-data/data/tiger.html

# note that census describes generalized cartographic boundary files as better for small areas, since they render faster than ungeneralized TIGER shapefiles

# might need gpclibPermit() to use fortify if you get error
# http://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true

# setwd
setwd("C:/Users/Stephen/Desktop/R/EDA/shapefile_distress_map/cb_2014_us_cd114_20m")

# read in tiger line file
counties_shape <- readOGR(dsn = ".", layer = "tl_2016_us_county")
names(counties_shape)
gpclibPermit()
counties_shape <- fortify(counties_shape, region="GEOID")
head(counties_shape)


######################################


# write hawaii and alaska to file


setwd("C:/Users/Stephen/Desktop/R/EDA/shapefile_distress_map")

hawaii <- df %>% filter(state == "HI")
write_csv(hawaii, "hawaii_shape.csv")
hawaii <- read_csv("hawaii_shape.csv")

alaska <- df %>% filter(state == "AL")
write_csv(alaska, "alaska_shape.csv")
alaska <- read_csv("alaska_shape.csv")


#####################################


# merge data with counties_shape
setwd("C:/Users/Stephen/Desktop/R/EDA/distress")
counties <- read_csv("counties_20170131.csv")

choropleth <- left_join(counties_shape, counties, by = c("id" = "fips_state_county"))
choropleth <- arrange(choropleth, order)


######################################


# create alaska map
choropleth <- alaska

# select color palette using brewer.pal
# then use colorRampPalette to build a function "pal" which divides the palette by a given number of factors
# display.brewer.all()
colors <- brewer.pal(9, "Blues")
pal <- colorRampPalette(colors)
# pal is a function, which takes a number as it's argument eg. pal(14)
# to generalize: pal(length(unique(choropleth$rate_d1)))

# create levels for pc_inc_distress for use in legend names
choropleth <- choropleth %>% mutate(pc_inc_distress_fct = case_when(.$pc_inc_distress == "0" ~ "Not Distressed", .$pc_inc_distress == "1" ~ "Distressed"))
choropleth$pc_inc_distress_fct <- factor(choropleth$pc_inc_distress_fct)
levels <- c("Not Distressed", "Distressed")
choropleth$pc_inc_distress_fct2 <- factor(choropleth$pc_inc_distress_fct, levels = levels, ordered = TRUE)





######################################


hawaii <- choropleth %>% filter(state == "HI")

p <- ggplot() + geom_polygon(data = hawaii, aes(x = long, y = lat, group = group,
        fill = unemp_rate), color = "white", size = 0.25) + coord_map() + 
        scale_fill_distiller(palette = "Blues")

# borrowing my styling from choropleth.examples.R script to make it look cleaner
# this is the better styling
p <- ggplot() + geom_polygon(data = df1, aes(x = long, y = lat, group = group,
                fill = rate), color = "black", size = 0.25) + theme_bw() + 
                scale_fill_distiller(palette = "Blues", labels = percent, breaks = pretty_breaks(n = 10), values = c(1,0)) +
                theme(plot.background = element_blank(), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
                axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title=element_text(size=20,face="bold")) + 
                labs(x = "", y = "", title = "Percent Without Health Insurance") + coord_fixed() + coord_map(project = "conic", lat0 = 30)

# without tract boundary lines 
p <- ggplot() + geom_polygon(data = df1, aes(x = long, y = lat, group = group,
                                             fill = rate)) + theme_bw() + 
        scale_fill_distiller(palette = "Blues", labels = percent, breaks = pretty_breaks(n = 10), values = c(1,0)) +
        theme(plot.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title=element_text(size=20,face="bold")) + 
        labs(x = "", y = "", title = "Percent Without Health Insurance") + coord_fixed() + coord_map(project = "conic", lat0 = 30)


ggsave(p, file = "map1.png", width = 6, height = 4.5, type = "cairo-png")





# test of reading in census city shapefiles
# http://www.census.gov/geo/maps-data/data/cbf/cbf_concity.html
# download shapefile, and copy all files from zipped folder into a working directory

# see census GEOID file for All Places, including Consolidated Cities FIPS
# http://www.census.gov/geo/maps-data/data/relationship.html
# variable PLACEFP10 is the CONCIT FIPS code used as the id in the city shapefiles

# general info on consolidated cities
# http://www.census.gov/geo/reference/webatlas/concities.html

# data seems to only have milford city???

cities <- readOGR(dsn = ".", layer = "cb_2014_09_concity_500k")
# call structure on cities to find out how to refer to GEOID id variable (eg. "GEO_ID" or "GEOID")
str(cities)
cities <- fortify(cities, region="GEOID")

# plot map
p <- ggplot() + geom_polygon(data = cities, aes(x = long, y = lat, group = group), color = "black", size = 0.25, fill = "red") + coord_map() 





# example with noaa cities shapefiles
# http://www.nws.noaa.gov/geodata/catalog/national/html/cities.htm
# doesn't work, gpplot can't read spatial data file??
cities <- readOGR(dsn = ".", layer = "ci08au12")
# call structure on cities to find out how to refer to GEOID id variable (eg. "GEO_ID" or "GEOID")
str(cities)
cities <- fortify(cities, region="ID")




# example with tiger/line cities shapefiles
# http://www.census.gov/geo/maps-data/data/tiger-line.html
# only shows milford again, and only has same six state options
cities <- readOGR(dsn = ".", layer = "tl_2014_09_concity")
# call structure on cities to find out how to refer to GEOID id variable (eg. "GEO_ID" or "GEOID")
str(cities)
cities <- fortify(cities, region="GEOID")




# try with 2012 place file
# ftp://ftp2.census.gov/geo/tiger/TIGER2012/PLACE/

# this works!

cities <- readOGR(dsn = ".", layer = "tl_2012_01_place")
# call structure on cities to find out how to refer to GEOID id variable (eg. "GEO_ID" or "GEOID")
str(cities)
cities <- fortify(cities, region="GEOID")

state_map <- map_data("state")
al_map <- filter(state_map, region == "alabama")

p <- ggplot() + geom_polygon(data = cities, aes(x = long, y = lat, group = group), color = "black", size = 0.25, fill = "red") + coord_map() +
        geom_polygon(data = al_map, aes(x = long, y = lat, group = group) colour = "black", fill = NA)


# iterative changes to working ggplot script in choropleth.examples
ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) + geom_polygon(data = state_map_input, colour = "black", fill = NA) + 
        scale_fill_manual(values = pal(length(unique(choropleth_input$rate_d2)))) + theme_bw() + 
        theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), 
        axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        plot.title = element_text(size=20, face = "bold")) + labs(x = "", y = "", title = "Unemployment rate in Alabama", fill = "Unemployment rate") + 
        coord_fixed() + coord_map(project = "conic", lat0 = 30)


ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) + geom_polygon(data = state_map_input, colour = "black", fill = NA) + theme_bw() + 
        theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), 
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
              plot.title = element_text(size=20, face = "bold")) + labs(x = "", y = "", title = "Unemployment rate in Alabama", fill = "Unemployment rate") + 
        coord_fixed() + coord_map(project = "conic", lat0 = 30)

# this works!
ggplot(data = al_map, aes(x = long, y = lat, group = group)) + 
        geom_polygon(data = cities, aes(x = long, y = lat, group = group), color = "black", size = 0.25, fill = "red") + 
        geom_polygon(data = al_map, colour = "black", fill = NA) +theme_bw() + 
        theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), 
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
              plot.title = element_text(size=20, face = "bold")) + labs(x = "", y = "", title = "Consolidated Cities in Alabama", fill = "Unemployment rate") + 
        coord_fixed() + coord_map(project = "conic", lat0 = 30)

