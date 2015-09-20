library(ggplot2)
library(rgdal)
library(scales)
library(ggmap)
library(dplyr)
library(RCurl)
library(stringr)
library(datasets)
library(maps)

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


tract <- readOGR(dsn = ".", layer = "gz_2010_13_140_00_500k")
# use names(tract) to see options for the region argument
names(tract)
tract <- fortify(tract, region="GEO_ID")

# download data
data <- getURL("http://api.census.gov/data/2012/acs5/profile?get=DP03_0099PE&for=tract:*&in=state:13&key=905cf5cb3674a223a81618f2365a799a6330bed4")
data1 <- read.csv(text = data, stringsAsFactors = FALSE)

# clean data
names(data1) <- c("rate", "state", "county", "tract")
data1 <- select(data1, 1:4)
data1$rate <- str_replace(data1$rate, "\\[", "")
# some tracts have a dash for the rate
dash_index <- which(data1$rate == "-")
data1$rate[dash_index] <- 0
data1$rate <- as.numeric(as.character(data1$rate))
data1$rate <- data1$rate / 100
data1$tract <- str_replace(data1$tract, "\\]", "")
data1$id <- str_c("1400000US", data1$state, str_pad(data1$county, 3, "left", pad = "0"), data1$tract)


# merge data with tract
df <- merge(tract, data1, by = "id", all.x = TRUE)
df1 <- join(tract, data1, by = "id")
df2 <- arrange(df, order)

p <- ggplot() + geom_polygon(data = df1, aes(x = long, y = lat, group = group,
        fill = rate), color = "black", size = 0.25) + coord_map() + 
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

