library(ggplot2)
library(rgdal)
library(scales)
library(ggmap)
library(dplyr)
library(Cairo)
library(RCurl)
library(stringr)
library(plyr)
library(dplyr)

# tutorial at http://www.kevjohnson.org/making-maps-in-r/

# download shapefile for census tract in georgia in 2010
# http://www2.census.gov/geo/tiger/

# other tiger products 
# https://www.census.gov/geo/maps-data/data/tiger.html

tract <- readOGR(dsn = ".", layer = "gz_2010_13_140_00_500k")
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

ggsave(p, file = "map1.png", width = 6, height = 4.5, type = "cairo-png")



