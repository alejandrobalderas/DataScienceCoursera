

# load packages
library(ggplot2)
library(ggmap)
library(ggalt)

# ========= GET INFORMATION =========
# Run this code to get the information on the cities, or just load the data 
# stored in the text file SupplierInfo.txt and mapMexico.RData

# Get the names of the cities
supplier_city <- c("monterrey","guadalajara", "mexico city", "san luis potosi",
                   "ciudad obregon", "puebla", "saltillo", "ciudad juarez")
# Add the country mexico to all the cities
supplier_city <- paste0(supplier_city,", mexico")
# Get the coordinates of all cities
supplier_city_coords <- geocode(supplier_city, force = TRUE)
# Number of suppliers per city
numberOfSuppliers <- c(5,4,3,5,7,8,4,4)
# Region in Mexico of the cities
region <- c("North","Center", "Center","Center","North","Center","North","North")
# Save information as a Data Frame
suppliers <- data.frame(supplier_city,supplier_city_coords2, numberOfSuppliers, region)
# Get the information of a roadmap from mexico taken from google
mex_road_map <- qmap("mexico", zoom = 5, source = "google", maptype = "roadmap", force = TRUE)


# ========= PLOT IN A MAP  ==============
# Load the data
suppliers <- read.table("SupplierInfo.txt", header=TRUE, sep = " ")
load("mapMexico.RData")
# Make the plots
mex_road_map + geom_point(aes(x = lon, y = lat),
                          data = suppliers,
                          alpha = 0.7,
                          size = suppliers$numberOfSuppliers,
                          color = "red") +
    geom_encircle(aes(x=lon, y=lat, fill = region),
                  data = suppliers,expand = 0, size = 2,alpha = 0.4, color = "steelblue")
