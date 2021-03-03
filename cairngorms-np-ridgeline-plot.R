# Ridgeline plot workflow with Cairngorms National Park:
# s-mxwll

# Load packages:
library(extrafont) # to change font
library(ggplot2) # general plotting
library(ggridges) # ridge plotting
library(raster) # to import and manipulate raster
library(rgdal) # to import shapefile
library(sp)
library(mapproj)

# Part 1: Getting the spatial data in

# Create file store:
dir.create("data")

# Download the GeoTiff file (https://dwtkns.com/srtm/):
download.file(url = "http://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_36_01.zip",
              destfile = "data/srtm_36_01.zip")

# Extract tif file:
unzip(zipfile = "data/srtm_36_01.zip",
      exdir = "data")

# Import raster (DEM):
rst <- raster("data/srtm_36_01.tif")

# Download the national park shapefile from the Scottish Government (https://maps.gov.scot/ATOM/shapefiles/SG_CairngormsNationalPark_2010.zip):
download.file(url = "https://maps.gov.scot/ATOM/shapefiles/SG_CairngormsNationalPark_2010.zip",
              destfile = "data/SG_CairngormsNationalPark_2010.zip")

# Extract shapefile:
unzip(zipfile = "data/SG_CairngormsNationalPark_2010.zip",
      exdir = "data")

# Import shapefile:
shp <- readOGR("data/SG_CairngormsNationalPark_2010.shp")

# Print Coordinate Reference System of DEM:
proj4string(rst)

# 
proj4string <- (shp)

# Shapefile reprojection:
shp <- spTransform(shp, proj4string(rst))

# Plot DEM and overlay shapefile:
plot(rst)
plot(shp, add = TRUE)

# Cut DEM to the shapefile:
cropped_rst <- mask(crop(rst, extent(shp)), shp)

# Check:
plot(cropped_rst)

# Part 2: Create grid along which to extract data

# Make grid programatically:

# Get geographic extent of raster:
extent <- extent(cropped_rst)

# Set number of transects:
t <- 100

# Get spacing between transects:
y_frac <- (extent[4] - extent[3])/t

# Create vector of latitudes to sample:
y_ic <- seq(extent[3], extent[4], ((extent[4] - extent[3])/(t - 1)))

# Set number points to sample along each transect:
n <- 1000

# Get spacing between points:
x_frac <- (extent[2] - extent[1])/n

# Create vector of longitudes to sample:
x_ic <- seq(extent[1], extent[2], ((extent[2] - extent[1])/(n - 1)))

# Create grid from all combinations of x and y:
grid <- expand.grid(cond = x_ic, rating = y_ic)

# Re-name headers:
colnames(grid)[colnames(grid) == "rating"] <- "lat"
colnames(grid)[colnames(grid) == "cond"] <- "long"

# ***
coordinates(grid) = ~long+lat
proj4string(grid) <- crs(cropped_rst)

# ***
grid <- spTransform(grid, CRS("+proj=longlat"))

# Extract elevation data from points:
result <- raster::extract(cropped_rst, grid, df = TRUE, cellnumbers = TRUE)

# Combine elevation data and coordinates using cell number:
transect_data <- cbind(result, coordinates(cropped_rst)[result[,2],])

# Part 3: Plotting

# Check data frame and change column headers:
head(transect_data)
names(transect_data)[3] <- "Elev"
names(transect_data)[4] <- "Long"
names(transect_data)[5] <- "Lat"

# Plot the transects using ggplot2 & ggridges:
plot_basic <- ggplot(transect_data,
                     aes(x = Long,
                         y = Lat,
                         group = Lat,
                         height = Elev)) +
  geom_density_ridges(stat = "identity")

# Call the default plot object:
plot_basic

# Customise the appearance to mimic Joy Division's Unknown Pleasures artwork:
plot_rl <- ggplot(transect_data,
                   aes(x = Long,
                       y = Lat,
                       group = Lat,
                       height = Elev)) +
  geom_density_ridges(stat = "identity",
                      scale = 6,
                      fill = "white",
                      color = "black") +
  scale_x_continuous(name = "CAIRNGORMS NATIONAL PARK") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "white"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour = "black",
                                    size = 18,
                                    family = "Poppins")) +
  coord_map() # projects the transect data to a specified PCS

# Call the stylized plot object:
plot_rl

# Save plot as a PDF:
ggsave("cairngorms-np.pdf", width = 297, height = 210, units = "mm")
