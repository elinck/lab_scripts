library(tidyverse)
library(ggplot2) # load ggplot graphics library
library(sf) # load Rspatial package
library(maps) # load "maps" package
library(elevatr) # load "elevatr" package
library(terra) # load "terra" package

# set map projections
map_proj <- st_crs("EPSG:4326")

# get state and county shpfiles
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE), crs = map_proj)
montana <- states[states$ID=="montana",]
counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE), crs = map_proj)
counties <- subset(counties, grepl("montana", counties$ID))

# check geometry of objects
st_geometry(montana)
st_geometry(counties)

# transform CRS
montana <- st_transform(montana, crs = map_proj) 
counties <- st_transform(counties, crs = map_proj) 

# double-check
st_geometry(montana)
st_geometry(counties)

# load occurence data!
points <- read_csv("data/montana_records.csv") 
points_sf <- st_as_sf(points, coords = c("decimalLon", "decimalLat"), crs = map_proj)

# simple ggplot of montana!
ggplot(data=counties) +
  theme_bw() +
  geom_sf() +
  geom_sf(data = points_sf, pch=1) + 
  theme(axis.title = element_blank())

# OK but let's separate it out by class
ggplot(data=counties) +
  theme_bw() +
  geom_sf() +
  geom_sf(data = points_sf, aes(color=class), pch=1) + 
  theme(axis.title = element_blank())

# or year? let's change the labels
ggplot(data=counties) +
  theme_bw() +
  geom_sf() +
  geom_sf(data = points_sf, aes(color=year), pch=1) + 
  theme(axis.title = element_blank()) +
  xlab("longitude") +
  ylab("latitude")

# let's practice plotting shapefiles! we'll use MT's level III ecoregions: 
# https://www.epa.gov/eco-research/ecoregion-download-files-state-region-8
mt_shp <- st_read("data/mt_eco_l3/mt_eco_l3.shp") # load shapefiles—notice other suffixes are required in the folder
mt_shp_transf <- st_transform(mt_shp, crs = map_proj) 
ggplot(data=montana) +
  theme_bw() +
  geom_sf(data=mt_shp_transf, aes(fill=US_L3NAME), alpha=0.5) + #"US_L3NAME" is an attribute of the .shp file
  geom_sf(data = points_sf, pch=1) +
  xlab("longitude") +
  ylab("latitude")

# another common task is plotting elevation. we can use the "elevatr" package to download a DEM to do so
# download state elev raster or load locally
f_elev_state <- "data/mt_elevs.tif"
if (!file.exists(f_elev_state)) {
  elevation_data <- get_elev_raster(locations=montana, z = 5, clip = "locations")
  terra::writeRaster(elevation_data, f_elev_state, filetype = "GTiff")                                                           
} else {
  elevation_data <- rast(f_elev_state)
}

# calculate ground resolution for z=5 
# (https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution)
ground_resolution <- (cos(45 * pi/180) * 2 * pi * 6378137) / (256 * 2^5)
ground_resolution

# crop raster down to scale
cropped_elev <- crop(elevation_data, montana)
mt_elev <- cropped_elev %>% raster::as.data.frame(xy=TRUE)
head(mt_elev)
colnames(mt_elev) <- c("x", "y", "elevation")

# plot as gradient
ggplot(data=montana) +
  theme_bw() +
  geom_sf() +
  geom_raster(data=mt_elev, aes(x = x, y = y, fill = elevation)) +
  scale_fill_viridis_c(na.value="transparent") +
  xlab("longitude") +
  ylab("latitude")

# points on this, maybe?
ggplot(data=montana) +
  theme_bw() +
  geom_sf() +
  geom_raster(data=mt_elev, aes(x = x, y = y, fill = elevation)) +
  scale_fill_viridis_c(na.value="transparent") +
  geom_sf(data = points_sf, pch=1, color="white") +
  xlab("longitude") +
  ylab("latitude")


# plot with simple break!
ggplot(data=montana) +
  theme_bw() +
  geom_sf() +
  geom_raster(data=mt_elev, aes(x = x, y = y, fill = elevation)) +
  binned_scale(aesthetics = "fill",
               palette = function(x) c("grey","darkgreen"),
               breaks = c(0, 1800),
               limits = c(0, 3500),
               show.limits = TRUE, 
               guide = FALSE) +
  geom_sf(data = points_sf, pch=1, color="black") +
  xlab("longitude") +
  ylab("latitude")

# let's do some simple statistics!
mtgrid <- montana %>%
  st_make_grid(cellsize = 0.5) %>%
  st_intersection(montana) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())

# why don't we look at this?
plot(mtgrid)
mtgrid <- st_transform(mtgrid, crs = map_proj) 

# create a simple feature showing where each species occurs
sp_occ_sf <- points_sf %>%
  group_by(species) %>%
  summarise()
sp_occ_sf

# calculate species richness in each grid cell
richness_grid <- mtgrid %>%
  st_join(sp_occ_sf) %>% #join montana cell grid with simple feature of species occurence
  mutate(overlap = ifelse(!is.na(id), 1, 0)) %>% # create "overlap" column adding a 1 for each species in a cell
  group_by(cellid) %>% # summarize over cell ids
  summarize(num_species = sum(overlap)) # create "number of species" column

# and plot!
ggplot(richness_grid) +
  geom_sf(data = montana, fill = NA, size = 0.1) +
  geom_sf(aes(fill = num_species), color = NA) +
  labs(fill = "richness") +
  scale_fill_viridis_c() +
  xlab("longitude") +
  ylab("latitude") +
  theme_bw() +
  ggtitle("Collecting Hotspots in the Northern Plains")

  
