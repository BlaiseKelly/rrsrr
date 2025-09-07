#library(remotes)
#remotes::install_github("nptscot/osmactive")

library(osmactive)
library(mapview)
library(sf)
library(dplyr)
library(osmdata)
library(h3jsr)
library(tmap)
library(cols4all)
library(stats19)

cities <- c("Manchester", "London", "Bristol", "Leeds", "Liverpool", "Edinburgh", "Birmingham", "Bath", "Newcastle",
            "Cardiff", "Swansea", "Southampton")

for (c in cities[1]){


  osm = get_travel_network(c)

  saveRDS(osm, paste0(c,".RDS"))
  print(c)

}

osm <- readRDS("Manchester.RDS")



# apply osm active functions
cycle_net = get_cycling_network(osm)
drive_net = get_driving_network(osm)
cycle_net_d = distance_to_road(cycle_net, drive_net)
cycle_net_c = classify_cycle_infrastructure(cycle_net_d)
#m = plot_osm_tmap(cycle_net_c)

# request collision data (entering 2004 results in a table with all years)
crashes = get_stats19(year = "2004", type = "collision", ask = FALSE, format = TRUE, output_format = "data.frame")

## request casualty
casualties = get_stats19(year = "2004", type = "casualty", ask = FALSE, format = TRUE, output_format = "data.frame")

# join together
ca_cra <- inner_join(casualties,crashes)

# make as a spatial data frame
cas_sf <- ca_cra |>
  filter(!is.na(location_easting_osgr)) |>
  filter(!is.na(location_northing_osgr)) |>
  st_as_sf(coords = c("location_easting_osgr", "location_northing_osgr"), crs = 27700) |>
  st_transform(4326)

dir.create("data/")

# download shape of greater manchester
download.file("https://assets.publishing.service.gov.uk/media/5a74e12940f0b65c0e845386/Greater_Manchester_LEP.zip", destfile = "data/Greater_Manchester_LEP.zip")

# unzip thhe downloaded file
unzip("data/Greater_Manchester_LEP.zip",exdir = "data")

# import shapefile
manc_shp <- st_read("data/Greater_Manchester_LEP.shp") |>
  st_transform(4326) |>
  st_union()

# trim to GM boundary
cycle_net_c <- st_intersection(cycle_net_c,manc_shp)

dir.create("plots/")

# plot map of cycle routes as determined by osmactive
tm_1 <-
  tm_shape(manc_shp)+
  tm_polygons(alpha = 0)+
  tm_shape(cycle_net_c)+
  tm_lines("detailed_segregation",lwd = 2, title = "type of cycle path")+
  tm_layout(frame = FALSE)

tmap_save(tm_1, "plots/cycle_routes.png")


# create h3
hex_ids <- h3jsr::polygon_to_cells(manc_shp, res = 8)

# Convert H3 indexes back to sf polygons
hex_sf <- st_as_sf(h3jsr::cell_to_polygon(hex_ids))

# assign corret index
hex_sf$h3_index <- unlist(hex_ids)

# filter cycle network for new infrastruture
cycle_net_seg <- filter(cycle_net_c,detailed_segregation %in% c('Level track', 'Light segregation'))

# find hexes that cover these routes
hex_cycling <- st_as_sf(hex_sf[cycle_net_seg,])

# get the rest that don't
hex_no_cycling <- filter(hex_sf,!h3_index %in% hex_cycling$h3_index)

# plot
tm_1 <-
  tm_shape(manc_shp)+
  tm_polygons(alpha = 0, lwd = 0.2)+
  tm_shape(hex_sf)+
  tm_polygons(alpha = 0, lwd = 1)+
  tm_layout(frame = FALSE)
tm_1
tmap_save(tm_1, "plots/hex_sf.png")


tm_1 <-
  tm_shape(manc_shp)+
  tm_polygons(alpha = 0, lwd = 0.2)+
  tm_shape(cycle_net_seg)+
  tm_lines("detailed_segregation",lwd = 2, title = "type of cycle path")+
  tm_shape(hex_cycling)+
  tm_polygons(alpha = 0, lwd = 1)+
  tm_layout(frame = FALSE)
tm_1
tmap_save(tm_1, "plots/cycle_hex.png")

# trim casualties to manchester shp
cas_sf <- cas_sf[manc_shp,]

# pick out only cyclists
cas_sf_cyclist <- cas_sf |>
  filter(casualty_type == "Cyclist") |>
  distinct(accident_reference, .keep_all = TRUE) |>
  mutate(number_of_casualties = 1) |>
  select(casualty_type, casualty_severity, accident_year, number_of_casualties, geometry)


# create periods
cas_sf_c_14_19 <- filter(cas_sf_cyclist, accident_year >= 2014 & accident_year < 2019)

cas_sf_c_GT_19 <- filter(cas_sf_cyclist, accident_year >= 2019)


hex_cycling_sf <- hex_cycling |> st_as_sf()

hex_cycling_sf_intersects_p1 <- st_intersects(hex_cycling_sf,cas_sf_c_14_19)
hex_cycling_sf_intersects_p2 <- st_intersects(hex_cycling_sf,cas_sf_c_GT_19)

hex_cycling_sf$p1 <- vapply(hex_cycling_sf_intersects_p1,length,numeric(1))
hex_cycling_sf$p2 <- vapply(hex_cycling_sf_intersects_p2,length,numeric(1))

hex_no_cycling_sf <- hex_no_cycling |> st_as_sf()

hex_no_cycling_sf_intersects_p1 <- st_intersects(hex_no_cycling_sf,cas_sf_c_14_19)
hex_no_cycling_sf_intersects_p2 <- st_intersects(hex_no_cycling_sf,cas_sf_c_GT_19)

hex_no_cycling_sf$p1 <- vapply(hex_no_cycling_sf_intersects_p1,length,numeric(1))
hex_no_cycling_sf$p2 <- vapply(hex_no_cycling_sf_intersects_p2,length,numeric(1))

hex_cycling_sf$diff <- hex_cycling_sf$p2-hex_cycling_sf$p1

hex_no_cycling_sf$diff <- hex_no_cycling_sf$p2-hex_no_cycling_sf$p1

# what is average change for each region
mean_diff_no_cycle_infra <- mean(hex_no_cycling_sf$diff)
mean_diff_cycle_infra <- mean(hex_cycling_sf$diff)

# final plots
bks <- seq(-10,15,by = 2)

tm_1 <-
tm_shape(hex_no_cycling_sf)+
  tm_polygons("diff", breaks = bks, lwd = 0.5, title = "difference in total casualties")+
tm_shape(hex_cycling_sf)+
  tm_polygons("diff", breaks = bks, legend.show = FALSE, lwd = 1.5)+
  tm_layout(frame = FALSE)

tmap_save(tm_1, "plots/diff.png")

bks <- seq(0,16,by = 2)

tm_1 <-
  tm_shape(hex_no_cycling_sf)+
  tm_polygons("p1", breaks = bks, lwd = 0.5, title = "total casualties 2014-2018")+
  tm_shape(hex_cycling_sf)+
  tm_polygons("p1", breaks = bks, legend.show = FALSE, lwd = 1.5)+
  tm_layout(frame = FALSE)

tmap_save(tm_1, "plots/tot_p1.png")

tm_1 <-
  tm_shape(hex_no_cycling_sf)+
  tm_polygons("p2", breaks = bks, lwd = 0.5, title = "total casualties 2019-2023")+
  tm_shape(hex_cycling_sf)+
  tm_polygons("p2", breaks = bks, legend.show = FALSE, lwd = 1.5)+
  tm_layout(frame = FALSE)

tmap_save(tm_1, "plots/tot_p2.png")

