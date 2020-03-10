# create Ethiopia basic vaccination coverage map using coverage data from DHS 2016

# load packages
library (rgdal)
library (data.table)
library (ggplot2)
library (broom)
library (dplyr)

# map tutorial
# https://www.r-graph-gallery.com/168-load-a-shape-file-into-r.html

# clear workspace
rm (list = ls())

# create Ethiopia basic vaccination coverage map using coverage data from DHS 2016
create_map <- function () {
  
  # read Ethiopia admin level map data
  my_spdf <- readOGR (dsn     = "ETH_adm",
                      layer   = "ETH_adm1",
                      verbose = FALSE
  )
  # 'fortify' the data to get a dataframe format required by ggplot2
  spdf_fortified <- tidy (my_spdf, region = "NAME_1")
  
  # coordinates and aspect ratio
  coord <- coord_quickmap (xlim = range (spdf_fortified$long), 
                           ylim = range(spdf_fortified$lat), 
                           expand = F
  )
  asp <- coord$aspect (list (x.range = range (spdf_fortified$long), 
                             y.range = range (spdf_fortified$lat)))
  
  # read Ethiopia basic vaccination coverage data (source: DHS 2016)
  coverage <- fread (file = "eth_coverage.csv")
  
  # join coverage data to map data
  shapefile.df <- right_join (spdf_fortified, coverage, by = "id")
  setDT (shapefile.df)
  
  # admin level-1 data (11 regions in Ethiopia)
  idList <- my_spdf@data$NAME_1
  
  # centre of each region
  centroids.df         <- as.data.frame (coordinates(my_spdf))
  names (centroids.df) <- c ("Longitude", "Latitude")
  
  pop.df <- (data.frame(coverage, centroids.df))
  setDT (pop.df)
  
  # minor update of region names, latitude, and longitude
  pop.df [id == "Benshangul-Gumaz", "id"] <- "Benishangul"
  pop.df [id == "Gambela Peoples",  "id"] <- "Gambela"
  pop.df [id == "Harari People",    "id"] <- "Harari"
  pop.df [id == "Southern Nations, Nationalities and Peoples",    "id"] <- "SNNPR"
  pop.df [id == "Oromia",      "Longitude"] <- 40.4
  pop.df [id == "Harari",      "Latitude"]  <- 9
  pop.df [id == "Dire Dawa",   "Latitude"]  <- 10
  pop.df [id == "Addis Ababa", "Latitude"]  <- 9.25
  pop.df [id == "Tigray",      "Latitude"]  <- 14
  pop.df [id == "Addis Ababa", "Longitude"] <- 38.4
  pop.df [id == "Benishangul", "Longitude"] <- 35.4
  
  # create map
  p <- ggplot() + 
    geom_polygon (data = shapefile.df, 
                  aes (x = long, y = lat, group = group, fill = coverage), 
                  colour = "gold") +
    labs (title = "       Basic vaccination coverage in different regions of Ethiopia", size = 10) +

    geom_polygon (data = shapefile.df [id == "Addis Ababa"], aes(x = long, y = lat, group = group, fill = coverage), colour = "gold") +
    geom_polygon (data = shapefile.df [id == "Harari People"], aes(x = long, y = lat, group = group, fill = coverage), colour = "gold") +
    geom_text (data = pop.df, 
               aes (x = Longitude, y = Latitude, label = id), 
               size = 4, 
               color = "gold") +
    theme_void () +
    theme (plot.title = element_text (size = 20)) +
    theme (legend.title = element_text(size = 18),
           legend.text = element_text(size = 14)) + 
    scale_fill_continuous (name  = "coverage (%)", 
                           trans = "reverse", 
                           guide = guide_colourbar(reverse = TRUE))
  
  print (p)
  
  # save map figure to file
  ggsave (filename = "Ethiopia_basic_vaccination_coverage_DHS2016.png",
          width = 10,
          height = 9 * asp,
          units = "in",
          dpi = 300)
  
  ggsave (filename = "Ethiopia_basic_vaccination_coverage_DHS2016.eps",
          width = 10,
          height = 9 * asp,
          units = "in",
          dpi = 600)
  
  return (p)
  
} # end of function -- create_map


# create Ethiopia basic vaccination coverage map using coverage data from DHS 2016
create_map ()

