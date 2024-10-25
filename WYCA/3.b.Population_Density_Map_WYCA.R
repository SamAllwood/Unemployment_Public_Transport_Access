### RUN 3.c.PopulationDensityChange_Liverpool.R FIRST to create the accurate Population shapefile
# 1. Setup ----------------------------------------------------------------
options(rgl.useNULL = FALSE)
require(tidyverse)
require(sf)
require(tmap)
require(ggplot2)
require(mapview)
require(stars)
require(rayshader)
require(MetBrewer)
require(colorspace)
require(rayrender)
require(magick)
require(extrafont)
library(stars)

setwd("~/Library/CloudStorage/GoogleDrive-sam.allwood3@gmail.com/My Drive/Consulting/Unemployment_Public_Transport_Access/Liverpool_City_Region")

# 2. Load Data ------------------------------------------------------------
# Read population density shapefile from "Population_Density_Change.R" script
LCR_pop <- read_sf("../../Data/LCR_population.shp") %>%
  st_transform(4326) %>%
  rename("Pop_Dens_change" = "Pp_dns_",
         "LSOA21CD" = "LSOA21C",
         "Pop_Dens"  = "P_2021_") %>%
  dplyr::select(LSOA21CD, Pop_Dens_change, Pop_Dens, geometry)

# check the boundary plot
ggplot(LCR_pop) +
  geom_sf(aes(fill = Pop_Dens),
          color = "gray66",
          linewidth = 0)+
  theme_minimal() 


# Create Bounding Box -----------------------------------------------------

# setting the boundary as a bounding box
bbox <- st_bbox(LCR_pop) %>%
  st_set_crs(4326)

# finding the aspect ratio
bottom_left <- st_point(c(bbox[["xmin"]], bbox[["ymin"]])) %>%
  st_sfc(crs = 4326)
bottom_right <- st_point(c(bbox[["xmax"]], bbox[["ymin"]])) %>%
  st_sfc(crs = 4326)
top_left <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>%
  st_sfc(crs = 4326)
top_right <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>%
  st_sfc(crs = 4326)

width <- st_distance(bottom_left, bottom_right)
height <- st_distance(bottom_left, top_left)

if(width > height) {
  w_ratio = 1
  h_ratio = height / width
  
} else {
  h_ratio = 1.1
  w_ratio = width / height
}

# convert to raster to convert to matrix
# size = 100
size = 250 * 3.5

pop_raster <- st_rasterize(
  LCR_pop,
  nx = floor(size * w_ratio) %>% as.numeric(),
  ny = floor(size * h_ratio) %>% as.numeric()
)


pop_dens_change_matrix <- matrix(pop_raster$Pop_Dens_change,
                     nrow = floor(size * w_ratio),
                     ncol = floor(size * h_ratio))

pop_matrix <- matrix(pop_raster$Pop_Dens,
                     nrow = floor(size * w_ratio),
                     ncol = floor(size * h_ratio))
# Define colours

# Name of Palette. Choices are: Archambault, Austria, Benedictus, Cassatt1, Cassatt2, Cross, 
#Degas, Demuth, Derain, Egypt, Gauguin, Greek, Hiroshige, Hokusai1, Hokusai2, Hokusai3, Homer1, Homer2, 
#Ingres, Isfahan1, Isfahan2, Java, Johnson,Juarez, Kandinsky, Klimt, Lakota, Manet, Monet, Moreau, 
#Morgenstern, Nattier, Navajo, NewKingdom, Nizami, OKeeffe1, OKeeffe2, Paquin, Peru1, Peru2, Pillement, 
#Pissaro, Redon, Renoir, Signac, Tam, Tara, Thomas, Tiepolo, Troy, Tsimshian, VanGogh1, VanGogh2, 
# VanGogh3, Veronese, and Wissing

color <- MetBrewer::met.brewer(name="OKeeffe1", direction = -1)
swatchplot(color)
tx <- grDevices::colorRampPalette(color, bias = 2.5)(256)
swatchplot(tx)

# Exclude colors
# Define the range of colors you want to exclude (for example, colors 5 to 10)
 exclude_range <- 1:2
 exclude_indices <- c(5)
# Create a subset of colors excluding the specified range
 subset_colors <- color[setdiff(seq_along(color), exclude_range)]
 subset_colors <- color[-exclude_indices]
#view colours in range
  swatchplot(subset_colors)
# blend range of colours
 tx_subset <- grDevices::colorRampPalette(subset_colors, bias = 1)(256)
 swatchplot(tx_subset)


# plotting 3D
# Close any existing 3D plot before plotting another
rgl::close3d()

pop_dens_change_matrix %>%
  height_shade(texture = tx) %>%
  plot_3d(heightmap = pop_matrix,
          zscale = 50,
          solid = F,
          shadowdepth = 0.8)

# Adjusting Camera Angle
render_camera(theta = 0,
              phi = 55,
              zoom = 0.45,
              fov = 100
)


outfile <- glue::glue("Plots/LCR_Pop.png")

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality(
    filename = outfile,
    interactive = TRUE,
    lightdirection = 55, #Degree
    lightaltitude = c(30, 80),
    lightcolor = c("white", "white"),  # Set both lights to white
    lightintensity = c(600, 100),
    # width = 1980,
    # height = 1180
    samples = 1000
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}
