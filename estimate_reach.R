library(sf)
library(terra)
library(dplyr)
library(here)
library(mapview)
library(openxlsx)
library(stringr)
library(raster)


# Set-up ----
options(warn = -1)  # Suppress warnings

# Set projection
proj <- "+proj=sinu +lat_0=0 +lon_0=25 +lat_1=20 +lat_2=-23 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +type=crs"

#--------------
# Read data
#--------------
# Read population raster and reproject it ----
print("Reprojecting population raster...")
population_raster <- raster(list.files(here("population"), full.names = T, pattern = "*.tif$")[1])
population_raster <- projectRaster(population_raster, crs= proj)

filepath = 'cloudrf/fem_kano/'
gpkg_files <- list.files(path = filepath, pattern = "\\.gpkg$", full.names = TRUE, recursive = F)

# Read Nigeria boundaries ----
boundaries <- st_read("boundaries/nga_admbnda_adm1_osgof_20190417.shp")
boundaries <- st_transform(boundaries, crs= proj)

# Read Kano stations ----
arewa <- gpkg_files[9]
freedom <- gpkg_files[10]

# read raster, replace 0s with NA, dissolve/unionize/aggregate
arewa_polygon <- st_read(arewa)
freedom_polygon <- st_read(freedom)

# add column to trace file
arewa_polygon$source_file <- basename(arewa)
freedom_polygon$source_file <- basename(freedom)

# Reproject station
arewa_polygon <- st_transform(arewa_polygon, proj)
freedom_polygon <- st_transform(freedom_polygon, proj)


# ------------------
# Geometric operations - Arewa----
# ------------------

# Which boundaries does arewa touch?
arewa_states <- st_intersection(boundaries, arewa_polygon)
arewa_states %>% distinct(ADM1_EN)


# Set-up table for export ----
population_data <- tibble(source_file = character(),
                          state_population = numeric(),
                          population_coverage = numeric(),
                          state = character()
)

# Loop through each touched state
for (i in seq_len(nrow(arewa_states))) {
  state_name <- arewa_states$ADM1_EN[i]
  state_geom <- boundaries %>% filter(ADM1_EN == state_name)
  
  # Get total population in state
  total_pop <- exactextractr::exact_extract(population_raster, state_geom,
                                            fun = function(values, coverage_fractions) {
                                              sum(values * coverage_fractions, na.rm = TRUE)
                                            })
  
  # Intersect Arewa polygon with the state
  arewa_in_state <- st_intersection(state_geom, arewa_polygon)
  
  # Crop population raster to Arewa-in-state area
  pop_arewa <- exactextractr::exact_extract(population_raster, arewa_in_state,
                                            fun = function(values, coverage_fractions) {
                                              sum(values * coverage_fractions, na.rm = TRUE)
                                            })
  
  # Calculate coverage
  coverage <- pop_arewa / total_pop
  
  # Append to table
  population_data <- population_data %>%
    add_row(source_file = "arewa_polygon",
            state_population = pop_arewa,
            population_coverage = coverage,
            state = state_name)
}


#-------------------
# Summarize ----
#-------------------
proportion_wra = 0.225
proportion_radio_owners = 0.437

# calculate the number of women of reproductive age (WRA)
population_data <- population_data %>%
  mutate(
    wra = state_population * proportion_wra * proportion_radio_owners
  ) 

# summarise to show total population overall
population_data %>%
  summarise(
    wra = sum(wra)
  )

#-------------------
# Export ----
#-------------------
write.csv(population_data, file = "output/arewa_population.csv")


# ------------------
# Geometric operations - Freedom ----
# ------------------

# Which boundaries does freedom touch?
freedom_states <- st_intersection(boundaries, freedom_polygon)
freedom_states %>% distinct(ADM1_EN)


# Set-up table for export ----
population_data <- tibble(source_file = character(),
                          state_population = numeric(),
                          population_coverage = numeric(),
                          state = character()
)

# Loop through each touched state
for (i in seq_len(nrow(freedom_states))) {
  state_name <- freedom_states$ADM1_EN[i]
  state_geom <- boundaries %>% filter(ADM1_EN == state_name)
  
  # Get total population in state
  total_pop <- exactextractr::exact_extract(population_raster, state_geom,
                                            fun = function(values, coverage_fractions) {
                                              sum(values * coverage_fractions, na.rm = TRUE)
                                            })
  
  # Intersect Freedom polygon with the state
  freedom_in_state <- st_intersection(state_geom, freedom_polygon)
  
  # Crop population raster to Freedom-in-state area
  pop_freedom <- exactextractr::exact_extract(population_raster, freedom_in_state,
                                            fun = function(values, coverage_fractions) {
                                              sum(values * coverage_fractions, na.rm = TRUE)
                                            })
  
  # Calculate coverage
  coverage <- pop_freedom / total_pop
  
  # Append to table
  population_data <- population_data %>%
    add_row(source_file = "freedom_polygon",
            state_population = pop_freedom,
            population_coverage = coverage,
            state = state_name)
}


###############
# Summarize ----
###############
proportion_wra = 0.225
proportion_radio_owners = 0.437

# calculate the number of women of reproductive age (WRA)
population_data <- population_data %>%
  mutate(
    wra = state_population * proportion_wra * proportion_radio_owners
  ) 

# summarise to show total population overall
population_data %>%
  summarise(
    wra = sum(wra)
  )

###############
# Export ----
###############
write.csv(population_data, file = "output/freedom_population.csv")


###############
# Visualize ----
###############
library(mapview)
# mapview(hf_buffer, col.regions = "blue") +
  mapview(freedom_states, col.regions = "red") +
    mapview(boundaries, col.regions = "green")
