#Ana Miller-ter Kuile
#December 12, 2024
#Ebird filtering to places with pinyon BA

#this script filters the SW ebird data to just be to areas
#whree there is pinyon basal area. The goal of this is to 
#create a species list which we can then draw from for models
#with multiple seed-eating birds

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "sf",  
                  "terra",
                  'readxl',
                  'sf',
                  'exactextractr',
                  'nngeo',
                  'sp',
                  'spatialEco',
                  'lubridate')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

set.seed(1)

# Load data ---------------------------------------------------------------

# load ebird --------------------------------------------------------------

ebird <- read.csv(here('data_clean',
                       'ebird_data',
                       'cleaned_data',
                       'all_ebird_data.csv'))

ebird %>%
  summarise(obs = sum(observation_count > 0, na.rm = T),
            total = n(),
            prop  = obs/total)
# Load spatial dataset ----------------------------------------------------

#for masking to
pinyonba_rast <- terra::rast(here('data',
                                  'spatial_data',
                                  'pinyonBA',
                                  'PinyonBA_4km_sqmPerHa.tif'))

pinyonba_df <- terra::as.data.frame(pinyonba_rast,
                                    xy = TRUE,
                                    cells = TRUE)

# Make spatial ------------------------------------------------------------

#get m radius for buffer.
#some are zero (stationary points)
#so maybe set a value for these, maybe 500m??
#get the others to be half the effort distance - assumption: walk to and
#from a vehicle
ebird2 <- ebird %>%
  rowwise() %>%
  mutate(buffer_m = (effort_distance_km*1000)/2) %>%
  ungroup()%>%
  mutate(buffer_m = case_when(buffer_m == 0 ~ 500,
                              TRUE ~ buffer_m))

#convert all ebird data to an sf object
ebird_spatial <- st_as_sf(ebird2, coords = c("longitude", "latitude"),
                          crs = st_crs(pinyonba_rast))

sw2 <- sw %>%
  st_transform(st_crs(pinyonba_rast))

# Extract cell IDs from raster ------------------------------------------------------------

##EBIRD
# Use template raster and ebird pts to get cell IDs
ebird_cellIDs <- terra::extract(pinyonba_rast, vect(ebird_spatial), cells = T)

# Add as column to ebird data
ebird_spatial$cellID <- ebird_cellIDs$cell

#some ebird observations don't have masting data cells, so I'll remove those
ebird_spatial <- ebird_spatial %>%
  filter(!is.na(cellID))
#this removes ~164 checklists
# Filter datasets for cells with cone data --------------------------------

ids <- pinyonba_df %>%
  distinct(cell)

#cells where all years == 0
pinyonba_all0 <- pinyonba_df %>%
  filter(if_all(.cols = c(PinyonBA_sqftPerAc_2010:PinyonBA_sqftPerAc_2022),
                ~. == 0))

all0 <- pinyonba_all0 %>%
  dplyr::select(cell)

ebird_spatial2 <- ebird_spatial %>%
  filter(cellID %in% ids$cell) %>% #1179455
  filter(!cellID %in% all0$cell) #462568

# Subset ebird data -------------------------------------------------------

#subsample some number based on the lowest 
#number in a year of checklists/grid cell

total_sum <- as.data.frame(ebird_spatial3) %>%
  group_by(year, cellID) %>%
  tally() %>%
  ungroup() 

total_sum %>%
  #ebird started in 2002, so look after that
  filter(year > 2001) %>%
  group_by(year) %>%
  summarise(mean = mean(n),
            sd = sd(n),
            total = n(),
            se = sd/sqrt(total)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#looks like there are more checklists per cell following
#2009 (2010 onward) ~4.2 checklists/cell then
#so we can use 2010 as our baseline 
#for filtering other years
# weekly_sum <- as.data.frame(ebird_spatial3) %>% 
#   mutate(week = week(observation_date)) %>%
#   group_by(year, week, cellID) %>% 
#   tally() %>%
#   ungroup()
# 
# #this ends up being ~same as just doing it yearly like below
# weekly_ss <- ebird_spatial3 %>% 
#   mutate(week = week(observation_date)) %>%
#   group_by(year, week, cellID) %>% 
#   #5 per week that we can then subsample to be 
#   #5 per cell in a year
#   slice_sample(n = 5) %>% 
#   ungroup() %>%
#   group_by(year, cellID) %>%
#   slice_sample(n = 5)
# 
# yearly_ss <- ebird_spatial3 %>%
#   mutate(week = week(observation_date)) %>%
#   group_by(year, cellID) %>%
#   slice_sample(n = 5)
#   
# weekly <- weekly_ss %>%
#   dplyr::select(week) %>%
#   mutate(type = 'weekly')
# 
# yearly <- yearly_ss %>%
#   dplyr::select(week) %>%
#   mutate(type = 'yearly')
# 
# df <- as.data.frame(weekly) %>%
#   bind_rows(as.data.frame(yearly)) 
# 
# ggplot(df, aes(x = week, fill = type)) +
#   geom_histogram(position = 'dodge') +
#   theme_bw()


# Get polygons around each bird point -------------------------------------

ebird_spatial3 %>%
  group_by(year, cellID) %>%
  slice_sample(n = 4) %>%
  ungroup() %>%
  filter(year > 2009) %>%
  group_by(year) %>%
  tally()

#doing a yearly subsample
#based on the average # checklists per grid
#for the lowest year (2010)
ebird_spatial4 <- ebird_spatial3 %>%
  group_by(year, cellID) %>%
  slice_sample(n = 4) %>%
  ungroup() %>%
  filter(year > 2009) %>%
  group_by(year) %>%
  #equal amt of data per year?
  #1715 in 2010,
  #average across years is 
  #7134 so maybe set to that???
  #the larger amt takes SO LONG to run
  #so going to try with a smaller amt.
  slice_sample(n = 1715) %>%
  ungroup()

# ebird_spatial4 %>%
#   group_by(year) %>%
#   tally() %>%
#   ungroup() %>%
#   summarise(mean = mean(n))
#give all ebird a buffer equal to sampling distance

#WOW This takes FOREVER WOWOWOWOW
#Create buffer zone:
ebird_buffer <- st_buffer(ebird_spatial4, ebird_spatial4$buffer_m)
#give all ebird a buffer equal to sampling distance

#dissolve the points within a cell to be one 
#big buffer "blob"
ebird_buffer2 <-  ebird_buffer %>%
  dplyr::select(cellID, year, geometry) %>%
  group_by(cellID, year) %>%
  summarise(buffer = st_union(geometry))

# Export ------------------------------------------------------------------

st_write(ebird_buffer2, here('data',
                             'ebird_data',
                             'cleaned_data',
                             'all_ebird_data_buffercellIDs.shp'))


#old code that takes one cell per observation
st_write(ebird_spatial4, here('data',
                      'ebird_data',
                      'cleaned_data',
                      'all_ebird_data_conefiltered.shp'))
