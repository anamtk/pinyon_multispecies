# Load packages -----------------------------------------------------------

package.list <- c("dplyr", 'tibble',
                  'lubridate',
                  'stringr',
                  'magrittr', 'tidyr',
                  'ggplot2',
                  "sf",  
                  "terra")

## Installing them if they aren't already on the computer
#new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

# Combine -----------------------------------------------------------------
ebd_co <- read.delim(file = '/scratch/atm234/pinyon_multispecies/ebird_filtering/inputs/ebd_co_filtered.txt' )
ebd_nm <- read.delim(file = '/scratch/atm234/pinyon_multispecies/ebird_filtering/inputs/ebd_nm_filtered.txt' )
ebd_ut <- read.delim(file ='/scratch/atm234/pinyon_multispecies/ebird_filtering/inputs/ebd_ut_filtered.txt')
ebd_az <- read.delim(file = '/scratch/atm234/pinyon_multispecies/ebird_filtering/inputs/ebd_az_filtered.txt')

ebd_co <- as.data.frame(ebd_co) %>%
    mutate(TAXONOMIC.ORDER = as.character(TAXONOMIC.ORDER))
    
colnames(ebd_co)
 
ebd_nm <- as.data.frame(ebd_nm) %>%
    mutate(TAXONOMIC.ORDER = as.character(TAXONOMIC.ORDER))
    
colnames(ebd_nm)

ebd_ut <- as.data.frame(ebd_ut) %>%
    mutate(TAXONOMIC.ORDER = as.character(TAXONOMIC.ORDER))
    
colnames(ebd_ut)
    
ebd_az <- as.data.frame(ebd_az) %>%
    mutate(TAXONOMIC.ORDER = as.character(TAXONOMIC.ORDER))
    
colnames(ebd_az)


ebd_all <- bind_rows(ebd_co, ebd_nm,
                     ebd_ut, ebd_az)


# Extra filtering/cleaining -----------------------------------------------

# function to convert time observation to hours since midnight
#time_to_decimal <- function(x) {
#  x <- hms(x, quiet = TRUE)
#  hour(x) + minute(x) / 60 + second(x) / 3600
#}

# [1] "GLOBAL.UNIQUE.IDENTIFIER"   "LAST.EDITED.DATE"          
# [3] "TAXONOMIC.ORDER"            "CATEGORY"                  
# [5] "TAXON.CONCEPT.ID"           "COMMON.NAME"               
# [7] "SCIENTIFIC.NAME"            "SUBSPECIES.COMMON.NAME"    
 #[9] "SUBSPECIES.SCIENTIFIC.NAME" "EXOTIC.CODE"               
#[11] "OBSERVATION.COUNT"          "BREEDING.CODE"             
#[13] "BREEDING.CATEGORY"          "BEHAVIOR.CODE"             
#[15] "AGE.SEX"                    "COUNTRY"                   
#[17] "COUNTRY.CODE"               "STATE"                     
#[19] "STATE.CODE"                 "COUNTY"                    
#[#21] "COUNTY.CODE"                "IBA.CODE"                  
#[23] "BCR.CODE"                   "USFWS.CODE"                
#[25] "ATLAS.BLOCK"                "LOCALITY"                  
#[27] "LOCALITY.ID"                "LOCALITY.TYPE"             
#[29] "LATITUDE"                   "LONGITUDE"                 
#[31] "OBSERVATION.DATE"           "TIME.OBSERVATIONS.STARTED" 
#[33] "OBSERVER.ID"                "SAMPLING.EVENT.IDENTIFIER" 
#[35] "PROTOCOL.TYPE"              "PROTOCOL.CODE"             
#[37] "PROJECT.CODE"               "DURATION.MINUTES"          
#[39] "EFFORT.DISTANCE.KM"         "EFFORT.AREA.HA"            
#[41] "NUMBER.OBSERVERS"           "ALL.SPECIES.REPORTED"      
#[43] "GROUP.IDENTIFIER"           "HAS.MEDIA"                 
#[45] "APPROVED"                   "REVIEWED"                  
#[47] "REASON"                     "TRIP.COMMENTS"             
#[49] "SPECIES.COMMENTS"           "X"   

# clean up variables
ebd_zf <- ebd_all %>% 
  mutate(
    # convert X to NA
    OBSERVATION.COUNT = if_else(OBSERVATION.COUNT == "X", 
                                NA_character_, OBSERVATION.COUNT),
    OBSERVATION.COUNT = as.integer(OBSERVATION.COUNT),
    # effort_distance_km to 0 for non-travelling counts
    EFFORT.DISTANCE.KM = if_else(PROTOCOL.TYPE != "Traveling", 
                                 0, EFFORT.DISTANCE.KM))#,
    # convert time to decimal hours since midnight
    #time_observations_started = time_to_decimal(time_observations_started),
    # split date into year and day of year
    #year = year(OBSERVATION.DATE),
    #day_of_year = yday(OBSERVATION.DATE)
  #)

# additional filtering
ebd_zf_filtered <- ebd_zf %>% 
  dplyr::filter(
    # effort filters
    DURATION.MINUTES <= 5 * 60,
    EFFORT.DISTANCE.KM <= 5,
    # 10 or fewer observers
    NUMBER.OBSERVERS <= 10)


# Select only variables of interest and expoort ---------------------------

ebird <- ebd_zf_filtered %>% 
  select(GLOBAL.UNIQUE.IDENTIFIER,  COMMON.NAME,              
  SCIENTIFIC.NAME, OBSERVATION.COUNT,
  STATE, COUNTY,           
  LATITUDE, LONGITUDE,
  OBSERVATION.DATE, TIME.OBSERVATIONS.STARTED, 
  OBSERVER.ID, PROTOCOL.TYPE, DURATION.MINUTES,         
  EFFORT.DISTANCE.KM, NUMBER.OBSERVERS)

#write.csv(ebird, '/scratch/atm234/pinyon_multispecies/ebird_filtering/outputs/all_ebird.csv')

# Load spatial dataset ----------------------------------------------------

#for masking to
pinyonba_rast <- terra::rast('/scratch/atm234/pinyon_multispecies/ebird_filtering/inputs/PinyonBA_4km_sqmPerHa.tif')

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
  mutate(buffer_m = (EFFORT.DISTANCE.KM*1000)/2) %>%
  ungroup()%>%
  mutate(buffer_m = case_when(buffer_m == 0 ~ 500,
                              TRUE ~ buffer_m))

#convert all ebird data to an sf object
ebird_spatial <- st_as_sf(ebird2, coords = c("LONGITUDE", "LATITUDE"),
                          crs = st_crs(pinyonba_rast))

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

write.csv(ebird_spatial2, '/scratch/atm234/pinyon_multispecies/ebird_filtering/outputs/species_occurence.csv')

# Get species list --------------------------------------------------------

species <- as.data.frame(ebird_spatial2) %>%
  filter(OBSERVATION.COUNT > 0) %>%
  distinct(SCIENTIFIC.NAME)

write.csv(species, '/scratch/atm234/pinyon_multispecies/ebird_filtering/outputs/species_list.csv')


