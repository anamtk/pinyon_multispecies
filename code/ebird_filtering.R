# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 
                  "auk", 'lubridate')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Extract using auk -------------------------------------------------------

#FOllowing protocol here:
#https://cornelllabofornithology.github.io/ebird-best-practices/ebird.html

# Colorado ----------------------------------------------------------------


ebd_co <- auk_ebd(file = here('data_raw',
                              'bird_data',
                              'ebd_US-CO_201001_202401_relOct-2024',
                              'ebd_US-CO_201001_202401_relOct-2024.txt'))

#define the filters I will want
ebd_filters <- ebd_co %>%
  #filtering nesting season for many birds
  auk_date(date = c("*-02-15", "*-09-15")) %>%
#restirct to the stationary and traveling protocols
  auk_protocol(protocol = c("Stationary", "Traveling")) %>%
  #less than 5 h
  auk_duration(duration = c(0, 300)) %>%
  # less than 5km distance traveled
  auk_distance(distance = c(0, 5)) %>%
  #less than 10 observers
  #not sure how to do this one...
  #only use complete checklists
  auk_complete()

f_ebd_co <- file.path(here('data_clean',
                           'ebird_data',
                           'ebd_co_filtered.txt'))

# only run if the files don't already exist
if (!file.exists(f_ebd_co)) {
  auk_filter(ebd_filters, file = f_ebd_co)
}

# New Mexico --------------------------------------------------------------

ebd_nm <- auk_ebd(file = here('data_raw',
                              'bird_data',
                              'ebd_US-NM_201001_202401_relOct-2024',
                              'ebd_US-NM_201001_202401_relOct-2024.txt'))

#define the filters I will want
ebd_filters <- ebd_nm %>%
  #filtering nesting season for many birds
  auk_date(date = c("*-02-15", "*-09-15")) %>%
  #restirct to the stationary and traveling protocols
  auk_protocol(protocol = c("Stationary", "Traveling")) %>%
  #less than 5 h
  auk_duration(duration = c(0, 300)) %>%
  # less than 5km distance traveled
  auk_distance(distance = c(0, 5)) %>%
  #less than 10 observers
  #not sure how to do this one...
  #only use complete checklists
  auk_complete()

f_ebd_nm <- file.path(here('data_clean',
                           'ebird_data',
                           'ebd_nm_filtered.txt'))

# only run if the files don't already exist
if (!file.exists(f_ebd_nm)) {
  auk_filter(ebd_filters, file = f_ebd_nm)
}


# Arizona -----------------------------------------------------------------

ebd_az <- auk_ebd(file = here('data_raw',
                              'bird_data',
                              'ebd_US-AZ_201001_202401_relOct-2024',
                              'ebd_US-AZ_201001_202401_relOct-2024.txt'))

#define the filters I will want
ebd_filters <- ebd_az %>%
  #filtering nesting season for many birds
  auk_date(date = c("*-02-15", "*-09-15")) %>%
  #restirct to the stationary and traveling protocols
  auk_protocol(protocol = c("Stationary", "Traveling")) %>%
  #less than 5 h
  auk_duration(duration = c(0, 300)) %>%
  # less than 5km distance traveled
  auk_distance(distance = c(0, 5)) %>%
  #less than 10 observers
  #not sure how to do this one...
  #only use complete checklists
  auk_complete()

f_ebd_az <- file.path(here('data_clean',
                           'ebird_data',
                           'ebd_az_filtered.txt'))

# only run if the files don't already exist
if (!file.exists(f_ebd_az)) {
  auk_filter(ebd_filters, file = f_ebd_az)
}


# UTah --------------------------------------------------------------------

ebd_ut <- auk_ebd(file = here('data_raw',
                              'bird_data',
                              'ebd_US-UT_201001_202401_relOct-2024',
                              'ebd_US-UT_201001_202401_relOct-2024.txt'))

#define the filters I will want
ebd_filters <- ebd_ut %>%
  #filtering nesting season for many birds
  auk_date(date = c("*-02-15", "*-09-15")) %>%
  #restirct to the stationary and traveling protocols
  auk_protocol(protocol = c("Stationary", "Traveling")) %>%
  #less than 5 h
  auk_duration(duration = c(0, 300)) %>%
  # less than 5km distance traveled
  auk_distance(distance = c(0, 5)) %>%
  #less than 10 observers
  #not sure how to do this one...
  #only use complete checklists
  auk_complete()

f_ebd_ut <- file.path(here('data_clean',
                           'ebird_data',
                           'ebd_ut_filtered.txt'))

# only run if the files don't already exist
if (!file.exists(f_ebd_ut)) {
  auk_filter(ebd_filters, file = f_ebd_ut)
}

# Combine -----------------------------------------------------------------
f_ebd_co <- read_delim(file = here('data_clean',
                                   'ebird_data',
                                   'ebd_co_filtered.txt'))

ebd_all <- f_ebd_co %>%
  rbind(f_ebd_nm,
        f_ebd_az,
        f_ebd_ut)

# Extra filtering/cleaining -----------------------------------------------

# function to convert time observation to hours since midnight
time_to_decimal <- function(x) {
  x <- hms(x, quiet = TRUE)
  hour(x) + minute(x) / 60 + second(x) / 3600
}

# clean up variables
ebd_zf <- ebd_all %>% 
  mutate(
    # convert X to NA
    observation_count = if_else(observation_count == "X", 
                                NA_character_, observation_count),
    observation_count = as.integer(observation_count),
    # effort_distance_km to 0 for non-travelling counts
    effort_distance_km = if_else(protocol_type != "Traveling", 
                                 0, effort_distance_km),
    # convert time to decimal hours since midnight
    time_observations_started = time_to_decimal(time_observations_started),
    # split date into year and day of year
    year = year(observation_date),
    day_of_year = yday(observation_date)
  )

# additional filtering
ebd_zf_filtered <- ebd_zf %>% 
  filter(
    # effort filters
    duration_minutes <= 5 * 60,
    effort_distance_km <= 5,
    # 10 or fewer observers
    number_observers <= 10)


# Select only variables of interest and expoort ---------------------------

ebird <- ebd_zf_filtered %>% 
  select(checklist_id, observer_id, sampling_event_identifier,
         scientific_name,
         observation_count, species_observed, 
         state_code, locality_id, latitude, longitude,
         protocol_type, all_species_reported,
         observation_date, year, day_of_year,
         time_observations_started, 
         duration_minutes, effort_distance_km,
         number_observers)

write.csv(ebird, here('data_clean',
               'ebird_data',
               'all_ebird_data.csv'))

