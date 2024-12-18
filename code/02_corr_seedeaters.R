#Co-occurence of large seed eaters

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse",
                  'corrplot', 'lubridate')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

data <- read.csv(here('data_clean',
                      'ebird_data',
                      'big_seed_eaters_species_occurence.csv'))

names(data)[1:(ncol(data)-1)] = names(data)[2:(ncol(data))]

data2 <- data %>%
  distinct(COMMON.NAME, SCIENTIFIC.NAME,
           OBSERVATION.COUNT, STATE,
           OBSERVATION.DATE, TIME.OBSERVATIONS.STARTED,
           OBSERVER.ID, 
           EFFORT.DISTANCE.KM, DURATION.MINUTES, cellID) %>%
  mutate(year = as.numeric(str_sub(OBSERVATION.DATE, 1, 4))) %>%
  group_by(STATE,
           year, COMMON.NAME, SCIENTIFIC.NAME, cellID) %>%
  summarise(total = sum(OBSERVATION.COUNT, na.rm = T)) %>%
  ungroup() %>%
  group_by(STATE, year, cellID) %>%
  mutate(id = cur_group_id()) %>%
  ungroup()

species_matrix <- data2 %>%
  dplyr::select(COMMON.NAME, total, id) %>%
  pivot_wider(names_from = "COMMON.NAME",
              values_from = "total",
              values_fill = 0) %>%
  column_to_rownames(var = "id")

corsp <- cor(species_matrix)

corrplot(cor(species_matrix),
         type = 'lower',
         diag = F)
# subplot -----------------------------------------------------------------

#pinyon jays top co-occurence speices
#Canyon Towhee
#Woodhouse's Scrub-Jay
#Common Raven
#Stellar's Jay
#Clark's Nutcracker
#Acorn Woodpecker
jaymat <- data2 %>%
  filter(COMMON.NAME %in% c("Pinyon Jay", "Canyon Towhee",
                            "Woodhouse's Scrub-Jay",
                            "Common Raven",
                            "Stellar's Jay",
                            "Clark's Nutcracker", 
                            "Acorn Woodpecker")) %>%
  dplyr::select(COMMON.NAME, total, id) %>%
  pivot_wider(names_from = "COMMON.NAME",
              values_from = "total",
              values_fill = 0) %>%
  column_to_rownames(var = "id")

corrplot(cor(jaymat),
         type = 'lower',
         diag = F,
         tl.col = "black")
