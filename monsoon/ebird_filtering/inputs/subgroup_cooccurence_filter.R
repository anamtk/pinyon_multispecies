# Load packages -----------------------------------------------------------

package.list <- c("dplyr", 'tibble',
                  'stringr',
                  'magrittr', 'tidyr',
                  'ggplot2','purrr')

## Installing them if they aren't already on the computer
#new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

seedeaters <- read.csv('/scratch/atm234/pinyon_multispecies/ebird_filtering/inputs/big_seed_eater_species_list.csv')
  
bugeaters <- read.csv('/scratch/atm234/pinyon_multispecies/ebird_filtering/inputs/small_insect_eater_species_list.csv')

allobs <- read.csv('/scratch/atm234/pinyon_multispecies/ebird_filtering/inputs/species_occurence.csv')

str(allobs)

str(seedeaters)

str(bugeaters)

# Filter observations in groups -------------------------------------------

seedeaters$SCIENTIFIC.NAME

bugeaters$SCIENTIFIC.NAME

seedeaters2 <- allobs %>%
  filter(SCIENTIFIC.NAME %in% seedeaters$SCIENTIFIC.NAME)

bugeaters2 <- allobs %>%
  filter(SCIENTIFIC.NAME %in% bugeaters$SCIENTIFIC.NAME)


# Export ------------------------------------------------------------------

write.csv(seedeaters2, 
          '/scratch/atm234/pinyon_multispecies/ebird_filtering/outputs/big_seed_eaters_species_occurence.csv')

write.csv(bugeaters2, 
          '/scratch/atm234/pinyon_multispecies/ebird_filtering/outputs/small_bug_eaters_species_occurence.csv')




