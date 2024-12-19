#Traits - filtering out 
#obligates/DoD species/seed eaters

# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

#species from ebird that occured in areas with some 
#pinyon basal area
species <- read.csv(here('data_clean',
                         'ebird_data',
                         'species_list.csv'))

traits <- read_delim(here('data_raw',
                        'trait_data',
                        'BirdFuncDat.txt'))  

traits2 <- read.csv(here(here('data_raw',
                              'trait_data',
                              'AVONET_eBIRD.csv')))


# Combine -----------------------------------------------------------------


species2 <- species %>% 
  left_join(traits2, by = c("SCIENTIFIC.NAME" = "Species2")) %>%
  filter(Order2 %in% c("Passeriformes", "Piciformes")) %>%
  dplyr::select(SCIENTIFIC.NAME, Order2, Family2, Trophic.Niche, Mass) %>%
  filter(Trophic.Niche %in% c("Granivore", "Invertivore",
                              "Omnivore"))


# How many are there? -----------------------------------------------------

species %>%
  filter(!str_detect(SCIENTIFIC.NAME, "sp.")) %>%
  filter(!str_detect(SCIENTIFIC.NAME, " x ")) %>%
  filter(!str_detect(SCIENTIFIC.NAME, "/")) %>%
  tally()
  
# Clean -------------------------------------------------------------------

#not big enough to eat pinyon 
#Leucosticte tephrocotis ~37 g
#	Sitta canadensis
#Myadestes townsendi - but eats juniper! ~33 g
#Melospiza melodia
#Pipilo maculatus - 39
#Sitta carolinensis
#Eremophila alpestris
#roughly < 40g, probs doesn't eat pinyon

#Mission Sensitive Species for DoD:
#Grey vireo  Vireo vicinior 12.8 g
#Bendire's Thrasher Toxostoma bendirei 62.2 g
#Burrowing Owl
#Pinyon Jay


#big seed eaters that could potentially eat pinyon
#or other things that pinyon jays eat (juniper, acorns)
#based on just some All About Birds exploration and a 
#general cutoff based on diets I was seeing there
seed_species <- species2 %>%
  filter(Mass >= 40) %>%
  filter(Trophic.Niche %in% c("Omnivore", "Granivore"))
#40 species

#small insectivores that may compete with grey vireo
insect_species <- species2 %>%
  filter(Trophic.Niche %in% c("Omnivore", "Invertivore")) %>%
  filter(Mass < 20)

# Export ------------------------------------------------------------------

write.csv(seed_species,
          here('data_clean',
               'ebird_data',
               'big_seed_eater_species_list.csv'))

write.csv(insect_species,
          here('data_clean',
               'ebird_data',
               'small_insect_eater_species_list.csv'))




