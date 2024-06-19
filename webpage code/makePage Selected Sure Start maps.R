## makePage 

library(tidyverse)
library(sf)
library(tmap)


## input /output
wards_sf <-
  'data/OSNI_Open_Data_-_50K_Boundaries_-_Wards_(1993).geojson' %>%
  #'data/OSNI_Open_Data_-_Largescale_Boundaries_-_Wards_(1993).geojson' %>%
   st_read


## Sure start 

sure_start_2009 <-
  'data/national archive to nimdm wards (checked).csv' %>%
  read_csv




## amend some names
#sure_start_2009 <- 


## Correct some names -----------------------------------------------
wards_sf <-
  wards_sf %>%
  mutate(
    wards_lower = WARDS %>% tolower %>% gsub(x=., ",", '')
  )

sure_start_2009 <-
  sure_start_2009 %>%
  mutate(
    wards_lower = bestMatch %>% tolower()
  )


### QA check joins  -------------------------------------------------
sure_start_2009 %>%
  filter(
    !( wards_lower %in% wards_sf$wards_lower )
  )

## take the apostrophe out of audley's acre
sure_start_2009 <-
  sure_start_2009 %>%
  mutate(
    wards_lower = wards_lower %>% gsub(x=., "audley's", 'audleys')
  )
  
map_me <- wards_sf %>% left_join(sure_start_2009)

map_me <-
  map_me %>%
  select( `sure start centre`, WARDS)

## map
tmap_mode('view')
map_me %>%
  filter(`sure start centre` == 'Blossom Sure Start') %>% 
  tm_shape() +
  tm_borders(col = 'blue') +
  tm_fill(alpha = 0.3) +
  tm_text('WARDS', col = 'blue', size = 0.7) +
  tm_layout(title = 'Wards covered by Blossom Sure Start in 2009 (based on former Sure Start website)') +
  tm_basemap("OpenStreetMap") 

?tm_basemap
?tm_layout

?tm_