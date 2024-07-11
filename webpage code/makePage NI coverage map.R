## makePage -- whole NI map

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

## lookup
osni_lookup_df <-
  'data/osni (wards) to ward code lookup.csv' %>%
  read_csv()


## Correct some names -----------------------------------------------
wards_sf <-
  wards_sf %>%
  mutate(
    osni_ward_lgd = paste(WARDS, LGD),
    wards_lower = WARDS %>% tolower %>% gsub(x=., ",", '')
  )

wards_sf <-
  wards_sf %>%
  left_join(osni_lookup_df)

wards_sf %>%
  filter(ward_code %>% is.na)

### QA check joins  -------------------------------------------------

sure_start_2009$bestMatch %>% duplicated()
sure_start_2009$bestMatch %>% table() %>% sort()
## why are there duplicated? -- due to data entry -- no real errors

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

map_me <- wards_sf %>% left_join(sure_start_2009, by = c(ward_code = 'bestCode'))
map_me$bestMatch %>% is.na %>% summary

map_me <-
  map_me %>%
  select( `sure start centre`, WARDS)

## map
tmap_mode('view')
map_me %>%
  filter(
    !(`sure start centre` %>% is.na)
    ) %>% 
  tm_shape() +
  tm_borders(col = 'blue') +
  tm_fill(
    col = 'sure start centre', legend.show = F,
    popup.vars=c("Centre"="sure start centre", "Ward"="WARDS"),
    alpha = 0.3) +
#  tm_text('WARDS', col = 'blue', size = 0.7) +
  tm_scale_bar() + 
  tm_layout(title = 'Wards covered by Sure Start in 2009 (based on former Sure Start website)') #+
#  tm_basemap("OpenStreetMap") 
