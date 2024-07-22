## NI postcode 
library(tidyverse)
library(sf)
ni_pcd <- 'data/ukpostcodes.csv' %>% read_csv


## all NI postcodes begin with BT


ni_pcd <- 
  ni_pcd %>%
  filter(
    (postcode %>% substr(1,2)) == 'BT' 
  )

### Input Sure Start address by Centre 

ss_pcd_pre2006 <- 
  list(
    `Ballymena South Sure Start`  = 'BT42 4DN',
    `Coleraine Sure Start` = 'BT52 2QR',
    `Dalriada Sure Start` = 'BT57 8QD',
    `G-Old Community Sure Start` = 'BT80 8NH',
    `Abbey Sure Start` = 'BT37 9DQ',
    `Blossom Sure Start` = 'BT62 1DS',
    `Clogher Valley Sure Start` = 'BT77 0BG',
    `Dungannon Sure Start` = 'BT70 1BS',
#    Orana Sure Start (currently called' Newry)',
    `Orana Sure Start` = 'BT34 2PF',
    `Cherish Sure Start` = 'BT94 1HG',
    # Dungiven Sure Start (currently Dry' Arch)',
    `Dungiven Sure Start` = 'BT47 4QL',
    `Last Sure Start` = 'BT79 7XL',
# Crevagh / Springtown Sure Start (currently little hands sure' start)',
    `Little Hands Sure Start` =  'BT48 0PZ',
    `Shantallow Sure Start` = 'BT48 8HJ',
    `Strabane Sure Start` = 'BT82 9BT',
    `Colin Neighbourhood Sure Start` = 'BT17 OGD',
    `Downpatrick Sure Start` = 'BT30 6NE',
    `Lower Ards Sure Start` = 'BT22 1EB',
    `Clan Mór Sure Start` = 'BT12 4HL',
    `Inner City East Belfast Sure Start (East belfast)` = 'BT5 4GS',
    `Glenbrook Sure Start` = 'BT14 7EJ',
    `SMILE Sure Start` = 'BT15 2GN',
    `Shankill Sure Start` = 'BT13 2BB',
    `Inner City South Belfast Sure Start` = 'BT7 1NR'
)

## make into data.frame
ss_pcd_pre2006 <-
  ss_pcd_pre2006 %>% map_df(
    .f = function(x)
      data.frame(postcode = x),
    .id = 'sure start centre'
  )

ss_pcd_pre2006 <- 
  ss_pcd_pre2006 %>%
  mutate(opened = '<2006')

### QA --------------

ss_pcd_pre2006 <-
  ss_pcd_pre2006 %>%
  left_join(
    ni_pcd
  )
### all there except Colin neighbourhod -- 
## manual look up
# https://www.doogal.co.uk/ShowMap?postcode=BT17%200TD
# lat: 54.552252 Longitude 	-6.029012
ss_pcd_pre2006 <- 
  ss_pcd_pre2006 %>%
  mutate(
    latitude  =
      ifelse(`sure start centre` == 'Colin Neighbourhood Sure Start', 54.552252, latitude),
    longitude =
      ifelse(`sure start centre` == 'Colin Neighbourhood Sure Start', -6.029012, longitude)
  )
  



# add post 2005 centres ---------------------------------------------------
## WIP - based on DE there should be 9 of these 

ss_pcd_post2005 <- 
  list(
    `Horizon Sure Start` =    'BT38 8JJ',
    `South Armagh Sure Start` =    'BT35 7JG',
    `Splash Sure Start` =    'BT65 5BE'
  )


## make into data.frame
ss_pcd_post2005 <-
  ss_pcd_post2005 %>% map_df(
    .f = function(x)
      data.frame(postcode = x),
    .id = 'sure start centre'
  )

ss_pcd_post2005 <- 
  ss_pcd_post2005 %>%
  mutate(opened = '=>2006')



### QA -------------------------
ss_pcd_post2005 <- ss_pcd_post2005 %>% left_join(ni_pcd)
ss_pcd_post2005


# merge list and turn to geojson ------------------------------------------

ss_centres_df <-
  ss_pcd_pre2006 %>%
  bind_rows(ss_pcd_post2005)
ss_centres_df  

ss_centres_sf <-
  ss_centres_df %>%
  st_as_sf(
    crs = st_crs(4326), ##wgs 84
    coords = c('longitude', 'latitude')
  )


## Save data as geojson
ss_centres_sf <- 
  sf::st_write(
    ss_centres_sf, 
    dsn = 'data/sure start centres.geojson',
    append = F
  )



