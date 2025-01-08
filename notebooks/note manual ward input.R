## note: manual entry and data check for Sure Start
## Goal: enter the Sure Start wards and compare to nearest fit then manual checks


## Source is the NI Sure Start list (most record ward data from March 2009)
## https://webarchive.nationalarchives.gov.uk/ukgwa/20081105164100/http://www.surestart.gov.uk/aboutsurestart/help/contacts/northernireland/
library(tidyverse)
library(RecordLinkage)

### input/ output

## Ward list from here 
wards_df <- 
  readxl::read_xls('data/NIMDM_2010_Results_Ward_0.xls', sheet = 2)

## the checking function
## takes characters and output closest 
check_names_char <-
  function(checkThis, checkList, nClosestToShow = 2){
    ## lowercase
    lower_x = checkThis %>% tolower()
    lower_checkList = checkList %>% tolower()
    
    dist = levenshteinSim(lower_x, str2 = lower_checkList)
    data.frame(
      checkThis = checkThis,
      #        perfectMatch = tolower(x) %in% tolower(checkList),
      bestMatch = checkList[which.max(dist)],
      bestMatchId = which.max(dist),
      bestScore = max(dist), 
      others = checkList[(dist %>% order(decreasing = T))[2:(1 + nClosestToShow)]] %>% paste(collapse = "|") #next two best        
    )
    
    
  }
  
# check_names_char('dave', c(letters[1:26])) ## Don't run example


# step 1: manual data entry -----------------------------------------------


ss <- list()


ss$`G-Old Community Sure Start` <-
  c('Gortalowry', 'Oldtown', 'Killymoon', 'Ardboe')
ss$`Dalriada Sure Start` <-
  c(
    'Armoy',
    'Bushmills',
    'Ballylough',
    'Mosside',
    'Moyarget',
    'Dalriada',
    'Kinbane',
    'Glentaise',
    'Dunserverick',
    'Knocklayd',
    'Bonamargy and Rathlin'
  )

ss$`Coleraine Sure Start` <-
  c('Ballysally',
    'Central',
    'Churchlands',
    'CrossGlebe',
    'Knocklyn and University')

ss$`Ballymena South Sure Start` <-
  c(
    'Ballykeel',
    'Ballee',
    'Moat',
    'Harryville',
    'Dunclag',
    'Fairgreen',
    'Castle Demesne',
    'Summerfield'
  )

ss$`Abbey Sure Start` <-
  c('Abbey',
    'Cloughfern',
    'Coole',
    'Dunanney',
    'Monkstown',
    'Valley and Whitehouse')

ss$`Horizon Sure Start` <-
  c(
    'Northlands',
    'Sunnylands',
    'Clipperstown',
    'Love Lane',
    'Antiville',
    'Ballyloran and Craigyhill'
  )


ss$`Dungannon Sure Start` <- c(
  'Ballysaggart',
  'Benburb',
  'Coalisland South',
  'Coolhill',
  'Drumglass',
  'Killymeal',
  'Moygashel',
  'Mullaghmore',
  'Castlecaulfield'
)

ss$`Clogher Valley Sure Start` <-
  c('Anghnacloy',
    'Augher',
    'Ballygawley',
    'Clogher',
    'Fivemiletown')

ss$`South Armagh Sure Start` <-
  c('Bessbrook',
    'Carnlough', # Mispelling of Camlough? Carnlough is a village in Antrim (opposite end of NI)
    'Creggan',
    'Crossmaglen',
    'Derrymore',
    'Newtownhamilton')

ss$`Orana Sure Start` <- c(
  'Ballybot',
  'Daisyhill',
  'Drumalane',
  'Drumgullion',
  'St. Marys',
  'St. Patricks',
  'Windsor Hill'
)

ss$`Blossom Sure Start` <-
  c('Annagh',
    'Ballybay',
    'Ballyoran',
    'Brownstown',
    'Cocrain',
    'Tavanagh')

ss$`Splash Sure Start` <-
  c('Church',
    'Court',
    'Drumgas',
    'Drumgor',
    'Drumnamoe',
    'Taghnevan')

## `Western`
  
ss$`Strabane Sure Start` <- c(
  'North',
  'South',
  'East',
  'West',
  'Ballycolman',
  'Sion Mills',
  'Finn',
  'Dunnamanagh',
  'Plumbridge'
)


ss$`Little Hands Sure Start` <-
  c('Creevagh', 'Springtown', 'Rosemount')

ss$`Shantallow Sure Start` <-
  c('Shantallow East', 'Shantallow West', 'Carnhill', 'Culmore Area')

ss$`Cherish Sure Start` <- 
  c(
  'Irvinestown',
  'Kesh',
  'Ederney and Lack',
  'Lisnarrick',
  'Ballinamallard',
  'Trillick',
  'Devenish',
  'Rosslea',
  'Newtownbutler'
)

ss$`Dungiven Sure Start` <-
  c(
    'The Highlands',
    'Dungiven',
    'Feeny',
    'Upper Glenshane',
    'Glack',
    'Coolessan',
    'Greystone'
  )

ss$`Last Sure Start` <- c('Lisanelly',
                          'Drumragh',
                          'Killyclogher',
                          'Camowen',
                          'Strule',
                          'Fintona',
                          'Termon')

ss$`Edenballymore Sure Start`<- c(
  'Brandywell',
  'The Diamond',
  'Westland',
  'Strand',
  'Beechwood',
  'Creggan Central',
  'Creggan South'
)

ss$`Rainbow Sure Start` <-
  c('Castlederg',
    'Glenderg',
    'Clare',
    'Drumquin',
    'Newtownstewart')

ss$`Waterside Sure Start` <- c('Victoria', 'Ebrington', 'Clondermott', 'Enagh')


## Eastern

ss$`Glenbrook Sure Start` <-
  c('Ardoyne', 'Cliftonville', 'Ligoniel') ## This ends in a , but no further values on webpage

ss$`East Belfast Sure Start` <-
  c('Island',
    'The Mount',
    'Ballymacarett Woodstock',
    'Enler',
    'Tullycarnet')

ss$`Colin Neighbourhood Sure Start` <-
  c('Twinbrook',
    'Poleglass',
    'Colin Glen',
    'Old Warren',
    'Kilwee',
    'Lagmore (Derriaghy)')

ss$`Lower Ards Peninsula Sure Start` <-
  c('Scrabo',
    'Portavogie',
    'Kircubbin',
    'Ballywalter',
    'Portaferry')

ss$`SMILE Sure Start` <- c(
  'New Lodge',
  'Waterworks',
  'Duncairn',
  'Mount Vernon and Shore Crescent (Castleview)',
  'York Park (Fortwilliam)'
)

ss$`Downpatrick Sure Start` <-
  c(
    'Cathedral',
    'Killough',
    'Ballymote',
    'Ardglass',
    "Audley's Acre",
    'Strangford',
    'Quoile'
  )

ss$`South Belfast Sure Start` <-
  c('Ballynafeigh',
    'Shaftsbury',
    'Botanic',
    'Blackstaff',
    'Upper Malone')

ss$`Clan Mór Sure Start` <- c('Clonard', 'Falls')

ss$`Shankill Sure Start` <-
  c('Shankill Road',
    'Highfield',
    'Glencairn',
    'Woodvale',
    'Ballysillan',
    'Crumlin')

ss$`Beechmount Sure Start` <- c('Beechmount')

ss$`Outer West Belfast Sure Start` <-
  c('Andersonstown', 'Glencolin', 'Glen Rd', 'Ladybrook')

ss$`Saol Ur Sure Start` <- c('Falls Park', 'Upper Springfield', 'Whiterock')

## Those are 33 Sure start areas


# step 2: Render as table and check ---------------------------------------


ss_df <-
  ss %>% map_df(.f = function(x) data.frame(inputted_ward = x), .id = 'sure start centre')
## 186 areas


check_ss <- 
  ss_df$inputted_ward %>% 
  map_df(
    .f = check_names_char,
    checkList = wards_df$`WARD NAME`
  )

## get LDG and name
check_ss <-
  check_ss %>%
  mutate(
    bestLGD = wards_df$`LGD NAME`[bestMatchId],
    bestCode = wards_df$`WARD CODE`[bestMatchId]
  )

check_ss <-
  check_ss %>%
  rename(
    inputted_ward = checkThis
  )

check_ss
export_this <- ss_df %>% left_join(check_ss)
export_this <- export_this %>% arrange(bestScore)

## Step 3: export and check
export_this %>% 
  write_csv('data/national archive to nimdm wards.csv')
