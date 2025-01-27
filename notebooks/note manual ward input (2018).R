## note: manual entry and data check for Sure Start
## Goal: enter the Sure Start wards and compare to nearest fit then manual checks


## Source is the NI Sure Start list ni gov direct 2014

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

ss$`lisburn` <- # + 2018 areas taken from Colin neighbourhood ss
 c(
  'Old Warren', #not new from Colin
  'Tonagh', # +2018
  'Hillhall 1' #likely this SOA from colin -- not new
 )


ss$`bangor` <- # + 2018 supports areas prev covered by Ards
  c(
  'Whitehill',
  'Dufferin',
  'Conlig 3', # lsited as 'Conlig' but modern site = conlig 3 
  'Harbour 1' #list as 'Harbour (North Down)' but modern page confirm habour 1
  )

ss$`Star SS` <- # + 2018
  c(
    'The Cut',
    'Edenderry'
  )

ss$`little steps` <- #joined with Ballymena sure start? Called ballymena + little steps (duplicated entries in 2018)
  c(
    'Ballycraigy',
  'Farranshane',
  'Steeple'
  )

ss$`Arke Sure Start` <- # supposedly set up in 2007
  c(
    'Abbey Park', #  ward
  'Callan Bridge', #  ward
  'Downs', #  ward
  'Keady', #  ward
  'The Mall', #part ward according to their 2024 website
  'Observatory', #part ward according to their 2024 website. LA area according to website
#  'Augher' #  ward, removed by 2018
  )

ss$`kilkeel` <- #aka Mourne https://www.mindingyourhead.info/family-support-programmes -- set up ub 2011
  c(
    'Kilkeel Central 2 (SOA)',# 'Kilkeel Central' in 2011 but list as SOA in 2014 so likely mistake orignally
    'Kilkeel South 2 (SOA)', #'
  )

ss$`G-Old Community Sure Start` <-
  c('Gortalowry', 'Oldtown', 'Killymoon', 'Ardboe',
  'Dunamore', ## new by 2011. Dunamore is a village not a ward or SOA
  'Pomeroy', # new in 2011 (could ward or SOA) -- likely SOA
  'Maghera' # +2018
  )
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
    'Bonamargy and Rathlin',
    'Newhill' #+2018
  )

ss$`Coleraine Sure Start` <-
  c('Ballysally',
    'Central',
    'Churchlands',
    'Cross Glebe',
    'Knocklyn (Windyhall Estate)',
    'University (Millburn Estate)',
    'Royal Portrush' # +2018
    )

ss$`Ballymena South Sure Start` <- #i.e. Ballymena and Little stepsSure start
  c(
    'Ballykeel',
    'Ballee',
    'Moat',
    'Harryville',
    'Dunclag',
    'Fairgreen',
    'Castle Demesne',
    'Summerfield',
    'Ballycraigy', # +2018 part of little steps
    'Farranshane', # +2018 little steps 
    'Steeple' # +2018 little steps
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
    'Northlands', # (C) -- = carrickfergue
    'Sunnylands',
    'Clipperstown',
    'Love Lane',
    'Antiville', # (L) = larne
    'Ballyloran',
    'Craigyhill',
    'Killycrot' #+ 2018 
  )


ss$`Dungannon Sure Start` <- c(
  'Ballysaggart',
  'Benburb (part ward)',
  'Coalisland South',
  'Coolhill (part ward)',
  'Drumglass (part ward)',
  'Killymeal (part ward)',
  'Moygashel (part ward)',
  'Mullaghmore (part ward)',
  'Castlecaulfield (part ward)',
  'Coalisland North' # added 2011
)

ss$`Clogher Valley Sure Start` <-
  c('Anghnacloy',
    'Augher',
    'Ballygawley',
    'Clogher',
    'Fivemiletown')

ss$`South Armagh Sure Start` <-
  c('Bessbrook',
    'Camlough',# 'Carnlough', previously spely
    'Creggan',
    'Crossmaglen',
    'Derrymore',
    'Newtownhamilton',
    'Silver Bridge' # +2011 ward
    )

ss$`Orana Sure Start` <- c( #aka Newry city sure start
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
    'Drumgask',
    'Drumgor',
    'Drumnamoe',
    'Taghnevan',
    'Woodville 1 (SOA)', #added 2011
    'Parkmore Housing Estate in Craigavon',
    'Mourneview' #+2018 part ward
    )

## `Western`
  
ss$`Strabane Sure Start` <- c(
  'Ballycolman North', #Ballycolman
  'Ballycolman South',
  'Ballycolman East',
  'Ballycolman West',
#  'Ballycolman',
  'Sion Mills',
  'Finn',
  'Dunnamanagh',
  'Plumbridge',
  'Strabane (North, South, East, West)' # +2018 but maybe just weird way of listing old areas
)


ss$`Little Hands Sure Start` <-
  c('Creevagh', 'Springtown', 'Rosemount',
  'Foylesprings 2 (SOA)' # added by 2014 
  )

ss$`Shantallow Sure Start` <-
  c('Shantallow East', 'Shantallow West', 'Carnhill', 'Culmore Area',
  'Ballynashallog' # added 2011 - ward
  )

ss$`Cherish Sure Start` <- 
  c(
  'Irvinestown',
  'Kesh',
  'Ederney',
  'Lack',
  'Lisnarrick',
  'Ballinamallard',
  'Trillick',
  'Devenish',
  'Rosslea',
  'Newtownbutler'
)

ss$`Dungiven Sure Start` <- #i.e. dry arch ss
  c(
    'The Highlands',
    'Dungiven',
    'Feeny',
    'Upper Glenshane',
    'Glack',
    'Coolessan',
    'Greystone',
    'Enagh (Limavady)', # +2011 ward
    'Roeside (SOA)' #+ by 2014 likely SOA but could be ward
  )

ss$`Last Sure Start` <- c('Lisanelly',
                          'Drumragh',
                          'Killyclogher',
                          'Camowen',
                          'Strule',
                          'Fintona',
                          'Termon',
                          'Gortrush' #+ by 2014 -- ward as also said on modern website -
                          )

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

ss$`Waterside Sure Start` <- 
  c('Victoria', 'Ebrington', 'Clondermott', 'Enagh (Derry)',
  'Caw' #unsure if Ward or SOA - added by 2014
  )


## Eastern

ss$`Glenbrook Sure Start` <-
  c('Ardoyne', 
  'Cliftonville', 
  'Ligoniel') ## This ends in a , but no further values on webpage

ss$`East Belfast Sure Start` <-
  c('Island',
    'The Mount',
    'Ballymacarett',
    'Woodstock',
    'Enler',
    'Tullycarnet',
    'Bloomfield 1 (SOA)'
    )

ss$`Colin Neighbourhood Sure Start` <-
  c('Twinbrook',
    'Poleglass',
    'Colin Glen',
#    'Old Warren', ## removed by 2014 - to old lisburn
    'Kilwee',
    'Lagmore (Derriaghy)'
#    'Hillhall 1 (SOA)' ## removed  by 2014!
    )

ss$`Lower Ards Peninsula Sure Start` <- # aka sure start ards
  c('Scrabo',
    'Portavogie',
    'Kircubbin',
    'Ballywalter',
    'Portaferry',
#    'Harbour 1 (SOA)', # +2011, removed 2018
#    'Conlig 3 (SOA)' # +2011, removed 2018
    'Central Ards' # +by 2018
    )

ss$`SMILE Sure Start` <- c(
  'New Lodge',
  'Waterworks',
  'Duncairn',
  'Mount Vernon and Shore Crescent (Castleview)', #listed as just castleview in 2018
#  'York Park (Fortwilliam)', # removed 2018 but close to chichester park
  'Chichester Park' # +2018 but close to york park
)

ss$`Downpatrick Sure Start` <- # i.e down sure start
  c(
    'Cathedral',
    'Killough',
    'Ballymote (Flying Horse)', # ward or soa
    'Ardglass',
    "Audley's Acre",
    'Strangford',
    'Quoile',
    'Murlough' #+2018
  )

ss$`South Belfast Sure Start` <- # aka inner city south belfast
  c('Ballynafeigh',
    'Shaftsbury',
    'Botanic',
    'Blackstaff', # in 2018 amended to just blackstaff
    #'Upper Malone' # upper malone no logner in 2011 -- maybe include else?
    'Upper Malone (Taughmonagh and Benmore Estates)', # readded in 2018
    'Minnowburn' # +2018
    )

ss$`Clan Mór Sure Start` <- c('Clonard', 'Falls')

ss$`Shankill Sure Start` <-
  c('Shankill', #previously just listed as shankill rd in 2009
    'Highfield',
    'Glencairn',
    'Woodvale',
    'Ballysillan',
    'Crumlin (Belfast)')

ss$`Beechmount Sure Start` <- c('Beechmount')

ss$`Outer West Belfast Sure Start` <-
  c('Andersonstown', 
  'Glencolin', 
  'Glen Rd', 
  'Ladybrook')

ss$`Saol Ur Sure Start` <- 
  c('Falls Park', 
  'Upper Springfield', 
  'Whiterock'
  )

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
