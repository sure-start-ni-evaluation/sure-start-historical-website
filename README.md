# Sure Start website data (Northern Ireland) 

Sure Start is a flagship early years programme aimed at families with children under five living in the most deprived areas of Northern Ireland. It began in 2000 and has undergone a series of expansions over time (incorporating more areas of Northern Ireland). The first of these expansions took place between 2006 and 2010, and additional wards were added to the programme based on deprivation. 

In 2009, the former Sure Start website (archived [here](https://web.archive.org/web/20090101115113/http://www.surestart.gov.uk/aboutsurestart/help/contacts/northernireland/)) recorded information on all the wards eligible for Sure Start.  

The purpose of this repository is to format information on Sure Start coverage in a structured way for further data linkage and research. 

## Usage

For those interested in the cleaned data, the table of wards covered by Sure Start is contained in the `data` folder as `national archive to nimdm wards (checked).csv`. This table contains the ward codes and names of Sure Start wards (using 1992 ward boundaries—the geographical unit used at the time). 

Interactive maps of the coverage areas for each Sure Start ward are in the `docs` folder and can be viewed on the [project page](https://menglezhang.github.io/sure-start-historical-website/sure-start-map) for this repository or as html files in the `docs` folder. The code and data for the maps are in `webpage code` and `data`.



## Data collection

The earliest data on ward coverage comes from January 2009 (located by clicking on the links at the bottom of the page [here](https://web.archive.org/web/20090101115113/http://www.surestart.gov.uk/aboutsurestart/help/contacts/northernireland/)). 

Ward names were:

- manually inputted into R and compared to official ward names using string matching (`notebooks/note manual ward input.R`). The official ward names used come from the Northern Ireland Multiple Deprivation Index 2010 (only used as a convenient reference for ward names). Sometimes, only part of a ward may be eligible for Sure Start.  

- the results of the best matching ward names and codes are outputs in  `data/national archive to nimdm wards.csv`. These results were manually checked and amended to produce the data in `national archive to nimdm wards (checked).csv`. 

As part of ongoing research, we are attempting to validate the coverage data with other sources, including information held by Sure Start centres. Ideally, we would like to distinguish which wards were in the original rollout of Sure Start (2000 - 2003) and which wards were added as part of the first expansion of the programme. 

## Further information

This work was supported and funded by UK Research and Innovation (ES/Z502558/1) and the UK Prevention Research Partnership Maternal and Child Health Network (MR/S037608/1). 

Further information about the project is available [here](https://cascadewales.org/research/the-impact-of-sure-start-on-health-and-social-care/) and [here](https://matchnet.sphsu.gla.ac.uk/pump-priming-funding/). 

I can be contacted by email at [zhangm19@cardiff.ac.uk](zhangm19@cardiff.ac.uk). 
