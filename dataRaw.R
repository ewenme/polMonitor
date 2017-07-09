# script for initial data d/l from mappingpoliceviolence.org and census data

require(readxl)
require(tidyverse)
require(stringr)
require(ggmap)
require(tidycensus)
library(tigris)

# DEATHS DATA ----------------------------------------

# load -----------------------

# download police data
url <- "http://mappingpoliceviolence.org/s/MPVDatasetDownload-9pyl.xlsx"
download.file(url, destfile = "MPVDatasetDownload.xlsx")

# read in police data
deaths <- read_excel("MPVDatasetDownload.xlsx", sheet = "2013-2017 Police Killings")
rm(url)


# clean ----------------------

# make age band var
deaths$`Victim's age band` <- cut(as.numeric(deaths$`Victim's age`),
                                    breaks = c(0, 15, 34, 54, 74, Inf),
                                    labels = c("0-15", "16-34", "35-54", "55-74", "75+"))

# make full address variable
deaths$address <- paste0(deaths$`Location of injury (address)`, ", ", deaths$`Location of death (city)`,
                         ", ", deaths$`Location of death (state)`, " ", deaths$`Location of death (zip code)`, ", USA")

# make searchable address var
deaths$address_searchable <- str_replace_all(deaths$address, " ", "%20")
deaths$address_searchable <- str_replace_all(deaths$address_searchable, "&", "%26")

# # get lat/lon for records 
addressesA <- lapply(deaths$address_searchable[1:2500], geocode, output="latlon")
addressesB <- lapply(deaths$address_searchable[2501:5000], geocode, output="latlon")
addressesC <- lapply(deaths$address[5001:length(deaths$address)], geocode, output="latlon")

# bind records
addressesA <- bind_rows(addressesA)
addressesB  <- bind_rows(addressesB)
addressesC  <- bind_rows(addressesC)

addresses <- bind_rows(addressesA, addressesB, addressesC)

# bind geocodes with data
deaths <- bind_cols(deaths, addresses)

# reverse row order (newest at the bottom)
deaths <- deaths[seq(dim(deaths)[1],1),]

# write to file
write_csv(deaths, "geocodedMPVDataset.csv")


# CENSUS DATA --------------------------------------------------------

# tidycensus key
census_api_key("52dcc3442f47b4e138450637ce5dadb9f444f50c")

options(tigris_use_cache = TRUE)

# get census variables
vars <- load_variables(dataset = "sf1", year = 2010)

# load state census data for age/sex/race pops
pops <- get_decennial(year = 2010, variables = c("P012A003", "P012A004", 
                                                 "P012A005", "P012A006", "P012A007", "P012A008", 
                                                 "P012A009", "P012A010", "P012A011", "P012A012", 
                                                 "P012A013", "P012A014", "P012A015", "P012A016", 
                                                 "P012A017", "P012A018", "P012A019", "P012A020", 
                                                 "P012A021", "P012A022", "P012A023", "P012A024", 
                                                 "P012A025", "P012A026", "P012A027", "P012A028",
                                                 "P012A029", "P012A030", "P012A031", "P012A032",
                                                 "P012A033", "P012A034", "P012A035", "P012A036",
                                                 "P012A037", "P012A038", "P012A039", "P012A040",
                                                 "P012A041", "P012A042", "P012A043", "P012A044", 
                                                 "P012A045", "P012A046","P012A047", "P012A048", 
                                                 "P012A049",
                                                 "P012B003", "P012B004", 
                                                 "P012B005", "P012B006", "P012B007", "P012B008", 
                                                 "P012B009", "P012B010", "P012B011", "P012B012", 
                                                 "P012B013", "P012B014", "P012B015", "P012B016", 
                                                 "P012B017", "P012B018", "P012B019", "P012B020", 
                                                 "P012B021", "P012B022", "P012B023", "P012B024", 
                                                 "P012B025", "P012B026", "P012B027", "P012B028",
                                                 "P012B029", "P012B030", "P012B031", "P012B032",
                                                 "P012B033", "P012B034", "P012B035", "P012B036",
                                                 "P012B037", "P012B038", "P012B039", "P012B040",
                                                 "P012B041", "P012B042", "P012B043", "P012B044", 
                                                 "P012B045", "P012B046","P012B047", "P012B048", 
                                                 "P012B049",
                                                 "P012C003", "P012C004", 
                                                 "P012C005", "P012C006", "P012C007", "P012C008", 
                                                 "P012C009", "P012C010", "P012C011", "P012C012", 
                                                 "P012C013", "P012C014", "P012C015", "P012C016", 
                                                 "P012C017", "P012C018", "P012C019", "P012C020", 
                                                 "P012C021", "P012C022", "P012C023", "P012C024", 
                                                 "P012C025", "P012C026", "P012C027", "P012C028",
                                                 "P012C029", "P012C030", "P012C031", "P012C032",
                                                 "P012C033", "P012C034", "P012C035", "P012C036",
                                                 "P012C037", "P012C038", "P012C039", "P012C040",
                                                 "P012C041", "P012C042", "P012C043", "P012C044", 
                                                 "P012C045", "P012C046","P012C047", "P012C048", 
                                                 "P012C049",
                                                 "P012D003", "P012D004", 
                                                 "P012D005", "P012D006", "P012D007", "P012D008", 
                                                 "P012D009", "P012D010", "P012D011", "P012D012", 
                                                 "P012D013", "P012D014", "P012D015", "P012D016", 
                                                 "P012D017", "P012D018", "P012D019", "P012D020", 
                                                 "P012D021", "P012D022", "P012D023", "P012D024", 
                                                 "P012D025", "P012D026", "P012D027", "P012D028",
                                                 "P012D029", "P012D030", "P012D031", "P012D032",
                                                 "P012D033", "P012D034", "P012D035", "P012D036",
                                                 "P012D037", "P012D038", "P012D039", "P012D040",
                                                 "P012D041", "P012D042", "P012D043", "P012D044", 
                                                 "P012D045", "P012D046","P012D047", "P012D048", 
                                                 "P012D049",
                                                 "P012E003", "P012E004", 
                                                 "P012E005", "P012E006", "P012E007", "P012E008", 
                                                 "P012E009", "P012E010", "P012E011", "P012E012", 
                                                 "P012E013", "P012E014", "P012E015", "P012E016", 
                                                 "P012E017", "P012E018", "P012E019", "P012E020", 
                                                 "P012E021", "P012E022", "P012E023", "P012E024", 
                                                 "P012E025", "P012E026", "P012E027", "P012E028",
                                                 "P012E029", "P012E030", "P012E031", "P012E032",
                                                 "P012E033", "P012E034", "P012E035", "P012E036",
                                                 "P012E037", "P012E038", "P012E039", "P012E040",
                                                 "P012E041", "P012E042", "P012E043", "P012E044", 
                                                 "P012E045", "P012E046","P012E047", "P012E048", 
                                                 "P012E049",
                                                 "P012H003", "P012H004", 
                                                 "P012H005", "P012H006", "P012H007", "P012H008", 
                                                 "P012H009", "P012H010", "P012H011", "P012H012", 
                                                 "P012H013", "P012H014", "P012H015", "P012H016", 
                                                 "P012H017", "P012H018", "P012H019", "P012H020", 
                                                 "P012H021", "P012H022", "P012H023", "P012H024", 
                                                 "P012H025", "P012H026", "P012H027", "P012H028",
                                                 "P012H029", "P012H030", "P012H031", "P012H032",
                                                 "P012H033", "P012H034", "P012H035", "P012H036",
                                                 "P012H037", "P012H038", "P012H039", "P012H040",
                                                 "P012H041", "P012H042", "P012H043", "P012H044", 
                                                 "P012H045", "P012H046","P012H047", "P012H048", 
                                                 "P012H049"),
                      geography = "STATE") %>%
  left_join(vars, by=c("variable"="name")) %>%
  mutate(gender=ifelse(str_detect(label, "Female"), "Female", "Male"),
         age_band=ifelse(str_detect(label, "Under 5"), "0-15",
                  ifelse(str_detect(label, "5 to 9"), "0-15",
                  ifelse(str_detect(label, "10 to 14"), "0-15",
                  ifelse(str_detect(label, "15 to 17"), "16-34",
                  ifelse(str_detect(label, "18 and 19"), "16-34",
                  ifelse(str_detect(label, "20"), "16-34",
                  ifelse(str_detect(label, "21"), "16-34",
                  ifelse(str_detect(label, "22 to 24"), "16-34",
                  ifelse(str_detect(label, "25 to 29"), "16-34",
                  ifelse(str_detect(label, "30 to 34"), "16-34",
                  ifelse(str_detect(label, "35 to 39"), "35-54",
                  ifelse(str_detect(label, "40 to 44"), "35-54",
                  ifelse(str_detect(label, "45 to 49"), "35-54",
                  ifelse(str_detect(label, "50 to 54"), "35-54",
                  ifelse(str_detect(label, "55 to 59"), "55-74",
                  ifelse(str_detect(label, "60 and 61"), "55-74",
                  ifelse(str_detect(label, "62 to 64"), "55-74",
                  ifelse(str_detect(label, "65 and 66"), "55-74",
                  ifelse(str_detect(label, "67 to 69"), "55-74",
                  ifelse(str_detect(label, "70 to 74"), "55-74", "75+")))))))))))))))))))),
         race=ifelse(str_detect(concept, "White"), "White",
              ifelse(str_detect(concept, "Black Or African American"), "Black",
              ifelse(str_detect(concept, "American Indian"), "Native American",
              ifelse(str_detect(concept, "Asian"), "Asian",
              ifelse(str_detect(concept, "Native Hawaiian"), "Pacific Islander",
              ifelse(str_detect(concept, "Hispanic"), "Hispanic", ""))))))) %>%
  select(-label, -concept) %>%
  ungroup()

# write to file
write_csv(pops, "censusData.csv")
