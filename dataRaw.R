# load -----------------------

require(readxl)
require(stringr)

# download latest data
url <- "http://mappingpoliceviolence.org/s/MPVDatasetDownload-9pyl.xlsx"
download.file(url, destfile = "MPVDatasetDownload.xlsx")

# read data
deaths <- read_excel("MPVDatasetDownload.xlsx", sheet = "2013-2017 Police Killings")

rm(url)


# clean ----------------------

# make age band var
mpv_data$`Victim's age band` <- cut(as.numeric(mpv_data$`Victim's age`),
                                    breaks = c(0, 15, 34, 54, 74, Inf),
                                    labels = c("0-15", "16-34", "35-54", "55-74", "75+"))

# make full address variable
deaths$address <- paste0(deaths$`Location of injury (address)`, ", ", deaths$`Location of death (city)`,
                         ", ", deaths$`Location of death (state)`, " ", deaths$`Location of death (zip code)`, ", USA")

# make searchable address var
deaths$address_searchable <- str_replace_all(deaths$address, " ", "%20")
deaths$address_searchable <- str_replace_all(deaths$address_searchable, "&", "%26")

# # get lat/lon for records 
# addressesA <- lapply(deaths$address[1:2500], geocode, output="latlon")
# addressesB <- lapply(deaths$address[2501:5000], geocode, output="latlon")
# addressesC <- lapply(deaths$address[5001:length(deaths$address)], geocode, output="latlon")

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
