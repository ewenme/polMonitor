# LOAD -------------------------------------------------------

# load current version of db
raw_data <- read_csv("geocodedMPVDataset.csv")

# download latest data
url <- "http://mappingpoliceviolence.org/s/MPVDatasetDownload-sy54.xlsx"
download.file(url, destfile = "MPVDatasetDownloadUpdated.xlsx")

# load latest version of db
updated_data <- read_excel("MPVDatasetDownloadUpdated.xlsx", sheet = "2013-2017 Police Killings")


# CLEAN -----------------------------------------------------

