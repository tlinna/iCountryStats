# Process iRacing's CSV files and save them in RDS format

fileTime <- as_date(file.mtime("files/Road_driver_stats.csv"))

if(file.exists("files/Road_driver_stats.csv")) {
  roadDrivers <- read_csv("files/Road_driver_stats.csv")
} else {
  download.file("https://s3.amazonaws.com/ir-data-now/csv/Road_driver_stats.csv", "files/Road_driver_stats.csv")
  roadDrivers <- read_csv("files/Road_driver_stats.csv")
}
if(file.exists("files/Oval_driver_stats.csv")) {
  ovalDrivers <- read_csv("files/Oval_driver_stats.csv")
} else {
  download.file("https://s3.amazonaws.com/ir-data-now/csv/Oval_driver_stats.csv", "files/Oval_driver_stats.csv")
  ovalDrivers <- read_csv("files/Oval_driver_stats.csv")
}

fileTime <- as_date(file.mtime("files/Road_driver_stats.csv"))

colnames(roadDrivers) <- make.names(c("Driver", "CustID", "Location", "Club name", "Starts", "Wins", "Avg Start Pos", "Avg Finish Pos",
                                      "Avg Points", "Top 25%", "Laps", "Laps lead", "Avg Inc", "Class", "iRating", "ttRating",
                                      "Total Club Points", "Championship Points"))
colnames(ovalDrivers) <- make.names(c("Driver", "CustID", "Location", "Club name", "Starts", "Wins", "Avg Start Pos", "Avg Finish Pos",
                                      "Avg Points", "Top 25%", "Laps", "Laps lead", "Avg Inc", "Class", "iRating", "ttRating",
                                      "Total Club Points", "Championship Points"))

roadDrivers[roadDrivers == -1] <- NA
ovalDrivers[ovalDrivers == -1] <- NA
roadDrivers <- filter(roadDrivers, !is.na(iRating))
ovalDrivers <- filter(ovalDrivers, !is.na(iRating))

roadDrivers$SR <- 0
ovalDrivers$SR <- 0
roadDrivers$DriverClass <- "xxx"
ovalDrivers$DriverClass <- "xxx"
roadDrivers$DriverClass <- str_sub(roadDrivers$Class, start = 1, end = 1)
ovalDrivers$DriverClass <- str_sub(ovalDrivers$Class, start = 1, end = 1)

roadDrivers$SR <- roadDrivers$Class
str_sub(roadDrivers$SR, start = 1, end = 2) <- ""
roadDrivers$SR <- as.numeric(roadDrivers$SR)
ovalDrivers$SR <- ovalDrivers$Class
str_sub(ovalDrivers$SR, start = 1, end = 2) <- ""
ovalDrivers$SR <- as.numeric(ovalDrivers$SR)

roadDrivers$DriverClass <- factor(roadDrivers$DriverClass, levels = c("D", "C", "B", "A", "P"))
ovalDrivers$DriverClass <- factor(ovalDrivers$DriverClass, levels = c("D", "C", "B", "A", "P"))
roadDrivers <- roadDrivers[order(roadDrivers$DriverClass, roadDrivers$iRating),]
ovalDrivers <- ovalDrivers[order(ovalDrivers$DriverClass, ovalDrivers$iRating),]

roadDrivers$Win.rate <- roadDrivers$Wins / roadDrivers$Starts
roadDrivers$Win.rate[roadDrivers$Laps.lead < 1] <- 0
ovalDrivers$Win.rate <- ovalDrivers$Wins / ovalDrivers$Starts
ovalDrivers$Win.rate[ovalDrivers$Laps.lead < 1] <- 0

roadDrivers$Country <- countrycode(roadDrivers$Location, origin = "iso2c", destination = "country.name", nomatch = "Unknown")
ovalDrivers$Country <- countrycode(ovalDrivers$Location, origin = "iso2c", destination = "country.name", nomatch = "Unknown")

roadComb <- roadDrivers[,c("Driver", "iRating", "Country", "Starts", "Wins", "Class", "Win.rate")]
colnames(roadComb) <- c("Driver", "Road.iRating", "Country", "Road.Starts", "Road.Wins", "Road.Class", "Road.Win.rate")
ovalComb <- ovalDrivers[,c("Driver", "iRating", "Starts", "Wins", "Class", "Win.rate")]
colnames(ovalComb) <- c("Driver", "Oval.iRating", "Oval.Starts", "Oval.Wins", "Oval.Class", "Oval.Win.rate")
combinedDrivers <- merge(roadComb, ovalComb, by = "Driver")


saveRDS(roadDrivers, file = "files/roadDrivers.rds", compress = T)
saveRDS(ovalDrivers, file = "files/ovalDrivers.rds", compress = T)  
saveRDS(combinedDrivers, file = "files/combinedDrivers.rds", compress = T)
dir_create("files/archive")
saveRDS(combinedDrivers, file = paste0("files/archive/combinedDrivers-", fileTime, ".rds"), compress = T)

# roadDrivers <- readRDS("files/roadDrivers.rds")
# ovalDrivers <- readRDS("files/ovalDrivers.rds")
# combinedDrivers <- readRDS("files/combinedDrivers.rds")


listOfCountries <- roadDrivers[,c("Country")]
listOfCountries$Country <- factor(listOfCountries$Country)
# nRows <- 20 # number of drivers required
tab <- table(listOfCountries$Country) # count
tab <- sort(tab, decreasing = T)
listOfCountries <- names(tab)
listOfCountries <- listOfCountries[listOfCountries != "Unknown"]

# listOfCountries <- listOfCountries[listOfCountries$Location %in% names(tab)[tab >= nRows], ] # extract rows
# listOfCountries <- unique(listOfCountries)
# listOfCountries <- as.vector(listOfCountries[["Location"]])

listOfCountries <- c("All countries", listOfCountries, "Unknown")
