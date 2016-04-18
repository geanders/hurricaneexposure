study.states <- c('maine', 'new hampshire', 'vermont',                
                  'massachusetts', 'connecticut', 'new york', 'rhode island',
                  'new jersey', 'delaware', 'pennsylvania', 'maryland',
                  'virginia', 'north carolina', 'south carolina', 'georgia',
                  'florida', 'alabama', 'mississippi', 'louisiana', 'texas',
                  'west virginia', 'arkansas', 'tennessee', 'kentucky',
                  'oklahoma', 'missouri', 'illinois', 'kentucky', 'indiana', 
                  'michigan', 'ohio', 'wisconsin', 'kansas', 'iowa')
library(maps)
data(state.fips) ## from the 'maps' package
study.state.fips <- state.fips$fips[state.fips$polyname %in%
                                            study.states]
study.state.fips <- c(study.state.fips, 25, 26, 36, 37, 51)
fips.list <- study.state.fips

load("/Users/brookeanderson/Documents/Hopkins\ Postdoc/hurricanes/DetermineCountyStormDates/ClosestStormDates.Rdata")
get.storm.rain.date.range <- function(df){
        range.closest.dates <- range(strptime(as.character(df$closest.date),
                                              format = "%Y%m%d%H%M"))
        range.rain.windows <- as.Date(range.closest.dates) + c(-3, 3)
        rain.date.range <- seq(from = range.rain.windows[1],
                               to = range.rain.windows[2],
                               by = 1)
        return(as.character(rain.date.range))
}
storm.rain.dates <- lapply(closest.storm.dates, 
                           get.storm.rain.date.range)

## Create directories and files for all storms
for(storm in names(storm.rain.dates)){
        dir.name <- paste("/Users/brookeanderson/storm.precip.files",
                          storm, sep = "/")
        dir.create(dir.name)
        for(state in study.state.fips){
                file.name <- paste0(dir.name, "/", "state", state, ".csv")
                file.create(file.name)
                cat(paste0(paste(c("fips", "date", "precip"), collapse = ", "),
                           "\n"),
                    file = file.name)
        }
}

storm.dates <- data.frame(
        storm.id = rep(names(storm.rain.dates), 
                       unlist(lapply(storm.rain.dates, length))),
        date = as.Date(unlist(storm.rain.dates), format = "%Y-%m-%d"),
        stringsAsFactors = FALSE
        )

## Note: This file is the unzipped version of nasa_precipitation.zip
## I may not keep the unzipped file on my computer because it's so big
con <- file("/Users/brookeanderson/nasa_precip_export_2.txt")
open(con)
while (length(input <- readLines(con, n=1000)) > 0){ 
        j <- 0
        input <- strsplit(input, "\t") 
        input <- matrix(unlist(input), ncol = 15, byrow = TRUE)
        for(i in 1:nrow(input)){
                j <- j + 1
                state <- input[i, 3]
                row.date <- as.Date(input[i, 10], format = "%Y%m%d")
                if(!(state %in% fips.list)){ 
                        if(j == 100){
                                print(paste("no, ", state))
                                j <- 0
                        }
                        next 
                }
                if(!(row.date %in% storm.dates$date)){ 
                        if(j == 100){
                                print(paste("no, ", row.date))
                                j <- 0
                        }
                        next 
                }
                
                ## If the row has a relevant date and state, write the line to 
                ## the appropriate files
                storm.ids <- unique(subset(storm.dates, date == row.date)$storm.id)
                for(storm in storm.ids){
                        if(j == 100){
                                print(paste(storm, state, row.date))
                                j <- 0
                        }
                        filename <- paste0("storm.precip.files/",
                                           as.character(storm), "/state",
                                           state, ".csv")
                        cat(paste0(paste(input[i,c(4, 10, 12)], collapse = ", "),
                                  "\n"),
                            file = filename, append = TRUE)
                }
                
        }
        
} 
close(con)
