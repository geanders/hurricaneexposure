library(dplyr)
library(devtools)
library(stringi)

# Read in and clean up `county_centers`
county_centers <- read.csv(paste0("http://www2.census.gov/geo/docs/reference/",
                                  "cenpop2010/county/CenPop2010_Mean_CO.txt"),
                           as.is = TRUE) %>%
        mutate(fips = paste0(sprintf("%02d", STATEFP),
                             sprintf("%03d", COUNTYFP)),
               COUNAME = stri_trans_general(COUNAME, "latin-ascii")) %>%
        select(fips, COUNAME, STNAME, POPULATION, LATITUDE, LONGITUDE) %>%
        rename(county_name = COUNAME,
               state_name = STNAME,
               population = POPULATION,
               latitude = LATITUDE, longitude = LONGITUDE)
use_data(county_centers, overwrite = TRUE)
