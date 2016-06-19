#' Location of United States county centers of population
#'
#' A dataset containing the latitude and longitude of the
#' center of population of every United State county,
#' based on population as of the 2010 US Census.
#'
#' @details The latitude and longitude of each county are the county's
#' population-weighted mean center of population, based on population as of the
#' 2010 US Census. For more details on the calculation of these mean centers of
#' population, see the reference below.
#'
#' @format A data frame with 3221 rows and 8 variables:
#' \describe{
#'   \item{fips}{County's 5-digit Federal Information Processing Standard (FIPS)
#'              code}
#'   \item{county_name}{County name}
#'   \item{state_name}{State name}
#'   \item{population}{Population of the county as of the 2010 US Census}
#'   \item{latitude}{Latitude of county's center of population, in decimal
#'                  degrees}
#'   \item{longitude}{Longitude of county's center of population, in decimal
#'                   degrees (note: longitudes are given as negative values for
#'                   longitudes west of the prime meridian)}
#' }
#'
#' @source \url{https://www.census.gov/geo/reference/centersofpop.html}
#'
#' @references
#' Bureau of the Census, Centers of Population Computation for the United
#' States 1950-2010, U.S. Department of Commerce, Bureau of the Census,
#'   Washington, DC, issued 2011. \url{http://www2.census.gov/geo/pdfs/reference/cenpop2010/COP2010_documentation.pdf}
"county_centers"

#' Storm tracks for Atlantic basin storms
#'
#' A dataset containing the storm tracks for Atlantic basin tropical
#' storms between 1988 and 2012, from the Extended Best Track Dataset for
#' the Atlantic basin.
#'
#' @details This extended best tracks dataset is currently available for
#' 1988 to 2014. This dataset is based on the HURDAT best tracks dataset.
#' For more details, see the source and reference given below.
#'
#' @format A data frame with 11,829 rows and 5 variables:
#' \describe{
#'   \item{storm_id}{Unique storm identifier with the storm name and year,
#'                  separated by a hyphen(e.g., "Alberto-1988",
#'                  "Katrina-2005")}
#'   \item{date}{Date and time of storm track recording, in Universal Time
#'               Coordinate (UTC). This date is formated as
#'               "\%Y\%m\%d\%H\%M".}
#'   \item{latitude}{Latitude of storm center, in decimal degrees}
#'   \item{longitude}{Longitude of storm center, in decimal degrees (note:
#'                    longitudes are given as negative values for
#'                    longitudes west of the prime meridian)}
#'   \item{wind}{1-minute maximum sustained surface wind speed, measured at
#'              10 meters above the ground, in knots (values are rounded to
#'              the nearest 5-knot value)}
#' }
#'
#' @source \url{http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset}
#'
#' @references
#' Demuth J, DeMaria M, Knaff JA, 2006. Improvement of advanced microwave
#' sounder unit tropical cyclone intensity and size estimation algorithms.
#' Journal of Applied Meteorology 45:1573-1581
#'
"hurr_tracks"

#' Closest distances between counties and a storm track
#'
#' A dataframe that gives the distance and date-time for the closest
#' approach of each tropical storm to the mean population center of each
#' US county in states in the eastern half of the United States.
#'
#' @details The minimum distance was calculated using the Great Circle method,
#' using the \code{spDist} function from the \code{sp} package.
#'
#' @format A dataframe with 876,936 rows and 4 variables:
#' \describe{
#'   \item{storm_id}{Unique storm identifier with the storm name and year,
#'                  separated by a hyphen(e.g., "Alberto-1988",
#'                  "Katrina-2005")}
#'   \item{fips}{County's 5-digit Federal Information Processing Standard (FIPS)
#'              code}
#'   \item{closest_date}{Date (based on local time) of the closest
#'                          approach of the storm to the county's population
#'                          mean center.}
#'   \item{storm_dist}{Minimum distance (in kilometers) between the storm's
#'                     track and the county's population mean center.}
#'   \item{local_time}{Local time of the closest approach of the storm to the
#'                        county's population mean center, based on storm tracks
#'                        linearly interpolated to 15-minute increments.}
#'  \item{closest_time_utc}{Time, in UTC, of the closest approach of the
#'                             storm to the county's population mean center,
#'                             based on storm tracks linearly interpolated to
#'                             15-minute increments.}
#' }
"closest_dist"

#' Rainfall for a week-long window for tropical storms
#'
#' A dataframe that gives the total rainfall in US counties for a one-week
#' window centered at the date on which the tropical storm was closest
#' to the county for Atlantic basin storms between 1988 and 2011.
#'
#' @format A dataframe with 5,417,356 rows and 5 variables:
#' \describe{
#'   \item{fips}{County's 5-digit Federal Information Processing Standard (FIPS)
#'              code}
#'   \item{storm_id}{Unique storm identifier with the storm name and year,
#'                  separated by a hyphen(e.g., "Alberto-1988",
#'                  "Katrina-2005")}
#'   \item{lag}{Number of days from date when storm was closest to the county
#'              (e.g., \code{0} indicates the date the storm was closest to the
#'              county, \code{-2} indicates two days before the date when the
#'              storm was closest to the county)}
#'   \item{precip}{Average daily precipitation, in millimeters, for NLDAS grid
#'    points with the county for the given lag day}
#'   \item{precip_max}{Maximum daily precipitation, in millimeters, for NLDAS grid
#'    points with the county for the given lag day}
#' }
#'
#' @details This dataset was aggregated from hourly, 1/8 degree gridded data
#'    from North America Land Data Assimilation System Phase 2 (NLDAS-2)
#'    precipitation data files to generate daily, county-level precipitation
#'    measures. The NLDAS-2 data integrates satellite-based and land-based
#'    monitoring and applies a land-surface model to create a reanalysis
#'    dataset that is spatially and temporally complete across the continental
#'    United States. To aggregate to a daily county-level value, we averaged
#'    the data at each grid point to generate a daily averaged and then
#'    averaged this value across all grid points within a county's boundaries.
#'    We used county boundaries based on [boundaries at the time of the 2010
#'    US Census ?]. The date-time for each observation at each grid point was
#'    converted to local time (not considering Daylight Savings Time) before
#'    daily averages were generated for the grid point.
#'
#' @note This data set is based on data acquired as part of the mission of
#'    NASA-s Earth Science Division and archived and distributed by the
#'    Goddard Earth Sciences (GES) Data and Information Services Center (DISC).
#'    The county-aggregated daily precipitation values used for this data set
#'    are available for download through the Center for Disease Control's
#'    Wide-ranging Online Data for Epidemiological Research (WONDER) system
#'    (see references).
#'
#' @references
#'
#' Al-Hamdan MZ, Crosson WL, Economou SA, Estes MG, Estes SM, Hemmings SN,
#' Kent ST, Puckette M, Quattrochi DA, Rickman DL, Wade GM, McClure LA, 2014.
#' Environmental public health applications using remotely sensed data.
#' Geocarto International 29(1):85-98.
#'
#' North America Land Data Assimilation System (NLDAS) Daily Precipitation
#' years 1979-2011 on CDC WONDER Online Database, released 2012.
#' \url{http://wonder.cdc.gov/wonder/help/Precipitation.html}
#'
#' Rui H, Mocko D, 2014. README Document for North America Land Data
#' Assimilation System Phase 2 (NLDAS-2) Products. Goddard Earth Sciences
#' Data and Information Services Center.
"rain"
