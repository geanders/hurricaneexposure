#' Location of United States county centers of population
#'
#' A dataset containing the latitude and longitude of the
#' center of population of every United State county,
#' based on population as of the 2010 US Census.
#'
#' @format A data frame with 3221 rows and 7 variables:
#' \describe{
#'   \item{state_fips}{state FIPS code}
#'   \item{county_fips}{county 3-digit FIPS code}
#'   \item{fips}{county 5-digit FIPS code}
#'   \item{count_name}{county name}
#'   \item{state_name}{state name}
#'   \item{population}{population}
#'   \item{latitude}{latitude of county's center of population}
#'   \item{longitude}{longitude of county's center of population}
#' }
#'
#' @source \url{https://www.census.gov/geo/reference/centersofpop.html}
#'
#' @references
#' Bureau of the Census, Centers of Population Computation for the United
#' States 1950-2010, U.S. Department of Commerce, Bureau of the Census,
#'   Washington, DC, issued 2011. \url{http://www2.census.gov/geo/pdfs/reference/cenpop2010/COP2010_documentation.pdf}
"county_centers"

#' Weekly rainfall totals for tropical storms
#'
#' A list that gives the total rainfall in US counties for a one-week
#' window centered at the date on which the tropical storm was closest
#' to the county for all Atlantic basin storms between 1987 and 2012.
#'
#' @format A list with 991,530 rows and 3 variables:
#' \describe{
#'   \item{storm_id}{unique identifier for each tropical storm}
#'   \item{fips}{county 5-digit FIPS code}
#'   \item{tot_precip}{Total precipitation, in millimeters [?], for a
#'       one-week window centered at the date the storm was closest to
#'       the county}
#' }
#'
#' @references
#'    [Get proper reference from Bill and Mohammad]
"storm_rains"
