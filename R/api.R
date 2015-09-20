suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(XML))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))


#' set Google API key for future functions
#'
#' @param key your API key
set_apikey <- function(key) {
  options('googleflightskey'=key)
  base_url <- paste('https://www.googleapis.com/qpxExpress/v1/trips/search?key=',
                    key,sep='')
  assign('base_url',base_url,envir=.GlobalEnv)
}


#' Search QPX for flights
#' @param origin Departing Airport Code
#' @param dest Destination Airport Code
#' @param startDate Departure date (yyyy-mm-dd)
#' @param startDate Return date (yyyy-mm-dd)
#' @param numPassengers Number of people traveling
search <- function(origin,dest,startDate,returnDate,adultCount=1,...){

  dots <- list(...)

  # Build query
  query <- list(
    request=list(
      passengers=list(adultCount=numPassengers),
      slice=data.frame(origin=c(origin,dest),destination=c(dest,origin),date=c(startDate,returnDate)))
    )

  # Add extra passenger info
  query$request$passengers <-
    append(query$request$passengers,dots[names(dots) %in% c('childCount',
                                                            'infantInLapCount',
                                                            'infantInLapCount',
                                                            'infantInSeatCount',
                                                            'seniorCount')])

  # probably a better way to do this.
  if(!is.null(dots$maxStops)) query$request$slice$maxStops <- dots$maxStops
  if(!is.null(dots$maxConnectionDuration)) query$request$slice$maxConnectionDuration <- dots$maxConnectionDuration
  if(!is.null(dots$preferredCabin)) query$request$slice$preferredCabin <- dots$preferredCabin
  if(!is.null(dots$permittedCarrier)) query$request$slice$permittedCarrier <- dots$permittedCarrier
  if(!is.null(dots$prohibitedCarrier)) query$request$slice$maxStops <- dots$prohibitedCarrier
#
#   query$request$slice <-
#     append(query$request$slice,dots[names(dots) %in% c('maxStops',
#                                                        'maxConnectionDuration',
#                                                        'preferredCabin',
#                                                        'permittedCarrier',
#                                                        'prohibitedCarrier',
#                                                        'alliance')])
  query$request <-
    append(query$request,dots[names(dots) %in% c('maxPrice','saleCountry','refundable','solutions')])


  # make Request
  request <- POST(url = base_url,body=query,encode='json')

  # Check if query was successful
  if(status_code(request) != 200) stop('Error with request...')

  content(request)
}
