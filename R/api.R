suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(XML))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))

#' set Google API key for future functions
#' @param key your API key
set_apikey <- function(key) options('googleflightskey'=key)

#' Search QPX for flights
#' @param origin Departing Airport Code
#' @param dest Destination Airport Code
#' @param startDate Departure date (yyyy-mm-dd)
#' @param startDate Return date (yyyy-mm-dd)
#' @param numPassengers Number of people traveling
search <- function(origin,dest,startDate,returnDate,numPassengers=1,...){

  base_url <- paste('https://www.googleapis.com/qpxExpress/v1/trips/search?key=',
                    getOption('googleflightskey'),sep='')

  # Build query
  query <- list(
    request=list(
      passengers=list(adultCount=numPassengers),
      slice=data.frame(origin=c(origin,dest),destination=c(dest,origin),date=c(startDate,returnDate)))
    )

  # make Request
  request <- POST(url = base_url,body=query,encode='json')

  # Check if query was successful
  if(status_code(request) != 200) stop('Error with request...')

  return(content(request))
}

summarize_segments <- function(trip){

  startFlight <- trip$slice[[1]]
  returnFlight <- trip$slice[[2]]

  price <- trip$saleTotal %>% gsub('USD','',.) %>% as.numeric

  startOrig <- sapply(startFlight$segment, function(x) x$leg[[1]]$origin)
  startDest <- sapply(startFlight$segment, function(x) x$leg[[1]]$destination)

  returnOrig <- sapply(returnFlight$segment, function(x) x$leg[[1]]$origin)
  returnDest <- sapply(returnFlight$segment, function(x) x$leg[[1]]$destination)

  departing_flights <- sapply(startFlight$segment,function(x) x$flight %>% unlist %>% paste(collapse='-'))
  returning_flights <- sapply(returnFlight$segment,function(x) x$flight %>% unlist %>% paste(collapse='-'))

  departing_carriers <- sapply(startFlight$segment,function(x) x$flight$carrier)
  returning_carriers <- sapply(returnFlight$segment,function(x) x$flight$carrier)

  # test <- startFlight$segment[[1]]

  cbind.data.frame(price,
                   departing_stops=length(startOrig)-1,
                   returning_stops=length(returnOrig)-1,
                   departing_locations=startOrig,
                   returning_locations=returnOrig,
                   departing_flights=departing_flights,
                   returning_flights=returning_flights,
                   departing_carriers=departing_carriers,
                   returning_carriers=returning_carriers,
                   stringsAsFactors=FALSE)

}

simplify_ <- function(content){

  # Each trip
  trips <- content$trips$tripOption
  # Extract information from each trip.
  summary <-
    sapply(trips,summarize_trip) %>% t
  }

nothing <- function(){
  names(content$trips)
  length(content$trips)
  names(content$trips)
  content$trips$data$airport[1]
  tmp <- content$trips$tripOption[1]
  tmp2 <- tmp[[1]]
  tmp2$saleTotal
  tmp3 <- tmp2$slice
  tmp4 <- tmp3[[1]]
  tmp4$duration

  content$trips$tripOption
  content$trips$tripOption[[1]]$saleTotal
  salePrice <- sapply(content$trips$tripOption,function(x) gsub('USD','',x$saleTotal))
  routeBeg <- sapply(content$trips$tripOption[[1]]$slice[[1]]$segment, function(x) x$leg[[1]]$origin)
  routeEnd <- sapply(content$trips$tripOption[[1]]$slice[[1]]$segment, function(x) x$leg[[1]]$destination)
  individualFlights <-
    paste(sapply(content$trips$tripOption[[1]]$slice[[1]]$segment,function(x) x$flight %>% unlist %>% paste(collapse='-')),collapse=' -> ')
  paste(sapply(content$trips$tripOption[[1]]$slice[[2]]$segment,function(x) x$flight %>% unlist %>% paste(collapse='-')),collapse=' -> ')
  content$trips$tripOption[[1]]$slice %>% length

}
