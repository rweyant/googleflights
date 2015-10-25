
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
#'
#' add documentation about other keyword arguments
search <- function(origin,dest,startDate,returnDate,adultCount=1,...){

  dots <- list(...)

  # Build query
  query <- list(
    request=list(
      passengers=list(adultCount=adultCount),
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

  query$request <-
    append(query$request,dots[names(dots) %in% c('maxPrice','saleCountry','refundable','solutions')])


  # make Request
  request <- POST(url = base_url,body=query,encode='json')

  # Check if query was successful
  check_status_code(status_code(request) )

  content(request)
}


#' Check API call executed correctly
#' @param code the status code returned from the POST request
check_status_code <- function(code){
  if(code == 400) stop('The request had bad syntax or was inherently impossible to be satisfied.')
  if(code == 401) stop('Authentication failed, probably because of a bad API key.')
  if(code == 402) stop('A limit was reached, either you exceeded per hour requests limits or your balance is insufficient.')
  if(code == 403) stop('You are not authorized to perform this operation or the api version you\'re trying to use has been shut down.')
  if(code == 404) stop('Requested resource was not found.')
  if(code == 405) stop('Requested method was not found.')
  if(code != 200) stop("Unknown Status Code.")
}
