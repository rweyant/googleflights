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
  if(status_code(r) != 200) stop('Error with request...')

  return(content(r))
}

simplify_ <- function(content){
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
}
