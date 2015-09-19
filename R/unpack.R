
#' Summarize Segments
#'
#' @param trip
summarize_segments <- function(trip){

  startFlight <- trip$slice[[1]]
  returnFlight <- trip$slice[[2]]

  price <- (trip$saleTotal %>% gsub('USD','',.) %>% as.numeric)

  startOrig <- sapply(startFlight$segment, function(x) x$leg[[1]]$origin)
  startDest <- sapply(startFlight$segment, function(x) x$leg[[1]]$destination)

  returnOrig <- sapply(returnFlight$segment, function(x) x$leg[[1]]$origin)
  returnDest <- sapply(returnFlight$segment, function(x) x$leg[[1]]$destination)

  departing_flights <- sapply(startFlight$segment,function(x) x$flight %>% unlist %>% paste(collapse='-'))
  returning_flights <- sapply(returnFlight$segment,function(x) x$flight %>% unlist %>% paste(collapse='-'))

  departing_carriers <- sapply(startFlight$segment,function(x) x$flight$carrier)
  returning_carriers <- sapply(returnFlight$segment,function(x) x$flight$carrier)

  departing_duration <- sapply(startFlight$segment,function(x) x$duration)
  returning_duration <- sapply(returnFlight$segment,function(x) x$duration)

  # test <- startFlight$segment[[1]]
  list(departing=
         cbind.data.frame(
           departing_stops=length(startOrig)-1,
           departing_locations=startOrig,
           departing_flights=departing_flights,
           departing_carriers=departing_carriers,
           stringsAsFactors=FALSE
           ),
       returning=
         cbind.data.frame(
           returning_stops=length(returnOrig)-1,
           returning_locations=returnOrig,
           returning_flights=returning_flights,
           returning_carriers=returning_carriers,
           stringsAsFactors=FALSE)
       )
}

# simplify results of all
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
