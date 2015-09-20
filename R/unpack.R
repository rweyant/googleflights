
#' Summarize a trip
#'
#' @param trip
summarize_segment <- function(trip_segment){

  origin <- sapply(trip_segment$segment, function(x) x$leg[[1]]$origin)
  destination <- sapply(trip_segment$segment, function(x) x$leg[[1]]$destination)

  flight_number <- sapply(trip_segment$segment,function(x) x$flight %>% unlist %>% paste(collapse='-'))

  carrier <- sapply(trip_segment$segment,function(x) x$flight$carrier)

  duration <- sapply(trip_segment$segment,function(x) x$duration)

  cbind.data.frame(
    n_stops=length(origin)-1,
    origin=origin,
    destination=destination,
    flight_number=flight_number,
    carrier=carrier,
    duration=duration,
    stringsAsFactors=FALSE
    )
}

extract.time.info <- function(x){

  first.split <- str_split_fixed(x,'T',n=2)
  if(grepl('\\-',first.split[2])) {
    split_char <- '\\-'
  } else if(grepl('\\+',first.split[2])) {
    split_char <- '\\+'
  } else stop('extract.time.info: Error parsing time.')

  second.split <-  str_split_fixed(first.split[2],split_char,n=2)
  data.frame(date=first.split[1],time=second.split[1],timezone=paste(substr(split_char,2,3),second.split[2],sep=''))
}

flatten_segment <- function(trip_segment){
  nstops <- trip_segment$duration
  names(trip_segment)
  length(trip_segment$segment)
  first <- trip_segment$segment[[1]]
  second <- trip_segment$segment[[2]]

  departure_time <- extract.time.info(first$leg[[1]]$departureTime)
  arrival_time <- extract.time.info(second$leg[[1]]$arrivalTime)

  second$duration

  carriers
  depart_time
  arrive_time

}

summarize_trip <- function(trip){

  price <- (trip$saleTotal %>% gsub('USD','',.) %>% as.numeric)
  trip$slice


  trip_summaries <- lapply(trip$slice,summarize_segment)

  startFlight <- trip$slice[[1]]
  returnFlight <- trip$slice[[2]]



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
