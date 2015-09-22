
#' Summarize a trip_segment
#'
#' @param trip_segment
summarize_segment <- function(trip_segment){

  origin <- sapply(trip_segment$segment, function(x) x$leg[[1]]$origin)
  destination <- sapply(trip_segment$segment, function(x) x$leg[[1]]$destination)

  flight_number <- sapply(trip_segment$segment,function(x) x$flight %>% unlist %>% paste(collapse='-'))

  carrier <- sapply(trip_segment$segment,function(x) x$flight$carrier)

  duration <- sapply(trip_segment$segment,function(x) x$duration)

  cabin <- sapply(trip_segment$segment,function(x) x$cabin)

  cbind.data.frame(n_stops=length(origin)-1,origin,destination,flight_number,carrier,duration,cabin,stringsAsFactors=FALSE)
}

extract.time.info <- function(x){

  first.split <- str_split_fixed(x,'T',n=2)
  if(grepl('\\-',first.split[2])) {
    split_char <- '\\-'
  } else if(grepl('\\+',first.split[2])) {
    split_char <- '\\+'
  } else stop('extract.time.info: Error parsing time.')

  second.split <-  str_split_fixed(first.split[2],split_char,n=2)
  timezone <- paste(substr(split_char,2,3),str_replace(second.split[2],':',''),sep='')

  strptime(paste(first.split[1],second.split[1],timezone),format="%Y-%m-%d %H:%M %z")

}

flatten_segment <- function(trip_segment){

  nstops <- length(trip_segment$segment)
  first <- trip_segment$segment[[1]]
  last <- trip_segment$segment[[num_legs]]

  # First Departure
  departure_time <- extract.time.info(first$leg[[1]]$departureTime)

  # Final Arrival
  arrival_time <- extract.time.info(last$leg[[1]]$arrivalTime)

  total_duration_hours <- trip_segment$duration/60

  carriers <- paste(unique(sapply(trip_segment$segment, function(x) x$flight$carrier)),collapse=',')

  cabin <- paste(unique(sapply(trip_segment$segment,function(x) x$cabin)),collapse=',')

  data.frame(nstops,departure_time,arrival_time,total_duration_hours,carriers,cabin,stringsAsFactors=FALSE)

}

summarize_trip <- function(trip){

  price <- (trip$saleTotal %>% gsub('USD','',.) %>% as.numeric)

  trip_summaries <- lapply(trip$slice,summarize_segment)

  flat_summaries <- sapply(trip$slice,flatten_segment,simplify=TRUE) %>% t %>% as.data.frame

  nstops <- flat_summaries$nstops %>% unlist
  nstops_departing <- nstops[1]
  nstops_returning <- nstops[2]
  carriers <- flat_summaries$carriers %>% unlist %>% unique %>% paste(.,collapse=',')
  cabins <- flat_summaries$cabin %>% unlist %>% unique %>% paste(.,collapse=',')

  origin <- trip_summaries[[1]]$origin[1]
  destination <- trip_summaries[[1]]$destination[length(trip_summaries[[1]]$destination)]

  data.frame(price,origin,destination,nstops_departing,nstops_returning,carriers,cabins,stringsAsFactors=FALSE)
}

# simplify results of all
simplify_ <- function(content){

  # Each trip
  trips <- content$trips$tripOption
  # Extract information from each trip.
  summary <-
    sapply(trips,summarize_trip) %>% t
}
