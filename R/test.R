origin <- 'DTW'
dest <- 'BCN'
numPassengers <- 1
startDate <- '2015-11-27'
returnDate <- '2015-12-05'

set_apikey('AIzaSyCG0HuvV6Kr58r-gi_gver43pWizTfJ1Uk')
content <-
  search(origin = origin,
         dest = dest,
         startDate = startDate,
         returnDate = returnDate)

trips <- content$trips$tripOption
trip <- trips[[1]]

summarize_segment(trip$slice[[1]])
summarize_segment(trip$slice[[2]])
