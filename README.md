# googleflights

This is an R interface for the [Google QPX Express API](https://www.google.com/flights/).  Still undergoing development and testing.  Has not been tested on Windows or Mac yet.  More documentation on the API can be found [here](https://developers.google.com/qpx-express/).

## Installation

```{r}
devtools::install_github('rweyant/googleflights')
```

## Usage

```{r}
library(googleflights)

# Set API key in a place all the functions have access to
set_apikey(YOUR_APIKEY)

search(origin='DTW',dest='BCN',startDate='2015-11-27',returnDate='2015-12-11')
```
