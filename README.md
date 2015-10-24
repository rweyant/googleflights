# googleflights

This is an R interface for the Google QPX Express API.  Still undergoing development and testing.  Has not been tested on Windows or Mac yet.  

## Installation

```{r}
devtools::install_github('rweyant/googleflights')
```

## Usage

```{r}
library(googleflights)
search(origin='DTW',dest='BCN',startDate='2015-11-27',returnDate='2015-12-11')
```
