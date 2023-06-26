get_ons_timeseries <- function(cdid ){
  if(missing(cdid)) {
    error("Please specify at least one cdid")
  }

 ts <- as.list(cdid) %>%
   purrr::transpose()

 ts <- ts %>%
   map(req = httr2::)


}

