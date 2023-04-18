## code to prepare `apis` dataset goes here

apis <- new.env(parent = emptyenv())

apis$ons$department <- "Office for National Statistics"
apis$ons$url$main <- "https://api.ons.gov.uk"
apis$ons$url$beta <- "https://api.beta.ons.gov.uk/v1"

apis$dfe$department <- "Department for Education"
apis$dfe$url <- "https://ees-api-mock.ambitiousocean-cb084d07.uksouth.azurecontainerapps.io"

usethis::use_data(apis, overwrite = TRUE)
