#' Search the ONS API
#'
#' This function searches the ONS API for timeseries or datasets based on a query string.
#'
#' @param query The search query string.
#' @param type The type(s) of items to search for. Must be one or more of "timeseries" or "dataset".
#' @param limit The maximum number of items to return.
#'
#' @return A list of search results.
#' @export
#'
#' @examples
#' search_ons("GDP")
#'
#' @references
#' \url{[ONS Zebedee Reader Search API]()}
#'
#' @seealso
#' \code{\link{get_ons_data}}, \code{\link{get_ons_metadata}}
#'
#' @import httr2
#' @import rlist
#' @importFrom utils simpleError

search_ons <- function(query,
                       type = c("timeseries","dataset"),
                       limit = 100) {

  # Build the API request using httr2 package
  req <- httr2::request(apis$ons$url$main) %>%
    httr2::req_url_path_append("search") %>%
    httr2::req_url_query(q = query, limit = limit) %>%
    httr2::req_paginate()


  # Define a custom error message
  s <- simpleError("HTTP 404 NOT FOUND\n Try another search term")


  # Try to send the API request and catch any errors
  tryCatch(resp = httr2::paginate_req_perform(req),
           error = function(e) warning(e),
           finally = if (exists("resp")) {

             # If the request is successful, parse the response body as JSON
             resp_body <- resp |>
               httr2::resp_body_json()

             # Filter the response items based on the specified type(s)
             resp_body$items <- resp_body$items |>
               rlist::list.filter(type %in% type)

               return(resp_body)

           } else {

             # If the request fails, capture the HTML error response
             html_error <- httr2::last_response()

             # Show a warning message with the error status code
             warning(sprintf(
               "Error %s. No responses, please try another search term.",
               html_error$status_code)
             )
             # Return the HTML error response

             return(html_error)
           }
  )
}
