#' Generate ONS taxonomy sitemap
#'
#' @param base_url
#'
#' @return
#' @export
#'
#' @examples
get_ons_taxonomy <- function(base_url = apis$ons$url$main) {
  req <- httr2::request(base_url) %>%
    httr2::req_url_path_append("taxonomy") %>%
    httr2::req_url_query(uri = "/", depth = 5)

  resp <- get_api_response_parsed(req)
}

convert_ons_taxonomy <- function(ons_taxonomy_all) {
  ons_taxonomy_all <- ons_taxonomy_all %>%
    tibblify::unnest_tree(
      id_col = uri,
      child_col = children
    ) %>%
    dplyr::rename(parent.uri = parent) %>%
    tidyr::unnest_wider(col = description) %>%
    dplyr::mutate(parent.uri = tidyr::replace_na(parent.uri, "/")) %>%
    dplyr::add_row(
      title = "Home",
      type = "home_page",
      parent.uri = NA,
      uri = "/",
      .before = 1
    ) %>%
    dplyr::mutate(stringPath = paste0("home", uri))

  uri_title_ref <- ons_taxonomy_all %>%
    dplyr::filter(uri %in% parent.uri) %>%
    dplyr::select(uri, title) %>%
    dplyr::rename(parent.uri = uri) %>%
    dplyr::rename(parent.title = title)


  ons_taxonomy_all <- ons_taxonomy_all %>%
    dplyr::left_join(uri_title_ref, by = "parent.uri") %>%
    dplyr::relocate(parent.title, title) %>%
    return(ons_taxonomy_all)
}

# " List ONS Product Pages
# "
# " @param base_url
# "
# " @return
# " @export
# "
# " @examples

list_ons_product_pages <- function(base_url = apis$ons$url$main) {
  ons_taxonomy <- get_ons_taxonomy(base_url)

  ons_product_pages <- ons_taxonomy %>%
    dplyr::filter(type == "product_page")

  return(ons_product_pages)
}

# " Generate ONS taxonomy sitemap
# "
# " @param base_url
# "
# " @return
# " @export
# "
# " @examples

list_ons_datasets <- function(base_url = apis$ons$url$main) {
  prod_pages <- list_ons_product_pages()

  prod_pages$req <- purrr::map2(
    .x = rep(base_url, length(prod_pages$uri)),
    .y = prod_pages$uri,
    .f = get_ons_page_data
  )
  return(prod_pages)
}

get_ons_page_data <- function(base_url, page_uri) {
  req <- httr2::request(base_url) %>%
    httr2::req_url_path_append("resolveDatasets") %>%
    httr2::req_url_query(uri = page_uri) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(simplyDataFrame = TRUE)

  return(req)
}

# " Generate ONS taxonomy sitemap
# "
# " @param base_url
# "
# " @return
# " @export
# "
# " @examples

get_api_response_parsed <- function(req) {
  resp <- req %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(simplifyDataFrame = FALSE)

  return(resp)
}

get_ons_ds_and_ts_uris <- function() {
  req <- httr2::request(apis$ons$url$main) %>%
    httr2::req_url_path_append("publishedIndex")

  resp <- req_perform(req) %>%
    resp_body_json()

  index <- resp$items %>%
    purrr::transpose() %>%
    tibble::as.tibble()

  datasets <- index %>%
    dplyr::filter(grepl("/datasets/", uri))

  return(out)
}


search_ons <- function(query,
                       type = c("timeseries","dataset"),
                       limit = 100) {
  req <- httr2::request(apis$ons$url$main) %>%
    httr2::req_url_path_append("search") %>%
    httr2::req_url_query(q = query, limit = limit)

   s <- simpleError("HTTP 404 NOT FOUND\n Try another search term")

  tryCatch(resp <- httr2::req_perform(req),
    error = function(e) warning(e),
    finally = if (exists("resp")) {
      resp_body <- resp %>%
        httr2::resp_body_json()

 resp_body$items <- resp_body$items %>%
        rlist::list.filter(type %in% type) %>%
      return(resp_body)



    } else {
      html_error <- httr2::last_response()

      warning(sprintf(
        "Error %s. No responses, please try another search term.",
        html_error$status_code)
      )
      return(html_error)
    }
  )
}
