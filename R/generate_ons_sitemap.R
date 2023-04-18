# " Generate ONS taxonomy sitemap
# "
# " @param base_url
# "
# " @return
# " @export
# "
# " @examples
get_ons_taxonomy <- function(base_url = apis$ons.main) {

  req <- httr2::request(base_url) %>%
    httr2::req_url_path_append('taxonomy') %>%
    httr2::req_url_query(uri = '/', depth = 5)

  resp <- get_api_response_parsed(req)

  ons_taxonomy_all <- resp %>%
    tibblify::unnest_tree(id_col = uri,
                          child_col = children) %>%
    dplyr::rename(parent.uri = parent) %>%
    tidyr::unnest_wider(col = description) %>%
    dplyr::mutate(parent.uri = tidyr::replace_na(parent.uri,'/')) %>%
    dplyr::add_row(title = 'Home',
                   type = 'home_page',
                   parent.uri = NA,
                   uri = '/',
                   .before = 1) %>%
    dplyr::mutate(stringPath = paste0('home',uri))

  uri_title_ref <- ons_taxonomy_all %>%
    dplyr::filter(uri %in% parent.uri) %>%
    dplyr::select(uri, title) %>%
    dplyr::rename(parent.uri = uri) %>%
    dplyr::rename(parent.title = title)


  ons_taxonomy_all <- ons_taxonomy_all %>%
    dplyr::left_join(uri_title_ref, by = 'parent.uri') %>%
    dplyr::relocate(parent.title,title) %>%


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
    dplyr::filter(type == "product_page") %>%
    dplyr::mutate(req = httr2::req_url_query(uri = '/', depth = 5))

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

list_ons_datasets <- function(base_url = apis$ons.main){

  prod_pages <- list_ons_product_pages()

  prod_pages$req <- httr2::request(base_url) %>%
    httr2::req_url_path_append('data') %>%
    httr2::req_url_query(uri = uri)

}

get_ons_page_data <- function(){


}

# " Generate ONS taxonomy sitemap
# "
# " @param base_url
# "
# " @return
# " @export
# "
# " @examples


build_ons_api_url <- function(base_url = apis$ons.main, endpoint, uri = "/", query = NULL){

  my_uri <- uri
  my_query <- query
  rm(uri, query)

  url <- httr::parse_url(base_url)
  url$path <- endpoint
  url$query <- list(uri = my_uri, query = my_query)

  url_built <- httr::build_url(url)

  return(url_built)
}

get_api_response_parsed<- function(req) {
  resp <- req %>%
    httr2::req_perform() %>%
    httr2::resp_body_json(simplifyDataFrame = TRUE)

  return(resp)
}
