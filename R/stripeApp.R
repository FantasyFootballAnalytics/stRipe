#' stripeApp object
#'
#' The \code{stripeApp} object represents the endpoint, version and API key used
#' for the package. You will need to use the \code{\link{setApiKey}} function to
#' set the API to the one you want to use.
#'
#' @section Object Properties:
#' \itemize{
#'  \item \code{endpoint}: The URL of the API endpoint
#'  \item \code{ver}: The version of the endpoint
#'  \item \code{apiKey}: The API key to be used for requests
#' }
#'
#' @section Methods:
#' \itemize{
#'  \item \code{request_url}: Generates a URL for the specified resource
#' }
#' @docType class
#' @format An R6 Class object
#' @import httr
#' @export
stripeApp <- R6::R6Class(
  public = list(
    endpoint = httr::parse_url("https://api.stripe.com/"),
    ver = "v1",
    apiKey = NULL,
    initialize = function(apiKey = as.character()){
      self$apiKey <- apiKey
    },
    request_url = function(resource = NULL){
      resource_path <- tolower(paste(self$ver, resource, sep = "/"))
      resource_url <- httr::modify_url(self$endpoint, path = resource_path)
      return(resource_url)
    }
  )
)


#' @export app
app <- stripeApp$new()

#' Set API key
#'
#' Sets API key to be used for requests to Stripe. You can find this via your
#' Stripe dashboard
#'
#' @param key_value Character string representing the API key to be used
#' @export
setApiKey <- function(key_value){
  app$apiKey <- key_value
}

#' Search
#'
#' Returns database objects using the Stripe Search
#'
#' @param resource Character string representing object type to search for
#' @param query Character string representing the query in the format specified at https://docs.stripe.com/search#search-query-language
#' @export
stripe_search <- function(resource, query = null){
  search_params <- list(query = query)
  search_results <- stripe_request(paste(app$request_url(resource), "search", sep = "/"),
                                   request_body = search_params,
                                   request_type = "GET")
  return(search_results)
}

stripe_request <- function(request_url, request_body = NULL, request_type = "GET",
                          idempotency_key = NULL){
  auth_string <- paste("Bearer", app$apiKey)
  request_func <- switch (request_type,
                          "GET" = httr::GET,
                          "PUT" = httr::PUT,
                          "POST" = httr::POST,
                          "DELETE" = httr::DELETE,
                          "PATCH" = httr::PATCH
  )

  header_list <- list(Authorization = auth_string,
                      "Content-Type" = "application/x-www-form-urlencoded")

    if(!is.null(idempotency_key))
    header_list$`Idempotency-Key` <- idempotency_key

  request_header <- do.call(httr::add_headers, header_list)
  request_url <- httr::modify_url(request_url, query =  request_body)

  request_response <- request_func(request_url, request_header)
  request_status <- httr::status_code(request_response)
  request_content <- httr::content(request_response)

  if(request_status == 200){
    return(request_content)
  } else {
    stop(stripe_error(request_content$error, request_status), call. = FALSE)
  }
}
