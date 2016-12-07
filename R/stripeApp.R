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

#' @export setApiKey
setApiKey <- function(key_value = as.character()){
  app$apiKey <- key_value
}
#' @export stripe_request
stripe_request <- function(request_url, request_body = NULL, request_type = "GET"){
  auth_string <- paste("Bearer", app$apiKey)
  request_func <- switch (request_type,
                          "GET" = httr::GET,
                          "PUT" = httr::PUT,
                          "POST" = httr::POST,
                          "DELETE" = httr::DELETE,
                          "PATCH" = httr::PATCH
  )


  request_url <- httr::modify_url(request_url, query =  request_body)


  request_response <- request_func(request_url,
                                   httr::add_headers(Authorization = auth_string, "Content-Type" = "application/x-www-form-urlencoded"))
  request_status <- httr::status_code(request_response)
  request_content <- httr::content(request_response)
  if(request_status == 200){
    return(request_content)
  } else {
    stop(stripe_error(request_content$error, request_status), call. = FALSE)
  }
}
