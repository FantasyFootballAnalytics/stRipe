#' @export stripe_token
stripe_token <- R6::R6Class(
  "token",
  public = list(
    id = as.character(),
    object = "token",
    card = NULL ,
    client_ip = NULL,
    created = NULL,
    livemode = FALSE,
    type = as.character(),
    used = FALSE,
    initialize = function(id = NA, card =  stripe_card$new(), client_ip = NULL,
                          created = NULL, livemode = FALSE, type = NULL,
                          used = FALSE){
      self$id <- id
      self$card <- card
      self$client_ip <- client_ip
      self$created <- created
      self$livemode <- livemode
      self$type <- type
      self$used <- used
    }
  )
)
