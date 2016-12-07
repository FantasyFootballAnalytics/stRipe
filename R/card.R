
#' @export stripe_card
stripe_card <- R6::R6Class(
  "card",
  public = list(
    # Data elements
    id = NULL, object = "card", address_city = NULL, address_country = NULL,
    address_line1 = NULL, address_line1_check = NULL, address_line2 = NULL,
    address_state = NULL, address_zip = NULL, address_zip_check = NULL,
    brand = NULL, country = NULL, customer = NULL, cvc_check = NULL,
    dynamic_last4 = NULL, exp_month = NULL, exp_year = NULL, funding = NULL,
    last4 = NULL, metadata = list(), name = NULL, tokenization_method = NULL,

    # Initialize function
    initialize = function(..., metadata = list()){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), "metadata")){
          self[[i_var]] <- init_vars[[i_var]]
        }
        self$metadata <- metadata
      }
    },

    # Create function will create the card at Stripe
    create = function(customer_id, card_number, card_cvc){
      card_param <- list(
        object = "card",
        exp_month = self$exp_month,
        exp_year = self$exp_year,
        number = card_number,
        cvc = card_cvc,
        name = self$name)

      for(card_var in c("address_city", "address_country", "address_line1",
                        "address_line2", "address_state", "address_zip")){
        if(!is.null(self[[card_var]]))
          card_param[[card_var]] = self[[card_var]]
      }

      if(length(self$metadata) > 0)
        card_param$metadata <- self$metadata

      new_card <- stripe_request(private$card_url(), request_body = card_param,
                                 request_type = "POST" )

      new_card_vars <- setdiff(names(new_card), card_param)
      for(new_var in new_card_vars)
        self[[new_var]] <- new_card[[new_var]]
    },

    # Retrieve function will retrieve card information from Stripe based on
    # the "id" in the object and update all other elements with the information
    # retrieved from Stripe.
    retrieve = function(card_id){
      card_info <- stripe_request(private$card_url(card_id))

      for(card_var in names(card_info)){
        self[[card_var]] <- card_info[[card_var]]
      }
    },

    # Update function will update the card information provided at Stripe
    update = function(address_city = NULL, address_country = NULL,
                      address_line1 = NULL, address_line2  = NULL,
                      address_state = NULL, address_zip = NULL,
                      exp_month = NULL, exp_year = NULL,
                      metadata = list(), name = NULL){

      func_param <- as.list(match.call())[-1]

      if(length(func_param) == 0)
        return()

      update_param <- list(id = self$id)
      for(param_name in setdiff(names(func_param), "metadata"))
        update_param[param_name] <- func_param[[param_name]]

      if(any(names(func_param) == "metadata"))
        update_param$metadata <- metadata

      udpated_card <- stripe_request(private$card_url(self$id), request_body = update_param,
                     request_type = "POST")
      for(param_name in names(func_param))
        self[[param_name]] <- update_card[[param_name]]

    },

    # Delete function to remove the card from Stripe
    delete = function(){
      del_response <- stripe_request(private$card_url(self$id),
                                     request_type = "DELETE")
    }
  ),
  private = list(
    # Card url function will generate the url for calling the card API
    card_url = function(card_id = NULL){
      self_url <- app$request_url("customers")
      url_path <-  paste(httr::parse_url(self_url)$path, self$customer, "sources", sep = "/")
      if(!is.null(card_id))
        url_path <-  paste(url_path, card_id, sep = "/")
      self_url <- httr::modify_url(self_url, path = url_path)
      return(self_url)
    }
  )
)

newCard <- function(...){stripe_card$new(...)}
