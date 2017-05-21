#' @export stripe_token
stripe_token <- R6::R6Class(
  "token",
  public = list(
    id = NULL,
    object = "token",
    card = NULL ,
    bank_account = NULL,
    client_ip = NULL,
    created = NULL,
    livemode = FALSE,
    type = NULL,
    used = FALSE,
    email = NULL,
    initialize = function(..., card = NULL, bank_account = NULL){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), c("card", "bank_account"))){
          self[[i_var]] <- init_vars[[i_var]]
        }
        if(is.null(card) | R6::is.R6(card))
          self$card <- card
        else
          self$card <- do.call("newCard", card)

        if(is.null(bank_account) | R6::is.R6(bank_account))
          self$bank_account <- bank_account
        else
          self$bank_account <- do.call("newBankAccount", bank_account)

      }
    },
    create_card = function(card_token = NULL, card_exp_month = NULL,
                           card_exp_year = NULL, card_number = NULL, card_cvc = NULL,
                           card_holder, address_city = NULL, address_country = NULL,
                           address_line1 = NULL, address_line2 = NULL,
                           address_state = NULL, address_zip = NULL, currency = NULL){

      func_param <- as.list(match.call())[-1]
      if(!is.null(card_token))
        create_param <- list(card = card_token)
      else {
        create_param <- list(card = list(exp_month = card_exp_month, exp_year = card_exp_year,
                             cvc = card_cvc, number = card_number, name = card_holder))
        optional <- c("address_city", "address_country", "address_line1", "address_line2",
                      "address_state", "address_zip", "currency")

        for(param in optional){
          if(!is.null(func_param[[param]]))
            create_param[[param]] <- func_param[[param]]
        }
      }

      created_token <- stripe_request(private$token_url(),
                                      request_body = create_param,
                                      request_type = "POST")

      for(param in setdiff(names(created_token), c("card", "bank_account")))
        self[[param]] <- created_token[[param]]

      if(any(names(created_token) == "card"))
        self$card <- do.call("newCard", created_token$card)

      if(any(names(created_token) == "bank_account"))
        self$bank_account <- do.call("newBankAccount", created_token$bank_account)
    },
    create_bank = function(acct_number, country, currency, routing_number,
                            acct_holder_name = NULL, acct_holder_type = NULL){
      create_params <- list(bank_account = list(account_number = acct_number,
                                                country = country,
                                                currency = currency,
                                                routing_number = routing_number
                                                ))
      if(!is.null(acct_holder_name))
        create_params$bank_account$account_holder_name = acct_holder_name

      if(!is.null(acct_holder_type))
        create_params$bank_account$account_holder_type = acct_holder_type

      created_token <- stripe_request(private$token_url(),
                                      request_body = create_param,
                                      request_type = "POST")

      for(param in setdiff(names(created_token), c("card", "bank_account")))
        self[[param]] <- created_token[[param]]

      if(any(names(created_token) == "card"))
        self$card <- do.call("newCard", created_token$card)

      if(any(names(created_token) == "bank_account"))
        self$bank_account <- do.call("newBankAccount", created_token$bank_account)
    },
    retrieve = function(token_id){
      token_info <- stripe_request(private$token_url(token_id))

      print(token_info$card)
      for(param in setdiff(names(token_info), c("card", "bank_account")))
        self[[param]] <- token_info[[param]]

      if(any(names(token_info) == "card"))
        self$card <- do.call("newCard", token_info$card)

      if(any(names(token_info) == "bank_account"))
        self$bank_account <- do.call("newBankAccount", token_info$bank_account)
    }

  ),
  private = list(
    token_url = function(token_id = NULL){
      self_url <- app$request_url("tokens")

      if(!is.null(token_id)){
        url_path <-  httr::parse_url(self_url)$path
        url_path <-  paste(url_path, token_id, sep = "/")
        self_url <- httr::modify_url(self_url, path = url_path)
      }
      return(self_url)
    }
  )
)
newToken <- function(...)stripe_token$new(...)
