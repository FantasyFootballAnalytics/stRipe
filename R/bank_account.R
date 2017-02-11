#' @export stripe_bank_account
stripe_bank_account <- R6::R6Class(
  "bank_account",
  public = list(
    # Object properties
    id= NULL, object = "bank_account", account = NULL, account_holder_name = NULL,
    account_holder_type = NULL, bank_name = NULL, country = NULL, currency = NULL,
    default_for_currency = NULL, fingerprint = NULL, last4 = NULL,
    metadata = list(), routing_number = NULL, status = NULL,

    # Initialize Method
    initialize = function(..., metadata = list()){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), "metadata")){
          self[[i_var]] <- eval.parent(init_vars[[i_var]])
        }
        self$metadata <- metadata
      }
    },

    # Create Method
    create = function(acct_number = NULL, routing_number = NULL, country = NULL,
                      currency = NULL, default_for_currency = NULL,
                      token_id = NULL, type = "source", metadata = list(),
                      resource_id){
      # Paramater validation
      if(missing(acct_number) & missing(token_id))
        stop("Please specify account number or token to create an account")
      if(is.null(acct_number) & is.null(token_id))
        stop("Please specify account number or token to create an account")

      type <- tryCatch(match.arg(type, c("source", "external_account")),
                       error = function(e){
                         stop("type has to be 'source' or 'external_account'")
                       })

      # Paramter for account creation
      create_param <- list()
      if(!is.null(token_id)){
        create_param[[type]] <- token_id
      } else {
        create_param[[paste0(type, "[object]")]] <- self$object
        create_param[[paste0(type, "[account_number]")]] <- acct_number
        create_param[[paste0(type, "[routing_number]")]] <- routing_number
        create_param[[paste0(type, "[country]")]] <- country
        create_param[[paste0(type, "[currency]")]] <- currency
      }
      if(!is.null(default_for_currency))
        create_param$default_for_currency <- default_for_currency

      if(length(metadata) > 0){
        for(meta_name in names(metadata))
          create_param[[paste0("metadata[", meta_name, "]")]] <- metadata[[meta_name]]
      }

      created_account <- stripe_request(
        private$account_url(resource_id = resource_id,
                            acct_type = type),
        request_body = create_param,
        request_type = "POST")

      for(new_var in names(created_account))
        self[[new_var]] <- created_account[[new_var]]
    },

    # Retrieve Method
    retrieve = function(account_id, resource_id, account_type = "source"){
      account_info <- stripe_request(
        privtate$account_url(acct_id = account_id, resource_id = resource_id,
                             acct_type = account_type))
      for(account_var in names(account_info)){
        self[[account_var]] <- account_info[[account_var]]
      }
    },

    # Update Method
    update = function(resource_id = NULL, acct_holder_name = NULL, acct_holder_type = NULL,
                      metadata = list(), default_for_currency = NULL){
      account_type <- switch(substring(resource_id, 1, 3),
                             "cus" = "source",
                             "acc" = "external_account")

      update_param <- list()
      if(account_type == "source"){
        if(!is.null(acct_holder_name))
          update_param$account_holder_name <- acct_holder_name
        if(!is.null(acct_holder_type))
          update_param$account_holder_name <- acct_holder_name
      } else {
        if(!is.null(default_for_currency))
          update_param$default_for_currency <- default_for_currency
      }

      if(length(metadata) > 0){
        for(meta_name in names(metadata))
          update_param[[paste0("metadata[", meta_name, "]")]] <- metadata[[meta_name]]
      }

      if(length(update_param) > 0){
        updated_account <- stripe_request(
          private$account_url(resource_id = resource_id, acct_id = self$id,
                              acct_type = account_type),
          request_body = update_param,
          request_type = "POST")

        for(account_var in names(updated_account)){
          self[[account_var]] <- updated_account[[account_var]]
        }
      }
    },

    # Verify Method
    verify = function(customer_id, amounts = NULL, verifcation_method = NULL){
      verify_param <- list()
      verify_url <- private$account_url(resource_id = customer_id, acct_id = self$id)
      url_path <-  httr::parse_url(verify_url)$path
      url_path <-  paste(url_path, "verify", sep = "/")
      verify_url <- httr::modify_url(verify_url, path = url_path)

      if(!is.null(amounts))
        verify_param$amounts <- amounts
      if(!is.null(verifcation_method))
        verify_param$verifcation_method <- verifcation_method

      if(length(verify_param) > 0){
        verified_account <- stripe_request(verify_url,
                                           request_body = verify_param,
                                           request_type = "POST")
        self$status <- verified_account$status
      }
    },

    # Delete Method
    delete = function(resource_id, account_id){
      account_type <- switch(substring(resource_id, 1, 3),
                             "cus" = "source",
                             "acc" = "external_account")
      deleted_account <- stripe_request(private$account_url(acct_id = account_id,
                                                            resource_id = resource_id,
                                                            acct_type = account_type),
                                        request_type = "DELETE")
    }
  ),
  private = list(
    account_url = function(acct_id = NULL, resource_id, acct_type = "source"){
      resource <- switch (acct_type,
         "source" = "customers",
         "external_account" = "accounts"
      )
      self_url <- app$request_url(resource)

      url_path <-  httr::parse_url(self_url)$path
      url_path <-  paste(url_path, resource_id, paste0(type, "s"), sep = "/")
      self_url <- httr::modify_url(self_url, path = url_path)

      if(!is.null(acct_id)){
        url_path <-  httr::parse_url(self_url)$path
        url_path <-  paste(url_path, acct_id, sep = "/")
        self_url <- httr::modify_url(self_url, path = url_path)
      }
      return(self_url)
    }
  )
)

newBankAccount <- function(...){stripe_bank_account$new(...)}
