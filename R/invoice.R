#' Stripe Line Item
stripe_line_item <- R6::R6Class(
  "line_item", lock_objects = FALSE,
  public = list(
    id = NULL,
    object = "line_item",
    amount = NULL,
    currency = NULL,
    description = NULL,
    discountable = NULL,
    livemode = NULL,
    metadata = list(),
    period =  list(),
    plan =  NULL,
    proration = NULL,
    quantity =  NULL,
    subscription = NULL,
    type = "invoiceitem",
    initialize = function(..., plan = NULL, metadata = list(), period = list()){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), c("metadata", "period", "plan"))){
          self[[i_var]] <- init_vars[[i_var]]
        }
        self$metadata <- metadata
        self$period <- period
        if(R6::is.R6(plan) | is.NULL(plan))
          self$plan <- plan
        else
          self$plan <- do.call("newPlan", plan)
      }
    }
  )
)

newInvoiceLine <- function(...){stripe_line_item$new(...)}

stripe_inv_item <- R6::R6Class(
  "invoiceitem",
  public = list(
    # Object Properties
    id = NULL, object = "invoiceitem", amount = NULL, currency = NULL,
    customer = NULL, date = NULL, description = NULL, discountable = NULL,
    invoice = NULL, livemode = NULL, metadata  = list(), period = list(),
    plan = NULL, proration = NULL, quantity = NULL, subscription = NULL,

    # Initialize method
    initialize = function(..., plan = NULL, metadata = list(), period = list()){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), c("metadata", "period", "plan"))){
          self[[i_var]] <- init_vars[[i_var]]
        }
        self$metadata <- metadata
        self$period <- period
        if(R6::is.R6(plan) | is.NULL(plan))
          self$plan <- plan
        else
          self$plan <- do.call("newPlan", plan)
      }
    },

    # Create method
    create = function(){
      create_param <- list(amount = self$amount,
                           currency = self$currency,
                           customer = self$customer)
      optional <- c("description", "discountable", "invoice", "metadata", "subscription")
      for(param in optional){
        if(!is.NULL(self[[param]]))
          create_param[[param]] <- self[[param]]
      }
      created_item <- stripe_request(item_url(),
                                     request_body = create_param,
                                     request_type = "POST")

      for(param in setdiff(names(created_item), "plan"))
        self[[param]] <- created_item[[param]]

      self$plan <- do.call("newPlan", created_item$plan)
    },

    # Retrieve Method
    retrieve = function(item_id){
      item_info <- stripe_request(private$item_url(item_id))

      for(item_var in setdiff(names(item_info), "plan")){
        self[[item_var]] <- item_info[[item_var]]
      }
      self$plan <- do.call("newPlan", item_info$plan)
    },

    # Update method
    update = function(amount = NULL, description = NULL, discountable = NULL,
                      metadata = list()){
      func_param <- as.list(match.call())[-1]

      if(length(func_param) == 0)
        return()

      update_param <- list()
      for(param_name in setdiff(names(func_param), "metadata"))
        update_param[param_name] <- func_param[[param_name]]

      if(any(names(func_param) == "metadata"))
        update_param$metadata <- metadata

      updated_item <- stripe_request(private$item_url(self$id),
                                     request_body = update_param,
                                     request_type = "POST")

      for(param in names(func_param))
        self[[param]] <- updated_item[[param]]
    },
    delete = function(){
      deleted_item <- stripe_request(private$item_url(self$id),
                                     request_type = "DELETE")
    }
  ),
  private = list(
    item_url = function(item_id = NULL){
      self_url <- app$request_url("invoiceitems")
      if(!is.NULL(item_id))
        url_path <-  paste(httr::parse_url(self_url)$path, item_id, sep = "/")
      self_url <- httr::modify_url(self_url, path = url_path)
      return(self_url)
    }
  )
)

newInvoiceItem <- function(...){stripe_inv_item$new(...)}

stripe_invoice <- R6::R6Class(
  "invoice",
  public = list(
    id = NULL, object = "invoice", amount_due = NULL, application_fee = NULL,
    attempt_count = NULL, attempted = NULL, charge = NULL, closed = NULL,
    currency = NULL, customer = NULL, date = NULL, description = NULL,
    discount = NULL, ending_balance = NULL, forgiven = NULL, lines = NULL,
    livemode = FALSE, metadata = list(), next_payment_attempt = NULL,
    paid = FALSE, period_end = NULL, period_start = NULL,
    receipt_number = NULL, starting_balance = NULL, statement_descriptor = NULL,
    subscription = NULL, subtotal = NULL, tax = NULL, tax_percent = NULL,
    total = NULL, webhooks_delivered_at = NULL,

    # Initialize method
    initialize = function(..., metadata = list(), lines = NULL){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), c("metadata", "lines"))){
          self[[i_var]] <- init_vars[[i_var]]
        }
        self$metadata <- metadata
        self$lines <- do.call("newList", lines)
      }
    },

    # Create method
    create = function(){
      create_params <- list(customer = self$customer)

      optional <- c("application_fee", "currency", "description", "metadata",
                    "statement_descriptor", "subscription", "tax_percent")

      for(param in optional){
        if(!is.null(self[[param]]))
          create_params[[param]] <- self[[param]]
      }

      created_invoice <- stripe_request(private$invoice_url(),
                                        request_body = create_params,
                                        request_type = "POST")

      for(param in setdiff(names(created_invoice), "lines")){
        self[[param]] <- created_invoice[[param]]
      }
      if(R6::is.R6(lines) | is.null(lines))
        self$lines <- lines
      else
        self$lines <- do.call("newList", created_invoice$lines)
    },

    # Retrieve Method
    retrieve = function(item_id){
      invoice_info <- stripe_request(private$invoice_url(item_id))
      item_vars <- names(invoice_info)
      for(item_var in setdiff(item_vars, "lines")){
        self[[item_var]] <- invoice_info[[item_var]]
      }
      self$lines <- do.call("newList", invoice_info$lines)
    },

    # Update Method
    update = function(application_fee = NULL, closed = NULL, description = NULL,
                      forgiven = NULL, metadata = NULL,
                      statement_descriptor = NULL, tax_percent = NULL){
      func_param <- as.list(match.call())[-1]

      if(length(func_param) == 0)
        return()

      update_param <- list()
      for(param_name in setdiff(names(func_param), "metadata"))
        update_param[param_name] <- func_param[[param_name]]

      if(any(names(func_param) == "metadata"))
        update_param$metadata <- metadata


      updated_invoice <- stripe_request(private$invoice_url(self$id),
                                     request_body = update_param,
                                     request_type = "POST")

      for(param in names(func_param))
        self[[param]] <- updated_item[[param]]
    },
    pay = function(){
      inv_url <- private$invoice_url(self$id)
      inv_url <- httr::modify_url(path = paste(httr::parse_url(inv_url)$path, "pay", sep = "/"))
      paid_invoice <- stripe_request(inv_url, request_type = "POST")
      item_vars <- setdiff(names(paid_invoice), "id")
      for(item_var in setdiff(item_vars, "lines")){
        self[[item_var]] <- paid_invoice[[item_var]]
      }
      self$lines <- do.call("newList", paid_invoice$lines)
    },
    get_lines = function(limit = NULL){
      inv_url <- private$invoice_url(self$id)
      inv_url <- httr::modify_url(path = paste(httr::parse_url(inv_url)$path, "lines", sep = "/"))
      request_query <- NULL
      if(!is.null(limit)){
        request_query$limit <- limit
        inv_url <- httr::modify_url(query = request_query)
      }
      inv_lines <- stripe_request(inv_url)

      self$lines <- do.call("newList", inv_lines)
    }
  ),
  private = list(
    invoice_url = function(item_id = NULL){
      self_url <- app$request_url("invoices")
      if(!is.NULL(item_id))
        url_path <-  paste(httr::parse_url(self_url)$path, item_id, sep = "/")
      self_url <- httr::modify_url(self_url, path = url_path)
      return(self_url)
    }
  )
)

newInvoice <- function(...){stripe_invoice$new(...)}


