#' Customer object
#'
#' The \code{stripe_customer} object represents the data that is required to
#' manage customers at Stripe. This object allows you to perform recurring
#' charges and track multiple charges that are associated with the same customer.
#' You can use this object to create, delete, and update customers.
#'
#' @section Methods:
#' \itemize{
#'  \item \code{create}: Create a new customer
#'  \item \code{update}: Update customer information
#'  \item \code{retrieve}: Retrieve information on existing customer.
#'  \item \code{delete}: Delete a customer
#' }
#'
#' @docType class
#' @format An R6 object class
#' @include card.R subscription.R discount.R object_list.R
#' @export
stripe_customer <- R6::R6Class(
  "customer",
  public = list(
    id = NULL, object = "customer",
    account_balance = NULL, business_vat_id = NULL, created = NULL, currency = NULL,
    default_source = NULL, delinquent = NULL, description = NULL, discount = NULL,
    email = NULL, livemode = NULL, metadata = list(), shipping = NULL, sources = NULL,
    subscriptions = NULL,

    # Initialize method
    initialize = function(..., metadata = NULL, discount = NULL, shipping = NULL,
                          sources = NULL, subscriptions = NULL){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), c("metadata", "discount", "shipping", "sources", "subscriptions"))){
          self[[i_var]] <- eval.parent(init_vars[[i_var]])
        }
        self$metadata <- metadata
        self$shipping <- shipping

        if(R6::is.R6(discount) | is.null(discount))
          self$discount <- discount
        else
          self$discount <- do.call("newDiscount", discount)

        if(R6::is.R6(sources) | is.null(sources))
          self$sources <- sources
        else
          self$sources <- do.call("newList", sources)

        if(R6::is.R6(subscriptions) | is.null(subscriptions))
          self$subscriptions <- subscriptions
        else
          self$subscriptions <- do.call("newList", subscriptions)
      }

    },

    # Create method
    create = function(account_balance = NULL, business_vat_id = NULL,
                      description = NULL, email = NULL,
                      plan = NULL, quantity = NULL, tax_percent = NULL,
                      trial_end = NULL, coupon = NULL, metadata = list(),
                      ship_name = NULL, ship_city = NULL, ship_country = NULL,
                      ship_line1 = NULL, ship_line2 = NULL,
                      ship_state = NULL, ship_zip = NULL, ship_phone = NULL,
                      card_token = NULL, exp_month = NULL,
                      exp_year = NULL, number = NULL, cvc = NULL,
                      name = NULL, address_city = NULL, address_country = NULL,
                      address_line1 = NULL, address_line2 = NULL,
                      address_state = NULL, address_zip = NULL, currency = NULL,
                      default_for_currency = NULL, card_metadata = list()){
      func_param <- as.list(match.call())[-1]
      create_param <- list()
      cust_vars <- c("account_balance", "business_vat_id", "description", "email",
                     "shipping")

      ship_vars <- c("ship_name", "ship_city", "ship_country", "ship_line1",
                     "ship_line2", "ship_state", "ship_zip", "ship_phone")

      for(param in intersect(names(func_param), cust_vars)){
        if(!is.null(eval.parent(func_param[[param]])))
          create_param[[param]] <- eval.parent(func_param[[param]])
      }


      for(param in intersect(names(func_param), ship_vars)){
        if(!is.null(eval.parent(func_param[[param]]))){
          ship_param <- switch(param, "ship_name" = "name", "ship_city" = "city",
                               "ship_country" = "country", "ship_line1" = "line1",
                               "ship_line2" = "line2", "ship_state" = "state",
                               "ship_zip" = "postal_code", "ship_phone" = "phone")
          if(ship_param %in% c("name", "phone"))
            create_param[[paste0("shipping[", ship_param, "]")]] <- eval.parent(func_param[[param]])
          else
            create_param[[paste0("shipping[address[", ship_param, "]]")]] <- eval.parent(func_param[[param]])
        }
      }

      if(length(metadata) > 0){
        for(m_name in names(metadata))
          create_param[[paste0("metadata[", m_name, "]")]] <- metadata[[m_name]]
      }

      if(!is.null(plan)){
        create_param$plan <- plan
      }

      if(!is.null(quantity)){
        create_param$quantity <- quantity
      }

      if(!is.null(tax_percent)){
        create_param$tax_percent <- tax_percent
      }

      if(!is.null(trial_end)){
        create_param$trial_end <- trial_end
      }

      if(!is.null(coupon)){
        create_param$coupon <- coupon
      }

      if(!is.null(card_token)){
        create_param$source <- card_token
      } else {
        source_dict <- get_card_source(func_param)
        if(!is.null(source_dict)){
          create_param["source[object]"] <- "card"
          for(src_name in names(source_dict))
            create_param[[src_name]] <- source_dict[[src_name]]
        }
      }

      if(length(create_param) > 0){

        new_customer <- stripe_request(private$customer_url(),
                                       request_body = create_param,
                                       request_type = "POST" )

        new_cust_vars <- names(new_customer)

        for(new_var in setdiff(new_cust_vars, c("discount", "sources", "subscriptions")))
          self[[new_var]] <- new_customer[[new_var]]

        if(!is.null(new_customer$discount))
          self$discount <- do.call("newDiscount", new_customer$discount)

        if(length(new_customer$sources) > 0)
          self$sources <- do.call("newList", new_customer$sources)

        if(length(new_customer$subscriptions) > 0)
          self$subscriptions <- do.call("newList", new_customer$subscriptions)
      }
      self
    },

    # Retrieve Method
    retrieve = function(customer_id){
      customer_info <- stripe_request(private$customer_url(customer_id))

      cust_vars <- names(customer_info)
      for(new_var in setdiff(cust_vars, c("discount", "sources", "subscriptions")))
        self[[new_var]] <- customer_info[[new_var]]

      if(!is.null(customer_info$discount))
        self$discount <- do.call("newDiscount", customer_info$discount)

      if(length(customer_info$sources) > 0)
        self$sources <- do.call("newList", customer_info$sources)

      if(length(customer_info$subscriptions) > 0)
        self$subscriptions <- do.call("newList", customer_info$subscriptions)

      invisible(self)
    },

    # Update Method
    update = function(account_balance = NULL, business_vat_id = NULL, metadata = list(),
                      default_source = NULL, coupon = NULL, description = NULL,
                      email = NULL, ship_name = NULL, ship_city = NULL,
                      ship_country = NULL, ship_line1 = NULL, ship_line2 = NULL,
                      ship_state = NULL, ship_zip = NULL, ship_phone = NULL,
                      card_token = NULL, card_exp_month = NULL,
                      card_exp_year = NULL, card_number = NULL, card_cvc = NULL,
                      card_holder = NULL, address_city = NULL, address_country = NULL,
                      address_line1 = NULL, address_line2 = NULL,
                      address_state = NULL, address_zip = NULL, currency = NULL,
                      default_for_currency = NULL, card_metadata = list()){

      func_param <- as.list(match.call())[-1]

      if(length(func_param) == 0)
        return()
      update_param <- list()

      ship_vars <- c("ship_name", "ship_city", "ship_country", "ship_line1",
                     "ship_line2", "ship_state", "ship_zip", "ship_phone")

      source_dict <- get_card_source(func_param)

      if(!is.null(source_dict)){
        for(src_name in names(source_dict))
          update_param[[src_name]] <- source_dict[[src_name]]
      }

      if(any(names(func_param) == "metadata"))
        if(is.null(metadata)){
          update_param[["metadata"]] <- ""
        } else {
          if(length(metadata) > 0)
            for(m_name in names(metadata))
              update_param[[paste0("metadata[", m_name, "]")]] <- metadata[[m_name]]
        }
      for(param in c("account_balance", "business_vat_id", "default_source",
                     "coupon", "description"))
        update_param[[param]] <- eval.parent(func_param[[param]])

      for(param in intersect(names(func_param), ship_vars)){
        if(!is.null(eval.parent(func_param[[param]]))){
          ship_param <- switch(param, "ship_name" = "name", "ship_city" = "city",
                               "ship_country" = "country", "ship_line1" = "line1",
                               "ship_line2" = "line2", "ship_state" = "state",
                               "ship_zip" = "postal_code", "ship_phone" = "phone")
          if(ship_param %in% c("name", "phone"))
            update_param[[paste0("shipping[", ship_param, "]")]] <- eval.parent(func_param[[param]])
          else
            update_param[[paste0("shipping[address[", ship_param, "]]")]] <- eval.parent(func_param[[param]])
        }}

      udpated_cust <- stripe_request(private$customer_url(self$id),
                                     request_body = update_param,
                                     request_type = "POST")
      cust_vars <- names(udpated_cust)
      for(new_var in setdiff(cust_vars, c("discount", "sources", "subscriptions")))
        self[[new_var]] <- udpated_cust[[new_var]]

      if(!is.null(udpated_cust$discount))
        self$discount <- do.call("newDiscount", udpated_cust$discount)

      if(length(udpated_cust$sources) > 0)
        self$sources <- do.call("newList", udpated_cust$sources)

      if(length(udpated_cust$subscriptions) > 0)
        self$subscriptions <- do.call("newList", udpated_cust$subscriptions)


    },
    delete = function(customer_id){
      deleted_customer <- stripe_request(private$customer_url(customer_id),
                                         request_type = "DELETE")
      if(deleted_customer$deleted)
        cat("Deleted customer: ", deleted_customer$id)
    }
  ),
  private = list(
    customer_url = function(customer_id = NULL){
      self_url <- app$request_url("customers")

      if(!is.null(customer_id)){
        url_path <-  httr::parse_url(self_url)$path
        url_path <-  paste(url_path, customer_id, sep = "/")
        self_url <- httr::modify_url(self_url, path = url_path)
      }
      return(self_url)
    }
  )
)

newCustomer <- function(...)stripe_customer$new(...)

#' Create a customer
#'
#' Create a new customer on Stripe.
#'
#' @return A \code{\link{stripe_customer}} object representing the created customer.
#'
#' @param account_balance An integer amount in cents that is the starting account
#' balance for your customer. A negative amount represents a credit that will be
#' used before attempting any charges to the customer’s card; a positive amount
#' will be added to the next invoice.
#' @param business_vat_id The customer’s VAT identification number. If you are
#' using Relay, this field gets passed to tax provider you are using for your
#' orders. This can be unset by updating the value to \code{NULL}.
#' @param description An arbitrary string that you can attach to a customer
#' object. It is displayed alongside the customer in the dashboard. This can be
#' unset by updating the value to \code{NULL}.
#' @param email Customer’s email address. It’s displayed alongside the customer
#' in your dashboard and can be useful for searching and tracking. This can be
#' unset by updating the value to \code{NULL}.
#' @param plan The identifier of the plan to subscribe the customer to.
#' If provided, the returned customer object will have a list of subscriptions
#' that the customer is currently subscribed to.
#' If you subscribe a customer to a plan without a free trial, the customer must
#' have a valid card as well.
#' @param quantity The quantity you’d like to apply to the subscription you’re
#' creating (if you pass in a plan). For example, if your plan is 10 cents/user/month,
#' and your customer has 5 users, you could pass 5 as the quantity to have the
#' customer charged 50 cents (5 x 10 cents) monthly. Defaults to 1 if not set.
#' Only applies when the plan parameter is also provided.
#' @param tax_percent A positive decimal (with at most four decimal places)
#' between 1 and 100. This represents the percentage of the subscription invoice
#' subtotal that will be calculated and added as tax to the final amount each
#' billing period. For example, a plan which charges $10/month with a tax_percent
#' of 20.0 will charge $12 per invoice. Can only be used if a plan is provided
#' @param trial_end Unix timestamp representing the end of the trial period the
#' customer will get before being charged. If set, trial_end will override the
#' default trial period of the plan the customer is being subscribed to.
#' The special value now can be provided to end the customer’s trial immediately.
#' Only applies when the plan parameter is also provided.
#' @param coupon If you provide a coupon code, the customer will have a discount
#' applied on all recurring charges. Charges you create through the API will not
#' have the discount.
#' @param metadata A set of key/value pairs that you can attach to a customer
#' object. It can be useful for storing additional information about the customer
#' in a structured format. This can be unset by updating the value to \code{NULL}.
#' @param ship_name Name on shipping address
#' @param ship_city City of shipping address
#' @param ship_country Country of shipping address
#' @param ship_line1 Address line 1 of shipping address
#' @param ship_line2 Address line 2 of shipping address
#' @param ship_state State/Region/Territory of shipping address
#' @param ship_zip Zip/Postal Code of shipping address
#' @param ship_phone Phone number associated with shipping address
#' @param card_token Token ID of card token representing the customer's credit
#' card details. You can pass credit card details if you don't have a token
#' created yet.
#' @param exp_month Two digit number representing the card's expiration month.
#' @param exp_year Two or four digit number representing the card's expiration year.
#' @param number The card number, as a string without any separators.
#' @param cvc Card security code.
#' @param name Cardholder's full name
#' @param address_city City of billing address
#' @param address_country Country of billing address
#' @param address_line1 Address line 1 of billing address
#' @param address_line2 Address line 2 of billing address
#' @param address_state State of billing address
#' @param address_zip Zip/Postal Code of billing address
#' @param currency Required when adding a card to an account (not applicable to
#' customers or recipients). The card (which must be a debit card) can be used
#' as a transfer destination for funds in this currency. Currently, the only
#' supported currency for debit card transfers is usd.
#' @param default_for_currency Only applicable on accounts (not customers or
#' recipients). If you set this to true (or if this is the first external account
#' being added in this currency) this card will become the default external
#' account for its currency.
#' @param card_metadata  set of key/value pairs that you can attach to a card object.
#' It can be useful for storing additional information about the card in a structured format.
#' @export
create_customer <- function(account_balance = NULL, business_vat_id = NULL,
                            description = NULL, email = NULL,
                            plan = NULL, quantity = NULL, tax_percent = NULL,
                            trial_end = NULL, source = NULL, coupon = NULL, metadata = list(),
                            ship_name = NULL, ship_city = NULL, ship_country = NULL,
                            ship_line1 = NULL, ship_line2 = NULL,
                            ship_state = NULL, ship_zip = NULL, ship_phone = NULL,
                            card_token = NULL, exp_month = NULL,
                            exp_year = NULL, number = NULL, cvc = NULL,
                            name = NULL, address_city = NULL, address_country = NULL,
                            address_line1 = NULL, address_line2 = NULL,
                            address_state = NULL, address_zip = NULL, currency = NULL,
                            default_for_currency = NULL, card_metadata = list()){
  create_args <- as.list(match.call())[-1]
  newCustomer <- stripe_customer$new()
  add_customer <- newCustomer$create

  do.call("add_customer", create_args)
  return(newCustomer)
}

#' Update a customer
#'
#' Updates the specified customer by setting the values of the parameters passed.
#' Any parameters not provided will be left unchanged. If you pass credit card
#' information, the customer's active card to be used for all charges in the
#' future will be updated. When you update a customer to a new valid card, then
#' the latest unpaid, unclosed invoice for the subscription will be retried.
#' This retry will not count as an automatic retry, and will not affect the next
#' regularly scheduled payment for the invoice. Invoices pertaining to unpaid
#' subscriptions, or invoices pertaining to canceled subscriptions, will not be
#' retried as a result of updating the customer's credit card information.
#'
#' @return A \code{\link{stripe_customer}} object representing the updated
#' customer
#' @param id ID of customer to be updated
#' @param account_balance An integer amount in cents that is the starting account
#' balance for your customer. A negative amount represents a credit that will be
#' used before attempting any charges to the customer’s card; a positive amount
#' will be added to the next invoice.
#' @param business_vat_id The customer’s VAT identification number. If you are
#' using Relay, this field gets passed to tax provider you are using for your
#' orders. This can be unset by updating the value to \code{NULL}.
#' @param description An arbitrary string that you can attach to a customer
#' object. It is displayed alongside the customer in the dashboard. This can be
#' unset by updating the value to \code{NULL}.
#' @param email Customer’s email address. It’s displayed alongside the customer
#' in your dashboard and can be useful for searching and tracking. This can be
#' unset by updating the value to \code{NULL}.
#' @param coupon If you provide a coupon code, the customer will have a discount
#' applied on all recurring charges. Charges you create through the API will not
#' have the discount.
#' @param metadata A set of key/value pairs that you can attach to a customer
#' object. It can be useful for storing additional information about the customer
#' in a structured format. This can be unset by updating the value to \code{NULL}.
#' @param default_source ID of source to make the customer’s new default for
#' invoice payments
#' @param ship_name Name on shipping address
#' @param ship_city City of shipping address
#' @param ship_country Country of shipping address
#' @param ship_line1 Address line 1 of shipping address
#' @param ship_line2 Address line 2 of shipping address
#' @param ship_state State/Region/Territory of shipping address
#' @param ship_zip Zip/Postal Code of shipping address
#' @param ship_phone Phone number associated with shipping address
#' @param card_token Token ID of card token representing the customer's credit
#' card details. You can pass credit card details if you don't have a token
#' created yet.
#' @param exp_month Two digit number representing the card's expiration month.
#' @param exp_year Two or four digit number representing the card's expiration year.
#' @param number The card number, as a string without any separators.
#' @param cvc Card security code.
#' @param name Cardholder's full name
#' @param address_city City of billing address
#' @param address_country Country of billing address
#' @param address_line1 Address line 1 of billing address
#' @param address_line2 Address line 2 of billing address
#' @param address_state State of billing address
#' @param address_zip Zip/Postal Code of billing address
#' @param currency Required when adding a card to an account (not applicable to
#' customers or recipients). The card (which must be a debit card) can be used
#' as a transfer destination for funds in this currency. Currently, the only
#' supported currency for debit card transfers is usd.
#' @param default_for_currency Only applicable on accounts (not customers or
#' recipients). If you set this to true (or if this is the first external account
#' being added in this currency) this card will become the default external
#' account for its currency.
#' @param card_metadata  set of key/value pairs that you can attach to a card
#' object. It can be useful for storing additional information about the card in
#' a structured format.
#' @export
update_customer <- function(id, account_balance = NULL, business_vat_id = NULL, metadata = list(),
                            default_source = NULL, coupon = NULL, description = NULL,
                            email = NULL, ship_name = NULL, ship_city = NULL,
                            ship_country = NULL, ship_line1 = NULL, ship_line2 = NULL,
                            ship_state = NULL, ship_zip = NULL, ship_phone = NULL,
                            card_token = NULL, card_exp_month = NULL,
                            card_exp_year = NULL, card_number = NULL, card_cvc = NULL,
                            card_holder = NULL, address_city = NULL, address_country = NULL,
                            address_line1 = NULL, address_line2 = NULL,
                            address_state = NULL, address_zip = NULL, currency = NULL,
                            default_for_currency = NULL, card_metadata = NULL){
  update_args <- as.list(match.call())[-1]
  update_args <- update_args[setdiff(names(update_args), "id")]

  customer <- stripe_customer$new(id = id)
  upd_cust <- customer$update
  do.call("upd_cust", update_args)

  return(customer)
}

#' Get customer information
#'
#' Retrieve information on specified customer from Stripe.
#'
#' @param id ID of customer for which to rerieve information.
#' @return A \code{\link{stripe_customer}} object representing the customer
#' @export
get_customer <- function(id){
  customer <- stripe_customer$new()
  customer$retrieve(id)
  return(customer)
}

#' Delete a customer
#'
#' Delete specified customer from Stripe
#'
#' @param id ID of customer to be deleted
#' @export
delete_customer <- stripe_customer$new()$delete

#' @export
all_customers <- function(){
  cust_list <- stripe_request("https://api.stripe.com/v1/customers")
  num_cust <- length(cust_list$data)
  last_cust <- cust_list$data[[num_cust]]$id

  customer_list <- cust_list$data

  while(cust_list$has_more){
    cust_list <- stripe_request(paste0("https://api.stripe.com/v1/customers?starting_after=", last_cust))
    cust_list <- do.call("newList", cust_list)
    num_cust <- length(cust_list$data)
    last_cust <- cust_list$data[[num_cust]]$id

    customer_list[(length(customer_list)+1):(length(customer_list)+1+num_cust)] <- cust_list$data
  }

  return(customer_list)
}
