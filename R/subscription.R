#' Subscription object
#'
#' The \code{stripe_subscription} object represents the data and methods needed
#' to manage customer subscriptions. Subscriptions allow you to charge a
#' customer's card on a recurring basis. A subscription ties a customer to a
#' particular plan that was previously created.
#' @section Methods:
#' \itemize{
#'  \item \code{create}: Creates a subscription for a customer on Stripe.
#'  Required arguments are:
#'   \itemize{
#'    \item \code{plan_id}: The id of the plan that the customer is subscribing to
#'    \item \code{customer_id}: The id of the customer that the subscription
#'    should be created for.
#'  }
#'  \item \code{retrieve}: Retrieves subscription information from Stripe based
#'  on the \code{id} passed to the method.
#'  \item \code{update}: Updates subscription information at Stripe. Using this method
#'  the plan name, statement descriptor, trial days and metadata properties can
#'  be updated.
#'  \item \code{cancel}: Cancels the subscription with the provided id at Stripe.
#' }
#' @docType class
#' @format An R6 Class object
#' @include plan.R
#' @export
stripe_subscription <- R6::R6Class(
  "subscription",
  public = list(
    # Object Properties
    id = NULL,     object = "subscription", billing = NULL, application_fee_percent = NULL,
    cancel_at_period_end = FALSE, canceled_at = NULL, created = NULL,
    billing_cycle_anchor = NULL,
    current_period_end = NULL, current_period_start = NULL, customer = NULL,
    discount = NULL, ended_at = NULL, items = list(), livemode = FALSE, metadata = list(),
    plan = NULL, quantity = NULL, start = NULL, status = NULL, tax_percent = NULL,
    trial_end = NULL, trial_start = NULL, days_until_due = NULL,

    initialize = function(..., plan = NULL, metadata = list()){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), c("plan", "metadata", "items"))){
          self[[i_var]] <- eval.parent(init_vars[[i_var]])
        }

        if(length(metadata) > 0)
        self$metadata <- metadata

        if(R6::is.R6(plan) | is.null(plan))
          self$plan <- plan
        else
          self$plan <- do.call("newPlan", plan)


        if(any(names(init_vars) == "items"))
          if(length(init_vars$items) > 0)
            self$items <- do.call("newList", init_vars$items)
      }
      invisible(self)
    },

    create = function(plan_id, customer_id,  card_token = NULL, exp_month = NULL,
                      exp_year = NULL, number = NULL, cvc = NULL,
                      name = NULL, address_city = NULL, address_country = NULL,
                      address_line1 = NULL, address_line2 = NULL,
                      address_state = NULL, address_zip = NULL, currency = NULL,
                      default_for_currency = NULL, card_metadata = list(),
                      coupon = NULL, items = list(),
                      application_fee_percent = NULL, quantity = NULL,
                      tax_percent = NULL, trial_end = NULL, trial_period_days = NULL){

      func_param <- as.list(match.call())[-1]

      create_param <- list(customer = customer_id, plan = plan_id)

      card_data <- c("exp_month", "exp_month", "exp_year",
                     "number", "cvc", "name", "address_city",
                     "address_country", "address_line1",
                     "address_line2", "address_state",
                     "address_zip", "currency",
                     "default_for_currency", "card_metadata")

      card_param <- list(object = "card")
      for(param in intersect(names(func_param), card_data)){
        if(!is.null(func_param[[param]]))
          card_param[param] <- func_param[[param]]
      }


      if(length(card_param) > 1){
        card_source <- get_card_source(card_param)
        for(param in  names(card_source))
          create_param[param] <- card_source[param]
      }

      if(!is.null(coupon))
        create_param$coupon <- coupon

      for(sub_var in c("application_fee_percent", "quantity",
                       "tax_percent", "trial_end", "trial_period_days")){
        if(!is.null(self[[sub_var]]))
          create_param[[sub_var]] <- self[[sub_var]]
      }

      new_sub <- stripe_request(private$subscription_url(),
                                request_body = create_param,
                                request_type = "POST")
      for(new_var in setdiff(names(new_sub), c("plan", "items"))){
        self[[new_var]] <- new_sub[[new_var]]
      }
      self$plan <- do.call("newPlan", new_sub$plan)
      self$items <- do.call("newList", new_sub$items)
    },

    # Retrieve method to get the subscription information based on the id
    # property. Any property changes that have not been sent to Stripe via the
    # update method will be overwritten
    retrieve = function(sub_id){
      requested_sub <- stripe_request(private$subscription_url(sub_id))
      sub_vars <- setdiff(names(requested_sub), c("plan", "items"))

      for(sub_var in sub_vars){
        self[[sub_var]] <- requested_sub[[sub_var]]
      }
      self$plan <- do.call("newPlan", requested_sub$plan)

      if(any(names(requested_sub) == "items"))
        self$items <- do.call("newList", requested_sub$items)

      invisible(self)
    },
    update = function(plan = NULL, coupon = NULL, items = list(),
                      prorate = NULL, proration_date = NULL, source = NULL,
                      application_fee_percent = NULL, metadata = list(),
                      tax_percent = NULL, trial_end = NULL){

      update_params <- as.list(match.call())[-1]
      if(length(update_params) == 0)
        return()

      if(any(names(update_params) == "metadata"))
        update_params$metadata <- metadata

      updated_sub <- stripe_request(subscription_url(self$id),
                                    request_body = update_params,
                                    request_type = "POST")
      sub_vars <- intersect(c("application_fee_percent", "metadata", "tax_percent", "trial_end"),
                            update_params)
      for(sub_var in sub_vars){
        self[[sub_var]] <- updated_sub[[sub_var]]
      }
      if(any(names(update_params) == "plan"))
        self$plan <- do.call("newPlan", updated_sub$plan)

      invisible(self)
    },

    cancelSub = function(end_of_period = FALSE){

      cancel_sub <-stripe_request(private$subscription_url(self$id),
                                  request_body = list(at_period_end = tolower(as.character(end_of_period))),
                                  request_type = "DELETE")
      self$status <- cancel_sub$status
      self$cancel_at_period_end <- cancel_sub$cancel_at_period_end
    }

  ),
  private = list(
    subscription_url = function(sub_id = NULL){
      self_url <- app$request_url("subscriptions")
      if(!is.null(sub_id)){
        url_path <-  paste(httr::parse_url(self_url)$path, sub_id, sep = "/")
        self_url <- httr::modify_url(self_url, path = url_path)
      }
      return(self_url)
    }
  )
)

newSubscription <- function(...){stripe_subscription$new(...)}

#' Create a subscription
#'
#' Creates a subscription at Stripe and returns the newly created
#' \code{stripe_subscription} object. See the \link{stripe_subscription}
#' documentation for more information on the subscription object.
#'
#' @note If the plan that the customer is being subscribed to requires payment,
#' then the customer will either need to have an defined payment source, or a
#' payment source (card token or card infomration) will need to be passed in the
#' function.
#'
#' @param plan_id The identifier of the plan to subscribe the customer to.
#' @param customer_id The identifier of the customer to subscribe.
#' @param card_token The identifier of the card token that can be used for payment
#' of the subscription
#' @param exp_month Expiration month for the card to be used if \code{card_token}
#' is not passed
#' @param exp_year Expiration year for the card to be used if \code{card_token}
#' is not passed
#' @param number Card number for the card to be used if \code{card_token} is not
#' passed
#' @param cvc Card security code for the card to be used if \code{card_token} is
#' not passed
#' @param name Card holder name for the card to be used if \code{card_token} is
#' not passed
#' @param address_city City of the billing address for the card
#' @param address_country Country of the billing address for the card
#' @param address_line1 Line 1 of billing address for the card
#' @param address_line2 Line 2 of billing address for the card
#' @param address_state State from the billing address for the card
#' @param address_zip Zip or Postal Code for card billing address
#' @param tax_percent A positive decimal (with at most four decimal places)
#' between 1 and 100. This represents the percentage of the subscription invoice
#' subtotal that will be calculated and added as tax to the final amount each
#' billing period. For example, a plan which charges $10/month with a
#' tax_percent of 20.0 will charge $12 per invoice
#' @param coupon The code of the coupon to apply to this subscription. A coupon
#' applied to a subscription will only affect invoices created for that
#' particular subscription
#' @param quantity The quantity you’d like to apply to the subscription you’re
#' creating. For example, if your plan is 10/user/month, and your customer has 5
#' users, you could pass 5 as the quantity to have the customer charged 50 (5 x
#' 10) monthly. If you update a subscription but don’t change the plan ID (e.g.
#' changing only the trial_end), the subscription will inherit the old
#' subscription’s quantity attribute unless you pass a new quantity parameter.
#' If you update a subscription and change the plan ID, the new subscription
#' will not inherit the quantity attribute and will default to 1 unless you pass
#' a quantity parameter
#' @param trial_end Unix timestamp representing the end of the trial period the
#' customer will get before being charged for the first time. If set, trial_end
#' will override the default trial period of the plan the customer is being
#' subscribed to
#' @param trial_period_days Integer representing the number of trial period days
#' before the customer is charged for the first time. If set, trial_period_days
#' overrides the default trial period days of the plan the customer is being
#' subscribed to.
#' @export
create_subscription <- function(plan_id, customer_id,  card_token = NULL,
                                exp_month = NULL, exp_year = NULL, number = NULL,
                                cvc = NULL, name = NULL, address_city = NULL,
                                address_country = NULL, address_line1 = NULL,
                                address_line2 = NULL, address_state = NULL,
                                address_zip = NULL, currency = NULL,
                                default_for_currency = NULL,
                                card_metadata = list(), coupon = NULL,
                                application_fee_percent = NULL, quantity = NULL,
                                tax_percent = NULL, trial_end = NULL,
                                trial_period_days = NULL){
  create_args <- as.list(match.call())[-1]
  arg_names <- names(create_args)
  create_args <- lapply(create_args, eval.parent, n=2)
  names(create_args) <- arg_names
  add_sub <- stripe_subscription$new()$create

  new_sub <- do.call("add_sub", create_args)
  return(new_sub)
}

#' Retrieve a subscription
#'
#' Retrieves subscription with the provided ID and returns the
#' \link{stripe_subcription} object representing the requested subscription
#'
#' @param id The identifier of the subscription to be retrieved.
#' @export
get_subscription <- function(id){
  sub <- stripe_subscription$new()$retrieve(id)
  return(sub)
}

#' Update a subscription
#'
#' Updates the subscription with the provided ID. Enables changing plans,
#' applying discounts, prorating, and updating trial end dates.
#'
#' @param subscription_id Identifier of subscription to be updated
#' @param plan The identifier of the plan to update the subscription to. If
#' omitted, the subscription will not change plans
#' @param coupon The code of the coupon to apply to this subscription. A coupon
#' applied to a subscription will only affect invoices created for that
#' particular subscription
#' @param prorate Flag determining whether to prorate switching plans during a
#' billing cycle
#' @param proration_date If set, the proration will be calculated as though the
#' subscription was updated at the given time. This can be used to apply exactly
#' the same proration that was previewed with upcoming invoice endpoint. It can
#' also be used to implement custom proration logic, such as prorating by day
#' instead of by second, by providing the time that you wish to use for
#' proration calculations.
#' @param trial_end Unix timestamp representing the end of the trial period the
#' customer will get before being charged for the first time. If set, trial_end
#' will override the default trial period of the plan the customer is being
#' subscribed to
#' @export
update_subscription <- function(subscription_id, plan = NULL, coupon = NULL,
                                prorate = NULL, proration_date = NULL, source = NULL,
                                application_fee_percent = NULL, metadata = list(),
                                tax_percent = NULL, trial_end = NULL){
  update_args <- as.list(match.call())[-1]
  update_args <- update_args[setdiff(names(update_args), "subscription_id")]

  sub <- get_subscription(subscription_id)
  updateSub <- sub$update
  sub <- do.call("updateSub", update_args)
  return(sub)
}

#' Cancel a subscription
#'
#' Cancels the subscription with the provided ID on Stripe and returns the
#' \link{stripe_subscription} object representing the canceled subscription.
#'
#' @param subscription_id The identifier of the subscription to be canceled.
#' @param end_of_period If \code{FALSE} then the subscription will be canceled
#' immediately, if \code{TRUE} then the subscription will be canceled at the end
#' of the subscription period.
#' @export
cancel_subscription <- function(subscription_id, end_of_period = FALSE){
  subscription <- get_subscription(subscription_id)
  subscription$cancelSub(end_of_period)
  return(subscription)
}
