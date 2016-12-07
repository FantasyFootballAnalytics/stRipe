#' Subscription Resource
#'
#' This is the Stripe subscription resource.
#' @export stripe_subscription
#' @include plan.R
stripe_subscription <- R6::R6Class(
  "subscription",
  public = list(
    # Object Properties
    id = NULL,     object = "subscription", application_fee_percent = NULL,
    cancel_at_period_end = FALSE, canceled_at = NULL, created = NULL,
    current_period_end = NULL, current_period_start = NULL, customer = NULL,
    discount = NULL, ended_at = NULL, livemode = FALSE, metadata = list(),
    plan = NULL, quantity = NULL, start = NULL, status = NULL, tax_percent = NULL,
    trial_end = NULL, trial_start = NULL,

    # Initialize method, calling out plan separately as that could be passed
    # either as a class or list if not NULL. If plan is a list then a class
    # object is created with the use of do.call
    initialize = function(..., plan = NULL, metadata = list()){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), c("plan", "metadata"))){
          self[[i_var]] <- init_vars[[i_var]]
        }

        self$metadata <- metadata

        if(R6::is.R6(plan) | is.null(plan))
          self$plan <- plan
        else
          self$plan <- do.call("newPlan", plan)
      }
    },

    # Create method that accepts a plan id, source (card information) and a
    # coupon code. Plan id is required for the create request to be successful.
    # Make sure you have at least updated the customer property of the
    # subscription before calling the method. If any of application_fee_percent,
    # quantity, metadata, tax_percent, trial_end have been filled out then they
    # will be used in the create request too.
    create = function(plan_id, source = NULL, coupon_code = NULL){
      create_param <- list(customer = self$customer, plan = plan_id)

      if(!is.null(source))
        create_param$source <- source

      if(!is.null(coupon_code))
        create_param$coupon <- coupon_code

      for(sub_var in c("application_fee_percent", "quantity", "metadata",
                       "tax_percent", "trial_end")){
        if(!is.null(self[[sub_var]]))
          create_param[[sub_var]] <- self[[sub_var]]
      }

      new_sub <- stripe_request(private$subscription_url(),
                                request_body = create_param,
                                request_type = "POST")
      for(new_var in setdiff(names(new_sub), "plan")){
        self[[new_var]] <- new_sub[[new_var]]
      }
      self$plan <- do.call("newPlan", new_sub$plan)
    },

    # Retrieve method to get the subscription information based on the id
    # property. Any property changes that have not been sent to Stripe via the
    # update method will be overwritten
    retrieve = function(sub_id){
      sub <- stripe_request(private$subscription_url(sub_id))
      sub_vars <- setdiff(names(sub), "plan")
      for(sub_var in sub_vars){
        self[[sub_var]] <- sub[[sub_var]]
      }
      self$plan <- do.call("newPlan", sub$plan)
    },
    update = function(plan = NULL, coupon = NULL,
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
    },
    cancel = function(end_of_period = FALSE){
      cancel_sub <-stripe_request(subscription_url(self$id),
                                   request_body = list(at_period_end = end_of_period),
                                   request_type = "DELETE")

      self$status <- cancel_sub$status
      self$cancel_at_period_end <- cancel_sub$cancel_at_period_end
    }

  ),
  private = list(
    subscription_url = function(sub_id = NULL){
      self_url <- app$request_url("subscriptions")
      if(!is.null(sub_id))
        url_path <-  paste(httr::parse_url(self_url)$path, sub_id, sep = "/")
      self_url <- httr::modify_url(self_url, path = url_path)
      return(self_url)
    }
  )
)

newSubscription <- function(...){stripe_subscription$new(...)}
