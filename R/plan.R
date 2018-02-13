#' Plan Object
#'
#' The \code{stripe_plan} object represents the complete set of data required to
#' manage subscription plans at Stripe. A subscription plan contains the pricing
#' and billing information to be charged for differen features or products.
#' With different plans, customers can be charged differently for different
#' services or levels.
#'
#' @section Methods:
#' \itemize{
#'  \item \code{create}: Creates a plan on Stripe. Required arguments are:
#'   \itemize{
#'    \item \code{id}: The id of the plan
#'    \item \code{name}: Display name of the plan
#'    \item \code{amount}: The amount of the plan in cents. So a $4.99 plan is 499
#'    \item \code{currency}: The currency for the amount
#'    \item \code{interval}: The billing interval for the plan. Should be one of
#'    \code{day, week, month, year}.
#'  }
#'  \item \code{retrieve}: Retrieves plan information from Stripe based on the
#'  \code{id} passed to the method.
#'  \item \code{update}: Updates plan information at Stripe. Using this method
#'  the plan name, statement descriptor, trial days and metadata properties can
#'  be updated.
#'  \item \code{delete}: Deletes the plan with the provided id at Stripe.
#' }
#'
#' @examples
#' # Create Test Plan 1 with a monthly charge of $9.99
#' new_plan <- stripe_plan$new()
#' new_plan$create(id = "testPlan1", name = "Test Plan 1", amount = 999,
#'                 currency = "usd", interval = "month")
#'
#' # Retrieve and update a plan name
#' my_plan <- stripe_plan$new()
#' my_plan$retrieve("testPlan1")
#' my_plan$update(name = "My Test Plan")
#'
#' # Delete plan with id "testPlan1".
#' delete_plan <- stripe_plan$new()
#' delete_plan$delete("testPlan1")
#'
#' @docType class
#' @format An R6 Class object
#' @export
stripe_plan <- R6::R6Class(
  "plan",
  public = list(
    # Object properties
    id = NULL, object = "plan", amount = NULL, created = NULL, currency = NULL,
    interval = NULL, interval_count = NULL, livemode = NULL, metadata = list(),
    name = NULL, nickname = NULL, product = NULL, trial_period_days = NULL,

    # Initialize method
    initialize = function(..., metadata = list()){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), "metadata")){
          self[[i_var]] <- eval.parent(init_vars[[i_var]])
        }
        self$metadata <- metadata
      }
      invisible(self)
    },

    # Create method that will create the plan at Stripe
    create = function(id, amount, currency, interval, nickname,
                      interval_count = NULL, metadata = list(),
                      product = NULL, trial_period_days = NULL){
      create_params <- list(id = id,
                            amount = amount,
                            currency = currency,
                            interval = interval,
                            nickname = nickname)
      if(!is.null(interval_count))
        create_params$interval_count <- interval_count

      if(length(self$metadata) > 0 )
        create_params$metadata <- self$metadata

      if(!is.null(statement_descriptor))
        create_params$product <- product

      if(!is.null(trial_period_days))
        create_params$trial_period_days <- trial_period_days

      create_url <- app$request_url("plans")
      created_plan <- stripe_request(create_url, request_body = create_params,
                                     request_type = "POST")

      for(param in names(created_plan))
        self[[param]] <- created_plan[[param]]

      invisible(self)
    },

    # Retrieve method to retrieve data from Stripe based on the plan id
    retrieve = function(plan_id){
      self$id <- plan_id
      plan_info <- stripe_request(private$plan_url())

      for(param in names(plan_info))
        self[[param]] <- plan_info[[param]]

      invisible(self)
    },

    # Update method to update plan information at Stripe
    update = function(metadata = NULL, nickname = NULL,
                      product = NULL,
                      trial_period_days = NULL){
      func_param <- as.list(match.call())[-1]

      if(length(func_param) == 0)
        return()

      update_param <- list()
      for(param_name in setdiff(names(func_param), "metadata"))
        if(!is.null(eval.parent(func_param[[param_name]])))
          update_param[[param_name]] <- eval.parent(func_param[[param_name]])

      if(any(names(func_param) == "metadata"))
        for(m_name in names(metadata))
          update_param[[paste0("metadata[", m_name, "]")]] <- metadata[[m_name]]

      updated_plan <- stripe_request(private$plan_url(),
                                     request_body = update_param,
                                     request_type = "POST")

      for(param in names(func_param))
        self[[param]] <- updated_plan[[param]]

      invisible(self)
    },

    # Delete Method to remove plan from Stripe
    delete = function(plan_id){
      self$id <- plan_id
      del_response <- stripe_request(private$plan_url(), request_type = "DELETE")
      if(del_response$deleted)
        cat("Deleted plan with id:", del_response$id)
    }

  ),
  private = list(
    plan_url = function(){
      self_url <- app$request_url("plans")
      url_path <-  paste(httr::parse_url(self_url)$path, self$id, sep = "/")
      self_url <- httr::modify_url(self_url, path = url_path)
      return(self_url)
    }
  )
)

# Internal function to create plans for objects
newPlan <- function(...){stripe_plan$new(...)}

#' Create a plan
#'
#' Creates plan at Stripe and returns the \code{stripe_plan} object representing
#' the plan. See the \link{stripe_plan} documentation for more details on the
#' plan object.
#' @param id The id of plan to create
#' @param amount The amount of the plan in cents. So a $4.99 plan is 499
#' @param currency The currency for the amount
#' @param interval One of \code{day, week, month or year}. The frequency with
#' which a subscription should be billed.
#' @param name Display name of the plan
#' @param interval_count The number of intervals (specified in the interval
#' property) between each subscription billing. For example,
#' \code{interval = month} and \code{interval_count = 3} bills every 3 months
#' @param metadata A set of key/value pairs that you can attach to a plan object.
#' It can be useful for storing additional information about the plan in a
#' structured format. This will be unset if you specicy \code{NULL}.
#' @param statement_descriptor An arbitrary string to be displayed on your
#' customer’s credit card statement. This may be up to 22 characters
#' @param trial_period_days Number of trial period days granted when subscribing
#' a customer to this plan. \code{NULL} if the plan has no trial period
#' @examples
#' # Create Test Plan 1 with a monthly charge of $9.99
#' new_plan <- create_plan(id = "testPlan1", name = "Test Plan 1", amount = 999,
#'                         currency = "usd", interval = "month")
#'
#' @export
create_plan <- function(id, amount, currency, interval, nickname,
                        interval_count = NULL, metadata = list(),
                        product = NULL, trial_period_days = NULL){
  create_args <- as.list(match.call())[-1]
  add_plan <- stripe_plan$new()$create
  new_plan <- do.call("add_plan", create_args)
  return(new_plan)
}

#' Update a plan
#'
#' Updates the plan represented by the \code{id} provided and returns the updated
#' \code{\link{stripe_plan}} object. THe function can be used to update a plan's
#' name, metadata, statement descriptor and trial period.
#'
#' @param id The id of the plan to be updated
#' @param name Display name of the plan
#' @param metadata A set of key/value pairs that you can attach to a plan object.
#' It can be useful for storing additional information about the plan in a
#' structured format. This will be unset if you specicy \code{NULL}.
#' @param statement_descriptor An arbitrary string to be displayed on your
#' customer's credit card statement. This may be up to 22 characters
#' @param trial_period_days Number of trial period days granted when subscribing
#' a customer to this plan. \code{NULL} if the plan has no trial period
#'
#' @examples
#' # Update the display name of plan with id proPlan to "Pro Plan"
#' my_plan <- update_plan("proPlan", name = "Pro Plan")
#' @export
update_plan <- function(id, metadata = NULL, nickname = NULL,
                        product = NULL, trial_period_days = NULL){
  update_args <- as.list(match.call())[-1]
  update_args <- update_args[setdiff(names(update_args), "id")]

  plan <- stripe_plan$new(id = id)
  upd_plan <- plan$update
  plan <- do.call("upd_plan", update_args)
  return(plan)
}

#' Delete a plan
#'
#' Removes the plan with the provided \code{plan_id} from your Stripe account.
#' If successful a message will be displayed with the \code{plan_id} of the plan
#' that was removed
#' @param plan_id The id of the plan to be removed
#' @examples
#' # Remove plan with plan_id testPlan
#' delete_plan("testPlan")
#' @export
delete_plan <- stripe_plan$new()$delete

#' Retrieve a plan
#'
#' Fetches the plan with the provided id and returns the \code{\link{stripe_plan}}
#' object representing the plan.
#' @param id The id of the plan to be retrieved
#' @examples
#' # Retrieve plan with id testPlan
#' plan <- get_plan("testPlan")
#' @export
get_plan <- function(id){
  plan <- stripe_plan$new()
  plan$retrieve(id)
  return(plan)
}

