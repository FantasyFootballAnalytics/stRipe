#' @export stripe_plan
stripe_plan <- R6::R6Class(
  "plan",
  public = list(
    # Object properties
    id = NULL, object = "plan", amount = NULL, created = NULL, currency = NULL,
    interval = NULL, interval_count = NULL, livemode = NULL, metadata = list(),
    name = NULL, statement_descriptor = NULL, trial_period_days = NULL,

    # Initialize method
    initialize = function(..., metadata = list()){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), "metadata")){
          self[[i_var]] <- init_vars[[i_var]]
        }
        self$metadata <- metadata
      }
    },

    # Create method that will create the plan at Stripe
    create = function(){
      create_params <- list(id = self$id,
                            amount = self$amount,
                            currency = self$currency,
                            interval = self$interval,
                            name = self$name)
      if(!is.null(self$interval_count))
        create_params$interval_count <- self$interval_count

      if(length(self$metadata) > 0 )
        create_params$metadata <- self$metadata

      if(!is.null(self$statement_descriptor))
        create_params$statement_descriptor <- self$statement_descriptor

      if(!is.null(self$trial_period_days))
        create_params$trial_period_days <- self$trial_period_days

      create_url <- app$request_url("plans")
      created_plan <- stripe_request(create_url, request_body = create_params, request_type = "POST")
      for(param in names(created_plan))
        self[[param]] <- created_plan[[param]]
    },

    # Retrieve method to retrieve data from Stripe based on the plan id
    retrieve = function(plan_id){
      self$id <- plan_id
      plan_info <- stripe_request(private$plan_url())
      self$amount <- plan_info$amount
      self$created <- plan_info$created
      self$interval <- plan_info$interval
      self$interval_count <- plan_info$interval_count
      self$livemode <- plan_info$livemode
      self$statement_descriptor <- plan_info$statement_descriptor
      self$trial_period_days <- plan_info$trial_period_days
      self$name <- plan_info$name
    },

    # Update method to update plan information at Stripe
    update = function(metadata = NULL, name = NULL,
                      statement_descriptor = NULL,
                      trial_period_days = NULL){
      func_param <- as.list(match.call())[-1]

      if(length(func_param) == 0)
        return()

      update_param <- list()
      for(param_name in setdiff(names(func_param), "metadata"))
        update_param[param_name] <- func_param[[param_name]]

      if(any(names(func_param) == "metadata"))
        update_param$metadata <- metadata

      updated_plan <- stripe_request(private$plan_url(),
                                     request_body = update_param,
                                     request_type = "POST")

      for(param in names(func_param))
        self[[param]] <- updated_plan[[param]]
    },

    # Delete Method to remove plan from Stripe
    delete = function(){
      del_response <- stripe_request(private$plan_url(), request_type = "DELETE")
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


newPlan <- function(...){stripe_plan$new(...)}
