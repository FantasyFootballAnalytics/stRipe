#' @include plan.R
#' @export
stripe_sub_item <- R6::R6Class(
  "subscription_item", lock_objects = FALSE,
  public = list(
    id = NULL,
    object = "subsription_item",
    created = NULL,
    plan = NULL,
    quantity = NULL,

    initialize = function(..., plan = NULL){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), c("plan"))){
          self[[i_var]] <- eval.parent(init_vars[[i_var]])
        }

        if(R6::is.R6(plan) | is.null(plan))
          self$plan <- plan
        else
          self$plan <- do.call("newPlan", plan)
      }
      invisible(self)
    },

    create = function(plan_id, subscription_id, prorate = NULL, proation_date = NULL,
                      quantity = NULL){
      create_param <- list(plan = plan_id, subscription = subscription_id)

      if(!is.null(prorate))
        create_param$prorate <- prorate

      if(!is.null(proation_date))
        create_param$proration_date <- proation_date

      if(!is.null(quantity))
        create_param$quantity <- quantity

      new_item <- stripe_request(private$subitem_url(),
                                 request_body = create_param,
                                 request_type = "POST")

      for(new_var in setdiff(names(new_item), "plan")){
        self[[new_var]] <- new_item[[new_var]]
      }
      self$plan <- do.call("newPlan", new_item$plan)
    },

    retrieve = function(id){
      sub_item <- stripe_request(private$subitem_url(id))
      for(new_var in setdiff(names(sub_item), "plan")){
        self[[new_var]] <- sub_item[[new_var]]
      }
      self$plan <- do.call("newPlan", sub_item$plan)
    },

    update = function(item_id, plan_id = NULL, prorate = NULL, proration_date = NULL,
                      quantity = NULL){

      update_param <- list()

      if(!is.null(plan_id))
        update_param$plan = plan_id

      if(!is.null(prorate))
        update_param$prorate = prorate

      if(!is.null(proration_date))
        update_param$proration_date = proration_date

      if(!is.null(quantity))
        update_param$quantity = quantity

      if(length(update_param) > 0){
        update_item <- stripe_request(private$subitem_url(item_id),
                                      request_body = update_param,
                                      request_type = "POST")

        for(new_var in setdiff(names(update_item), "plan")){
          self[[new_var]] <- update_item[[new_var]]
        }
        self$plan <- do.call("newPlan", update_item$plan)
      }

    },

    delete = function(item_id){
      stripe_request(private$subitem_url(item_id),
                     request_type = "DELETE")
    }

  ),
  private = list(
    subitem_url = function(item_id = NULL){
      self_url <- app$request_url("subscription_items")
      if(!is.null(item_id)){
        url_path <-  paste(httr::parse_url(self_url)$path, item_id, sep = "/")
        self_url <- httr::modify_url(self_url, path = url_path)
      }
      return(self_url)
    }
  )
)


newSubitem <- function(...){stripe_sub_item$new(...)}
