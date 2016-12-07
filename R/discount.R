#' @include coupon.R
#' @export stripe_discount
stripe_discount <- R6::R6Class(
  "discount",
  public = list(
    object = "discount", coupon = NULL, customer = NULL, end = NULL,
    start = NULL, subscription = NULL,

    initialize = function(..., coupon = NULL){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), "coupon")){
          self[[i_var]] <- init_vars[[i_var]]
        }

        if(R6::is.R6(coupon) | is.null(coupon))
          self$coupon <- coupon
        else
          self$coupon <- do.call("newCoupon", coupon)
      }
    },

    delete_customer = function(){
      deleted_discount <- stripe_request(private$discount_url(self$customer),
                                         request_type = "DELETE")
    },
    delete_subscription = function(){
      deleted_discount <- stripe_request(private$discount_url(self$subscription),
                                         request_type = "DELETE")
    }
  ),
  private = list(
    # Coupon url function will generate the url for calling the card API
    discount_url = function(parent_id){
      parent_type <- switch(substr(parent_id, 1, 3),
                            "cus" = "customers",
                            "sub" = "subscriptions")
      self_url <- app$request_url(parent_type)

      url_path <-  httr::parse_url(self_url)$path
      url_path <-  paste(url_path, parent_id, "discount", sep = "/")
      self_url <- httr::modify_url(self_url, path = url_path)

      return(self_url)
    }
  )
)

newDiscount <- function(...){stripe_discount$new(...)}
