
#' @export stripe_coupon
stripe_coupon <- R6::R6Class(
  "coupon",
  public = list(
    id  = NULL, object = "coupon", amount_off = NULL, created = NULL,
    currency= NULL, duration = NULL, duration_in_months = NULL, livemode = NULL,
    max_redemptions = NULL, metadata = list(),  percent_off = NULL,
    redeem_by = NULL, times_redeemed = 0, valid = NULL,

    initialize = function(..., metadata = list()){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), "metadata")){
          self[[i_var]] <- init_vars[[i_var]]
        }
        self$metadata <- metadata
      }
    },

    create = function(){
      create_params <- list(duration = self$duration)
      optional  <- c("id", "amount_off", "currency", "duration_in_months",
                     "max_redemptions", "percent_off" , "redeem_by")

      for(opt_var in optional){
        if(!is.null(self[[opt_var]])){
          create_params[[opt_var]] <- self[[opt_var]]
        }
      }

      if(length(selfmetadata) > 0)
        create_params$metadata <- self$metadata

      created_coupon <- stripe_request(private$coupon_url(),
                                       request_body = create_params,
                                       request_type = "POST")

      for(new_var in names(created_coupon))
        self[[new_var]] <- created_coupon[[new_var]]
    },
    retrieve = function(coupon_id){
      coupon_info <- stripe_request(private$coupon_url(coupon_id))

      for(coupon_var in names(coupon_info)){
        self[[coupon_var]] <- coupon_info[[coupon_var]]
      }
    },
    update = function(metadata = list()){
      if(length(metadata) > 0){
        for(m_name in names(metadata)){
          self$metadata[[m_name]] <- metadata[[m_name]]
        }

        updated_coupon <- stripe_request(private$coupon_url(self$id),
                                         request_body = list(metatdata = self$metadata),
                                         request_type = "POST")
      }
    },
    delete = function(){
      del_response <- stripe_request(private$coupon_url(self$id),
                                     request_type = "DELETE")
    }
  ),
  private = list(
    # Coupon url function will generate the url for calling the card API
    coupon_url = function(coupon_id = NULL){
      self_url <- app$request_url("coupons")

      if(!is.null(coupon_id)){
        url_path <-  httr::parse_url(self_url)$path
        url_path <-  paste(url_path, coupon_id, sep = "/")
        self_url <- httr::modify_url(self_url, path = url_path)
      }
      return(self_url)
    }
  )
)

newCoupon <- function(...){stripe_coupon$new(...)}
