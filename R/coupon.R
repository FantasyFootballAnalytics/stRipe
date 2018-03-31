#' Coupon Object
#'
#' The \code{stripe_coupon} object represents the complete set of data required
#' to manage coupons at Stripe. A coupon has either a \code{percent_off} or an
#' \code{amount_off} and \code{currency}. If you set an \code{amount_off}, that
#' amount will be subtracted from any invoice’s subtotal. For example, an invoice
#' with a subtotal of $100 will have a final total of $0 if a coupon with an
#' \code{amount_off} of 20000 is applied to it and an invoice with a subtotal of
#' $300 will have a final total of $100 if a coupon with an \code{amount_off} of
#' 20000 is applied to it.
#'
#' @section Methods:
#' \itemize{
#'  \item \code{create}: Creates a coupon on Stripe.
#'  \item \code{retrieve}: Retrieves coupon information from Stripe based on the
#'  \code{id} passed to the method.
#'  \item \code{update}: Updates coupon metadata at Stripe.
#'  \item \code{delete}: Deletes the coupon with the provided id at Stripe.
#' }
#'
#' @docType class
#' @format An R6 Class object
#' @export
stripe_coupon <- R6::R6Class(
  "coupon", lock_objects = FALSE,
  public = list(
    id  = NULL, object = "coupon", amount_off = NULL, created = NULL,
    currency= NULL, duration = NULL, duration_in_months = NULL, livemode = NULL,
    max_redemptions = NULL, metadata = list(),  percent_off = NULL,
    redeem_by = NULL, times_redeemed = 0, valid = NULL,

    initialize = function(..., metadata = list()){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), "metadata")){
          self[[i_var]] <- eval.parent(init_vars[[i_var]])
        }
        self$metadata <- metadata
      }
    },

    create = function(id = NULL, amount_off = NULL, created = NULL,
                      currency = NULL, duration , duration_in_months = NULL,
                      max_redemptions = NULL, metadata = list(),  percent_off = NULL,
                      redeem_by = NULL){
      create_vars <-  as.list(match.call())[-1]

      create_params <- list(duration = duration)
      optional  <- intersect(names(create_vars),
                             c("id", "amount_off", "currency", "duration_in_months",
                               "max_redemptions", "percent_off" , "redeem_by"))

      for(opt_var in optional){
          create_params[[opt_var]] <- create_vars[[opt_var]]
      }

      if(length(metadata) > 0)
        for(meta_name in names(metadata))
          create_params[[paste0("metadata[", meta_name, "]")]] <- metadata[[meta_name]]

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

      if(length(metadata) >0){
        for(m_name in names(metadata)){
          update_param[[paste0("metadata[", m_name, "]")]] <- metadata[[m_name]]

        updated_coupon <- stripe_request(private$coupon_url(self$id),
                                         request_body = update_param,
                                         request_type = "POST")
        self$metadata <- metadata
        }
      }
    },
    delete = function(coupon_id){
      del_response <- stripe_request(private$coupon_url(coupon_id),
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

get_coupon <- function(coupon_id){

}
