#' Coupon Object
#'
#' The \code{stripe_checkout_session} represents your customer's session as they pay for
#' one-time purchases or subscriptions through Checkout or Payment Links. We recommend
#' creating a new Session each time your customer attempts to pay.
#'
#' @section Methods:
#' \itemize{
#'  \item \code{create}: Creates a checkout_session on Stripe.
#'  \item \code{retrieve}: Retrieves checkout_session information from Stripe based on the
#'  \code{id} passed to the method.
#' }
#'
#' @docType class
#' @format An R6 Class object
#' @export
stripe_checkout_session <- R6::R6Class(
  "checkout_session", lock_objects = FALSE,
  public = list(
    id  = NULL, object = "checkout.session", client_reference_id = NULL, currency = NULL,
    customer = NULL, customer_email = NULL, line_items = NULL, mode = NULL, payment_intent = NULL, payment_status = NULL,
    status = NULL, success_url = NULL, url = NULL, livemode = NULL, metadata = list(),
    created = NULL,

    initialize = function(..., metadata = list()){
      init_vars <- as.list(match.call())[-1]
      if(length(init_vars) > 0){
        for(i_var in setdiff(names(init_vars), "metadata")){
          self[[i_var]] <- eval.parent(init_vars[[i_var]])
        }
        self$metadata <- metadata
      }
    },
#"subscription"
    create = function(client_reference_id = NULL, customer = NULL, customer_email = NULL,
                      currency = NULL, line_items = NULL, metadata = list(),  mode = NULL,
                      success_url = NULL){
      #browser()
      create_vars <-  as.list(match.call())[-1]

      create_params <- list(customer = customer,
                            line_items = line_items,
                            mode = mode)
      optional  <- intersect(names(create_vars),
                             c("client_reference_id", "customer_email", "currency", "success_url"))

      for(opt_var in optional){
          create_params[[opt_var]] <- create_vars[[opt_var]]
      }

      if(length(metadata) > 0)
        for(meta_name in names(metadata))
          create_params[[paste0("metadata[", meta_name, "]")]] <- metadata[[meta_name]]

      created_checkout_session <- stripe_request(private$checkout_session_url(),
                                       request_body = create_params,
                                       request_type = "POST")

      for(new_var in names(created_checkout_session))
        self[[new_var]] <- created_checkout_session[[new_var]]
    },
    retrieve = function(checkout_session_id){
      checkout_session_info <- stripe_request(private$checkout_session_url(checkout_session_id))

      for(checkout_session_var in names(checkout_session_info)){
        self[[checkout_session_var]] <- checkout_session_info[[checkout_session_var]]
      }
    }
  ),
  private = list(
    # checkout_session url function will generate the url for calling the checkout_session API
    checkout_session_url = function(checkout_session_id = NULL){
      self_url <- app$request_url("checkout/sessions")

      if(!is.null(checkout_session_id)){
        url_path <-  httr::parse_url(self_url)$path
        url_path <-  paste(url_path, checkout_session_id, sep = "/")
        self_url <- httr::modify_url(self_url, path = url_path)
      }
      return(self_url)
    }
  )
)

newCheckoutSession <- function(...){stripe_checkout_session$new(...)}

get_checkout_session <- function(checkout_session_id){

}
