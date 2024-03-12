#' Billing Portal Session Object
#'
#' The \code{stripe_checkout_session} represents your customer's session as they pay for
#' one-time purchases or subscriptions through Checkout or Payment Links. We recommend
#' creating a new Session each time your customer attempts to pay.
#'
#' @section Methods:
#' \itemize{
#'  \item \code{create}: Creates a billing_portal_session on Stripe.
#' }
#'
#' @docType class
#' @format An R6 Class object
#' @export
stripe_billing_portal_session <- R6::R6Class(
  "billing_portal_session", lock_objects = FALSE,
  public = list(
    id  = NULL, object = "billing_portal.session", configuration = NULL, flow = NULL, livemode = NULL,
    locale = NULL, on_behalf_of = NULL, customer = NULL, return_url = NULL, url = NULL, metadata = list(),
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
    create = function(customer = NULL, configuration = NULL, flow_data = NULL,
                      locale = NULL, on_behalf_of = NULL, return_url = NULL, metadata = list()
                      ){

      create_vars <-  as.list(match.call())[-1]

      create_params <- list(customer = customer,
                            return_url = return_url
                            )

      optional  <- intersect(names(create_vars),
                             c("on_behalf_of", "locale", "flow_data", "configuration"))

      for(opt_var in optional){
          create_params[[opt_var]] <- create_vars[[opt_var]]
      }

      if(length(metadata) > 0)
        for(meta_name in names(metadata))
          create_params[[paste0("metadata[", meta_name, "]")]] <- metadata[[meta_name]]

      created_billing_portal_session<- stripe_request(private$billing_portal_session_url(),
                                       request_body = create_params,
                                       request_type = "POST")

      for(new_var in names(created_billing_portal_session))
        self[[new_var]] <- created_billing_portal_session[[new_var]]
    }
  ),
  private = list(
    billing_portal_session_url = function(id = NULL){
      self_url <- app$request_url("billing_portal/sessions")

      if(!is.null(id)){
        url_path <-  httr::parse_url(self_url)$path
        url_path <-  paste(url_path, id, sep = "/")
        self_url <- httr::modify_url(self_url, path = url_path)
      }
      return(self_url)
    }
  )
)

newBillingPortalSession <- function(...){stripe_billing_portal_session$new(...)}


