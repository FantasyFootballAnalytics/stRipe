#' @export object_list
object_list <- R6::R6Class(
  "list", lock_objects = FALSE,
  public = list (
    object = "list", data = list(), has_more = NULL, total_count = NULL,
    url = NULL,
    initialize = function(data = list(), has_more = NULL, object = "list",
                          total_count = NULL, url = NULL){
      self$has_more <- has_more
      self$total_count <- total_count
      self$url <- url

      self$data <- lapply(data,
                          function(list_data){
                            list_object <- list_data$object
                            obj_func <- switch (list_object,
                                                "plan" = "newPlan",
                                                "card" = "newCard",
                                                "subscription" = "newSubscription",
                                                "line_item" = "newInvoiceLine",
                                                "invoiceitem" = "newInvoiceItem",
                                                "invoice" = "newInvoice",
                                                "coupon" = "newCoupon",
                                                "discount" = "newDiscount",
                                                "bank_account" = "newBankAccount",
                                                "token" = "newToken",
                                                "customer" = "newCustomer",
                                                "checkout_session" = "newCheckoutSession",
                                                "billing_portal_session" = "newBillingPortalSession",
                                                "subscription_item" = "newSubitem",
                                                "list" = "newList",
                                                "source" = "newCard"
                            )
                            obj_class = switch(list_object,
                                               "plan" = stripe_plan,
                                               "card" = stripe_card,
                                               "subscription" = stripe_subscription,
                                               "line_item" = stripe_line_item,
                                               "invoiceitem" = stripe_inv_item,
                                               "invoice" = stripe_invoice,
                                               "coupon" = stripe_coupon,
                                               "bank_account" = stripe_bank_account,
                                               "token" = stripe_token,
                                               "customer" = stripe_customer,
                                               "checkout_session" = stripe_checkout_session,
                                               "billing_portal_session" = stripe_billing_portal_session

                            )
                            object_param <- setdiff(names(list_data), "object")
                            object_param <- intersect(object_param, names(obj_class$public_fields))

                            return(do.call(obj_func, list_data[object_param]))
                          }
      )
    }
  )
)
newList <- function(...){object_list$new(...)}
