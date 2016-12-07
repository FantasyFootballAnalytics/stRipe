#' @export object_list
object_list <- R6::R6Class(
  "list",
  public = list (
    object = "list", data = list(), has_more = NULL, total_count = NULL,
    url = NULL,
    initialize = function(data = list(), has_more = NULL,
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
                              "coupon" = "newCoupon"
                            )
                            object_param <- setdiff(names(list_data), "object")

                            return(do.call(obj_func, list_data))
                          }
                          )
    }
  )
)
newList <- function(...){object_list$new(...)}
