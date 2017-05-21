
#' @export
get_card_source <- function(params = list()){
  param_names <- intersect(names(params),
                           c("exp_month", "exp_month", "exp_year", "object",
                             "number", "cvc", "name", "address_city",
                             "address_country", "address_line1",
                             "address_line2", "address_state",
                             "address_zip", "currency",
                             "default_for_currency", "card_metadata"))
  if(length(param_names) == 0)
    return(NULL)

  card_param <- params[param_names]
  names(card_param)[which(names(card_param) == "card_metadata")] <- "metadata"

  card_source <- list()
  for(param_name in setdiff(names(card_param), "metadata")){
    if(!is.null(eval.parent(card_param[[param_name]], n = 2)))
      card_source[[paste0("source[", param_name, "]")]] <- eval.parent(card_param[[param_name]], n = 2)
  }


  if(is.null(eval.parent(card_param[["metadata"]], n = 2)))
    card_source[["source[metadata]"]] <- NULL
  else
    for(m_name in names(card_param[["metadata"]]))
      card_source[[paste0("source[metadata[", m_name, "]]")]] <- eval.parent(card_param["metadata"][[m_name]], n = 2)

  if(length(card_source) > 0)
    return(card_source)
  else
    return(NULL)
}
