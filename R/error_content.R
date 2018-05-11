stripe_error <- function(error_content, http_code){
  error_type <- switch (error_content$type,
                        "api_connection_error" = "API Connection Error",
                        "api_error" = "API error",
                        "authentication_error" = "Authentication Error",
                        "card_error" = "Card error",
                        "invalid_request_error" =	"Invalid Request Error",
                        "rate_limit_error" = "Rate Limit Error"
  )

  error_type <- paste(error_type, paste0("(HTTP ", http_code, ")"))
  error_code <- NA
  error_message <- NA

  default_message <- switch (error_content$type,
                             "api_connection_error" = "Failure to connect to Stripe's API",
                             "api_error" = "API error",
                             "authentication_error" = "Failure to properly authenticate yourself in the request",
                             "card_error" = "Card error",
                             "invalid_request_error" =	"Invalid request/invalid parameters",
                             "rate_limit_error" = "Too many requests hit the API too quickly"
  )
  if(any(names(error_content) == "code"))
    error_code <- switch(error_content$code,
                         "invalid_number" = "The card number is not a valid credit card number.",
                         "invalid_expiry_month" = "The card's expiration month is invalid.",
                         "invalid_expiry_year"= "The card's expiration year is invalid.",
                         "invalid_cvc" =  "The card's security code is invalid.",
                         "incorrect_number" = "The card number is incorrect.",
                         "expired_card" = "The card has expired.",
                         "incorrect_cvc" = "The card's security code is incorrect.",
                         "incorrect_zip" =  "The card's zip code failed validation.",
                         "card_declined" = "The card was declined.",
                         "missing" = "There is no card on a customer that is being charged.",
                         "processing_error" = "An error occurred while processing the card.",
                         error_content$code)

  if(any(names(error_content) == "message"))
    error_message <- error_content$message
  else
    error_message <- default_message

  error_text <- paste(error_type, error_message, sep = "\n")
  if(!is.na(error_code))
    error_text <- paste(error_text, error_code, sep = "\n")

  if(any(names(error_content) =="param"))
    error_text <- paste(error_text, error_content$param, sep = "\n")

  return(error_text)
}
