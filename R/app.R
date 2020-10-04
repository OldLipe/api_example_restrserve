library(RestRserve)
library(jsonlite)



model_lm <- function(data, ...) {
  lm(mpg ~cyl, data)
}

model_handler_residuals <- function(request, response) {
  n = jsonlite::fromJSON(request$body)
  if (is.null(n)) {
    raise(HTTPError$bad_request())
  }

  fit_model <- model_lm(n)

  response$body = fit_model$residuals
  response$content_type = "application/json"
  response$headers = character(0)
  response$status_code = 200L
}

# Creating new app
app = Application$new(content_type = "application/json")
app$add_post(path = "/predict", FUN = model_handler_residuals)
#app$add_openapi()

backend = BackendRserve$new()
backend$start(app, http_port = 9102)
