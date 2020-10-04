# Importing packages
library(RestRserve)
library(jsonlite)
library(dplyr)
library(tidyselect)

# Creating model function
model_cluster_kmeans <- function(request, response) {

  dataset <- jsonlite::fromJSON(request$body)

    if (is.null(dataset)) {
    raise(HTTPError$bad_request())
  }

  if (is.null(request$parameters_query[["centers"]]))
    response$status_code <- 400L

  if (is.null(request$parameters_query[["iter.max"]]))
    request$parameters_query[["iter.max"]] <- 10

  if (is.null(request$parameters_query[["nstart"]]))
    request$parameters_query[["nstart"]] <- 1

  if (is.null(request$parameters_query[["algorithm"]]))
    request$parameters_query[["algorithm"]] <- "Lloyd"

  # Select only numerical attributes
  dataset <-
    dplyr::select(dataset, where(is.numeric))

  # Creating a clustering with the provided parameters
  kmeans_clustering <-
    kmeans(x           = dataset,
           centers     = request$parameters_query[["centers"]],
           iter.max    = request$parameters_query[["iter.max"]],
           nstart      = request$parameters_query[["nstart"]],
           algorithm   = request$parameters_query[["algorithm"]])

  kmeans_obj <- kmeans_clustering

  dataset$group_id <- kmeans_obj$cluster
  response$body = dataset
  response$content_type = "application/json"
  response$headers = character(0)
  response$status_code = 200L
}

# Creating new app
app = Application$new(content_type = "application/json")
app$add_post(path = "/clustering/kmeans", FUN = model_cluster_kmeans)
#app$add_openapi()

backend = BackendRserve$new()
backend$start(app, http_port = 9102)
