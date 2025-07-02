#' A \code{Credentials} Class Representing a Credentials object
#'
#' @description
#' A \code{Credentials} object has a username, a password and a key.
#'
#' @details
#' A \code{Credentials} object can get API data via an API URL.

Credentials <- R6::R6Class(
  "NSSPCredentials",
  private = list(
    ..username = NSSPContainer$new(NULL),
    ..password = NSSPContainer$new(NULL),
    ..__ = NSSPContainer$new(stringi::stri_rand_strings(1, 1024, pattern = "[A-Za-z0-9*-+=/_$@.?!%|;:#~<>()[]\`\']"))
  ),
  public = list(

    #' @description
    #' Initializes a new Credentials object.
    #' @param username a string for username
    #' @param password a string for password
    #' @return A new \code{Credentials} object
    initialize = function(username = NULL, password = NULL) {
      if (!missing(username)) {
        private$..username <- NSSPContainer$new(safer::encrypt_string(username, key = private$..__$value, ascii = FALSE))
      }
      if (!missing(password)) {
        private$..password <- NSSPContainer$new(safer::encrypt_string(password, key = private$..__$value, ascii = FALSE))
      }
    },

    #' @description
    #' Get API response
    #' @param url a character of API URL
    #' @param http_version curl http version (default NULL)
    #' @return An object of class \code{response}
    #' @examples
    #' \dontrun{
    #' myProfile <- Credentials$new(askme("Enter my username: "), askme())
    #' url <- "https://httpbin.org/json"
    #' api_response <- myProfile$get_api_response(url)
    #' }
    get_api_response = function(url, http_version=NULL) {
      
      # check url is string
      assertions::assert_string(url)
      
      # create config for GET; start with empty, and add http_version if specified
      cfg = httr::config()
      if(!is.null(http_version)) cfg <- c(cfg, httr::config(http_version=http_version))
      
      
      # add the authenticate() if necessary  
      if (!is.null(private$..password$value)) {
        cfg = c(
          cfg, 
          httr::authenticate(
            private$..username$value %>% safer::decrypt_string(., private$..__$value),
            private$..password$value %>% safer::decrypt_string(., private$..__$value)
          )
        )
      }

            # execute GET with the cfg
      res <- httr::GET(url, config = cfg)
      
      res$request$options$userpwd <- ""
      cli::cli_alert_info(httr::http_status(res$status_code)$message)
      return(res)
    },

    #' @description
    #' Get API data
    #' @param url a character ofAPI URL
    #' @param fromCSV a logical, defines whether data are returned in .csv format or .json format
    #' @param ... further arguments and CSV parsing parameters to be passed to \code{\link[readr]{read_csv}} when \code{fromCSV = TRUE}.
    #' @return a dataframe (\code{fromCSV = TRUE}) or a list containing a dataframe and its metadata (\code{fromCSV = TRUE})
    #' @examples
    #' \dontrun{
    #' myProfile <- Credentials$new(askme("Enter my username: "), askme())
    #' json_url <- "https://httpbin.org/json"
    #' api_data_json <- myProfile$get_api_data(json_url)
    #'
    #' csv_url <- "https://httpbin.org/robots.txt"
    #' api_data_csv <- myProfile$get_api_data(csv_url, fromCSV = TRUE)
    #' }
    get_api_data = function(url, fromCSV = FALSE, ...) {
      
      dots <- list(...)
      
      if("http_version" %in% names(dots)) {
        http_version_val <- dots[["http_version"]]
        dots[["http_version"]] <- NULL
      } else {
        http_version_val <- NULL
      }
                   
      assertions::assert_string(url)
      apir <- self$get_api_response(url, http_version=http_version_val)
      if(apir$status_code == 200){
        if(any("data.frame" %in% class(httr::content(apir, as = "text")))){
          return(httr::content(apir, as = "text"))
        }
        apir %>% {
          if (fromCSV) {
            content <- httr::content(., by = "text/csv")
            do.call(readr::read_csv, args = c(list(file = content), dots))
          } else {
            httr::content(., as = "text") %>% jsonlite::fromJSON()
          }
        }
      }
    },

    #' @description
    #' Get API graph
    #' @param url a character of API URL
    #' @param file_ext a non-empty character vector giving the file extension. Default is \code{.png}.
    #' @param http_version (default= NULL); set to CURL HTTP VERSION if desired
    #' @return A list containing an api_response object and a path to a time series graph in .png format
    #' @examples
    #' \dontrun{
    #' myProfile <- Credentials$new(askme("Enter my username: "), askme())
    #' url <- "<url>"
    #' api_data_graph <- myProfile$get_api_graph(url)
    #' names(api_data_graph)
    #' img <- png::readPNG(api_data_graph$graph)
    #' grid::grid.raster(img)
    #' }
    get_api_graph = function(url, file_ext = ".png", http_version=NULL) {
      assertions::assert_string(url)
      graph <- tempfile(fileext = file_ext)
      
      # create config for GET; start with empty, and add http_version if specified
      cfg = httr::config()
      if(!is.null(http_version)) cfg <- c(cfg, httr::config(http_version=http_version))
      
      # It is not clear to me why the api_get_response function checks private before
      # adding this. However, rather than add that check, I'll retain the lack of checking
      # and just add it directly to the config
      cfg = c(
        cfg, 
        httr::authenticate(
          private$..username$value %>% safer::decrypt_string(., private$..__$value),
          private$..password$value %>% safer::decrypt_string(., private$..__$value)
        ), 
        httr::write_disk(graph, overwrite = TRUE)
      )
      
      apir <- httr::GET(url, config=cfg)
      
      apir$request$options$userpwd <- ""
      
      cli::cli_alert_info(httr::http_status(apir$status_code)$message)
      list("api_response" = apir, "graph" = graph)
    }
  )
)
