# Regroup with characters
group_by_char <- function(x, vars) {
  dots <- vars %>%
    as.list %>%
    lapply(as.symbol)
  group_by_(x, .dots = dots)
}

#' Get list of valid columns
#'
#' List of valid columns accepted in \code{\link{query_master}}, \code{\link{sum_master}} and related functions.
#'
#' @seealso \code{\link{query_master}}, \code{\link{sum_master}}
#'
#' @export
# TODO H5: Update this for H5PLEXOS version of rplexos.
valid_columns <- function() c("collection", "property", "name", "parent", "category", "region", "zone",
                              "period_type_id", "band", "sample", "timeslice", "time")


# For backwards compatibility with dplyr < 0.6 define a `pull` function to grab a vector from a tbl:
pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}

#### Validation rules ####

# Check that object is valid rplexos databasae
check_rplexos <- function(x) {
  if(!inherits(x, "rplexos"))
    stop("db is not a valid database object. It should be created with plexos_open().", call. = FALSE)
}

# Check that a vector of characters are folder names
check_is_folder <- function(x) {
  if ((length(x) == 1L) && identical(x, "*")) {
    test <- TRUE
  } else {
    x_folder <- file.exists(x) & file.info(x)$isdir
    test <- all(x_folder, na.rm = FALSE)
  }
  
  if (!test)
    stop(paste0("'folders' must be a vector of existing folders or the wildcard \"*\". ",
                "The following folders were no folders: '",
                paste0(x[!x_folder], collapse = "', '"),
                "'."), call. = FALSE)
}