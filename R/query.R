# Get one dataset from an HDF5 database
# TODO H5PLEXOS: The get_table() function in the original rplexos needs to be finished getting rewritten as a function
# that approximates special tables within an rplexos SQLite database:
# - data_properties (implemented), a table containing all query-able properties (one in each row) and their metadata for 
#   every scenario [used in plexos_open() and query_property()]
# - metadata_properties (implemented), a table containing every interval timestamp for every scenario [used in sum_master()]
# - config, a table containing PLEXOS run configuration data (e.g. version, date, time, file path, 
#   model, user) for every scenario [used only in query_config()]
# - log_info, a table containing log file info that's optionally passed to the solution file, 
#   including solution times and infeasibilities, for every scenario [used only in query_log()]
# - log_steps, a table containing solution times and ?? other stuff optionally passed to the solution
#   file for every scenario [used only in query_log_steps]
#
# Any of the tables above that don't have a corresponding function directly below in get_table()
# aren't currently implemented.

get_table <- function(filename, table) {
  if (table == 'data_properties'){
  out <- h5ls(filename) %>% 
    filter(grepl('/data',.$group)) %>% 
    filter(otype == 'H5I_DATASET') %>%
    mutate(dataset_name = paste0(.$group,'/',.$name)) %>% 
    group_by(group,name, dataset_name) %>% 
    do(unit = h5readAttributes(file = filename,
                               name = .$dataset_name)$units) %>% 
    ungroup() %>% 
    mutate(unit = unlist(unit)) %>% 
    mutate(group = stringr::str_replace(group,'/data/',''),
           count_band = 1,
           count_sample = 1,
           count_timeslice = 1) %>% 
    tidyr::separate(group,c('phase','time','collection'),'/') %>% 
    #mutate(is_summary = ifelse(.$time == 'interval',0,1)) %>% 
    add_phase_ids %>% 
    select(collection, 
           property = name, 
           unit, 
           phase_id, 
           #is_summary,
           time,
           dataset_name, 
           count_band,  # need to figure out what these last three actually do and not hardcode them to one
           count_sample, 
           count_timeslice)

  # Close H5
  H5close()
  # Return result
  out
  } else if (table == 'metadata_properties'){
    out <- h5ls(filename) %>% 
      filter(grepl('/metadata',.$group)) %>% 
      filter(otype == 'H5I_DATASET') %>%
      mutate(dataset_name = paste0(.$group,'/',.$name)) %>% 
      mutate(metadata_type = stringr::str_replace(group,'/metadata/','')) %>% 
      select(metadata_type, 
             name, 
             dataset_name)
    
    # Close H5
    H5close()
    # Return result
    out
  } else if (table == 'relation') {
    stop('You are trying to query relations/memberships but they are not implemented yet.')
  } else {
    stop('You are calling a table that does not exist or has not been implemented yet.')
  }
}

##############

# Get a list of all datasets in an HDF5 database
get_list_datasets <- function(filename) {
  # Read names
  out <- h5ls(filename) %>% 
    filter(otype == 'H5I_DATASET') %>% 
    select(dataset = name)
  
  # Return result
  out
}

# Get a table for all scenarios
get_table_scenario <- function(db, from) {
  # Check inputs
  check_rplexos(db)
  stopifnot(is.character(from), length(from) == 1L)
  
  db %>%
    group_by(scenario, position, filename) %>%
    do(get_table(.$filename, from)) %>%
    ungroup()
}

#' Get list of available properties
#'
#' Produce a list of available properties, their units and the collections they belong to.
#' Additionally, a column is created for each scenario that indicates in how many databases
#' the property appears.
#'
#' @inheritParams query_master
#' @seealso \code{\link{plexos_open}} to create the PLEXOS database object
#' @family special queries
#'
#' @examples
#' # Process the folder with the solution file provided by rplexos
#' location <- location_solution_rplexos()
#' process_folder(location)
#'
#' # Query data
#' db <- plexos_open(location)
#' query_property(db)
#'
#' @export
query_property <- function(db) {
  get_table_scenario(db, "data_properties") %>%
    add_phase_names %>%
    group_by(phase_id, phase, time, collection, property, unit, scenario) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(scenario, n) %>%
    as.data.frame
}

# Query databases ***********************************************************************

#' Query data and aggregate data
#'
#' This collection of functions retrieves data from the processed PLEXOS solutions and
#' returns it in a convenient format.
#'
#' The family \code{query_*} returns the raw data in the databases, while \code{sum_*}
#' aggregates the data according to \code{columns}.
#'
#' The functions \code{*_day}, \code{*_week}, \code{*_month} and \code{*_year} are
#' shortcuts for the corresponding, \code{*_master} function.
#'
#' The following is a list of valid items for \code{columns} and filtering. Additionally,
#' \code{time} can be specified for summary data (interval data always includes \code{time}).
#' \itemize{
#'   \item{\code{category}}
#'   \item{\code{property}}
#'   \item{\code{name} (default for columns)}
#'   \item{\code{parent} (automatically selected when \code{name} is selected)}
#'   \item{\code{category}}
#'   \item{\code{region} (only meaningful for generators)}
#'   \item{\code{zone} (only meaningful for generators)}
#'   \item{\code{period_type}}
#'   \item{\code{band}}
#'   \item{\code{sample}}
#'   \item{\code{timeslice}}
#' }
#'
#' If defined, the \code{filter} parameter must be a \code{list}. The elements must be chracter
#' vectors and need to have a valid column name (see previous bullet points). For example, one
#' could define it as follows:
#'
#' \code{filter = list(name = c("Generator1", "Generator2"), region = "Region1")}
#'
#' To filter by time use the \code{time.range} parameter, instead of adding it as an entry in the
#' \code{filter} parameter. For example use \code{c("2015-03-14", "2015-03-15")} in your query.
#' Please note that the year/month/date starts at midnight (00:00:00).
#'
#' If a scenario has multiple databases, the data will be aggregated automatically. If two or more
#' databases within the same scenario have overlapping time periods, the default is to select the
#' data from the last database (execute \code{summary(db)} so see the order). To change this behavior
#' set the global option \code{rplexos.tiebreak} to \code{first}, \code{last}, or \code{all} to
#' select data from the first database, the last one or keep all of them.
#'
#' Multiple properties can be queried within a collection. If \code{prop} equals the widcard
#' \code{"*"}, all the properties within a collection are returned.
#'
#' The parameter \code{multiply.time} allows to multiply values by interval duration (in hours) when
#' doing the sum of interval data. This can be used, for example, to obtain total energy (in MWh)
#' from power time series (in MW).
#'
#' @param db PLEXOS database object
#' @param time character. Table to query from (interval, day, week, month, year)
#' @param col character. Collection to query
#' @param prop character vector. Property or properties to query
#' @param columns character. Data columns to query or aggregate by (defaults to \code{name})
#' @param time.range POSIXt or character. Range of dates of length 2 (given as date, datetime or character in 'ymdhms' 
#'                   or 'ymd' format). The Plexos data is assumed to be in UTC so providing a POSIXt vector in a different
#'                   timezone might cause conflicts. Character vectors are also converted to the UTC format, so here there
#'                   is not issue.
#' @param filter list. Used to filter by data columns (see details)
#' @param phase integer. PLEXOS optimization phase (1-LT, 2-PASA, 3-MT, 4-ST)
#' @param multiply.time boolean. When summing interval data, provide the value multiplied by interval duration (See details).
#' @param ... parameters passed from shortcut functions to master (all except \code{time})
#'
#' @return A data frame that contains data summarized/aggregated by scenario.
#'
#' @seealso \code{\link{plexos_open}} to create the PLEXOS database object
#'
#' @examples
#' # First process
#' location <- location_solution_rplexos()
#' process_folder(location)
#'
#' # Query data
#' db <- plexos_open(location)
#' query_day(db, "Generator", "Generation")
#' query_day(db, "Region", "*")
#' query_interval(db, "Generator", "Generation")
#'
#' @export
#' @importFrom data.table data.table CJ
#' @importFrom foreach foreach %dopar%
query_master <- function(db, time, col, prop, columns = "name", time.range = NULL, filter = NULL, phase = 4) {
  which_time <- time #alias to avoid confusion with `time` column in db
  # Check inputs
  check_rplexos(db)
  stopifnot(is.character(time), length(time) == 1L)
  stopifnot(is.character(col), length(col) == 1L)
  stopifnot(is.character(prop), is.character(columns))
  stopifnot(is.numeric(phase), length(phase) == 1L)
  if (!time %in% c("interval", "day", "week", "month", "year"))
    stop("'time' must be one of: interval, day, week, month or year", call. = FALSE)
  if(!phase %in% 1:4)
    stop("'phase' must be one of: 1 (LT), 2 (PASA), 3 (MT) or 4 (ST)", call. = FALSE)
  if(!all(columns %in% valid_columns()))
    stop("Incorrect column parameter. Use valid_columns() to get the full list.", call. = FALSE)
  
  # Key filter checks
  if (!is.null(filter)) {
    stopifnot(is.list(filter))
    if ("time" %in% names(filter))
      stop("time should not be an entry in filter. Use time.range instead.", call. = FALSE)
    if(!all(names(filter) %in% valid_columns()))
      stop("The names in 'filter' must correspond to correct columns. Use valid_columns() to get the full list.", call. = FALSE)
  }
  
  # Time range checks
  if (!is.null(time.range)) {
    stopifnot(length(time.range) == 2L)
    
    if (inherits(time.range, "POSIXt")) {
      time.range2 <- time.range
    } else {
      time.range2 <- c(NA, NA)
      
      if (inherits(time.range, "character")) {
        time.range2 <- lubridate::parse_date_time(time.range, c("ymdHMS", "ymd"), quiet = TRUE)
      }
      
      if(any(is.na(time.range2)))
        stop("time.range must be POSIXt or character with 'ymdHMS' or 'ymd' formats", call. = FALSE)
    }
    
    # Convert dates to ymdhms format, so that queries work correctly
    time.range <- format(time.range2, "%Y-%m-%d %H:%M:%S")
  }
  
  ### BEGIN: Master query checks
  
  # Get list of properties for the collection
  res <- bind_rows(db$properties) %>%
    filter(collection == col, time == time, phase_id == phase, time == which_time)
  
  # Check that collection is valid
  if (nrow(res) == 0L) {
    stop("Collection '", col, "' is not valid for ",
         which_time, " data and phase '", phase, "'.\n",
         "   Use query_property() for list of collections and properties.",
         call. = FALSE)
  }
  
  # Checks if properties are valid
  if (!identical(prop, "*")) {
    invalid.prop <- setdiff(prop, res$property)
    if (length(invalid.prop) > 0L) {
      stop("Properties ", paste0("'", invalid.prop, "'", collapse = ", "), " in collection '", col,
           "' are not valid for ", which_time, " data and phase '", phase, "'.\n",
           "   Use query_property() for list of available collections and properties.",
           call. = FALSE)
    }
    
    # Filter properties
    res <- res %>%
      filter(property %in% prop)
  }
  
  # Find if the data is going to have multiple sample, timeslices or bands
  # TODO H5: This is not yet implemented in the H5PLEXOS-based version of rplexos.
  res2 <- res %>%
    ungroup() %>%
    summarize(is_multi_band      = max(count_band) > 1,
              is_multi_sample    = max(count_sample) > 1,
              is_multi_timeslice = max(count_timeslice) > 1)
  if (res2$is_multi_timeslice)
    columns <- c(setdiff(columns, "timeslice"), "timeslice")
  if (res2$is_multi_band)
    columns <- c(setdiff(columns, "band"), "band")
  if (res2$is_multi_sample)
    columns <- c(setdiff(columns, "sample"), "sample")
  
  # Columns should not include collection and property; they are always reported
  columns <- setdiff(columns, c("collection", "property"))
  
  # If columns include name, add parent automatically
  if ("name" %in% columns)
    columns <- c("name", "parent", setdiff(columns, c("name", "parent")))
  
  ### END: Master query checks
  
  # Query data for each property
  db2 <- db %>%
    group_by(scenario, position)
  
  if (TRUE ) { #!is_parallel_rplexos()) { #TODO H5: implement this later once I figure out how to set rplexos_globals
    out <- db2 %>%
      do(query_master_each(., time, col, prop, columns, time.range, filter, phase))
  } else {
    out <- foreach(i = db2$position, .combine = bind_rows,
                   .packages = c("dplyr", "rplexos")) %dopar% {
                     db2 %>%
                       filter(position == i) %>%
                       do(query_master_each(., time, col, prop, columns, time.range, filter, phase))
                   }
  }
  
  # Ungroup results
  out <- out %>% ungroup
  
  # Return empty dataframe if no results were returned
  if (nrow(out) == 0) {
    warning("Query returned no results", call. = FALSE)
    return(data.frame())
  }
  
  # Check if any scenario is missing from the results
  missing.scenario <- setdiff(unique(db$scenario), unique(out$scenario))
  if (length(missing.scenario) >= 1L) {
    warning("Query returned no results for scenario(s): ",
            paste(missing.scenario, collapse = ", "),
            call. = FALSE)
  }
  
  # Solve ties if they exist
  out <- out %>%
    solve_ties()
  
  out
}

#' Open one database and query data
#'
#' Used internally by \code{\link{query_master}}. Use that function instead to access data. The
#' use of this function is not recommended
#'
#' @inheritParams query_master
#'
#' @seealso \code{\link{query_master}} to query PLEXOS solutions
#'
#' @keywords internal
#' @export
query_master_each <- function(db, time, col, prop, columns = "name", time.range = NULL, filter = NULL, phase = 4) {
  which_time <- time #alias to avoid confusion with `time` column in db
  # Grab whichever datasets should be queried:
  datasets_to_query <- data.frame(db$properties) %>% 
    filter(.$time == which_time, collection == col, property %in% prop, phase_id == phase ) %>% 
    select(collection,property,dataset_name,unit,phase_id)
  metadata_to_query <- get_table(db$filename,'metadata_properties') %>% 
    filter(grepl(col,.$name))  #implement this get_table in plexos_open instead and include it in db object
  
  # First, get the list of objects.
  # If there's a match with a dataset of metadata_type "object", the output is name and category. If the type is "relation", 
  # the output is parent (which is "name") and child. 
  objects <- h5read(file = db$filename, 
                    (metadata_to_query %>% 
                      filter(.$name == col))$dataset_name)
  # If the metadata for the objects is a relation, change the names:
  if('parent' %in% names(objects)) names(objects) <- c('name','category')
  
  # Query timestamps:
  timestamps <- data.frame(time = h5read(file = db$filename, 
                                         name =  paste0('/metadata/times/',which_time))) %>% 
    mutate(time = lubridate::ymd_hms(time, quiet = TRUE))
  
  # Grab the number of time periods to offset the beginning of the records by:
  period_offset <- h5readAttributes(file = db$filename,
                                    name = datasets_to_query$dataset_name[1])$period_offset

  # Now construct the output by querying for the values, the names of the associated objects, any relations requested: 
  values <- data.frame(datasets_to_query) %>% 
    group_by(dataset_name,collection,property,unit) %>% 
    do(value = matrix(drop(h5read(file = db$filename,
                               name = .$dataset_name)))) %>% 
    ungroup() %>%
    mutate(unit = unlist(unit)) %>% 
    tidyr::unnest() %>% 
    mutate(value = drop(pull(.$value,value)))
  
  # The timestamps in an h5plexos db include an entire year's worth of entries even if the solution is
  # for a model that has a horizon of, say, six months. So, only keep the number of entries equal to the
  # number of columns in each dataset.
  num_timestamps_in_query <- dim(values)[1]/length(prop)/dim(objects)[1]
  
  # Add timestamps and object names:
  print(sprintf('dims of values div num props: %s   dims of objects$name: %s',dim(values)[1]/length(prop),dim(objects$name)))
  out <- values %>% 
    mutate(time = rep(timestamps[seq(period_offset + 1, period_offset + num_timestamps_in_query),], 
                      length.out =dim(values)[1])) %>% 
    mutate(name = rep(objects$name,
                      each = num_timestamps_in_query,
                      length.out =dim(values)[1])) %>% 
    # mutate(name = rep(objects$category,
    #                   each = num_timestamps_in_query,
    #                   length.out =dim(values)[1])) %>% 
    select(collection,
           property,
           unit,
           name,
           time,
           value)
  
  # For yearly queries, the HDF5 only reports one timestamp of e.g. "<year>-12-31T00:00:00", but we need the timestamp
  # to be the first hour of the model so that the tie-breaker for overlapping data isn't triggered:
  if(time == 'year') {
    # Get the starting date from the first dataset with a monthly entry--if there aren't any, try interval data, and if there
    # are no monthly or interval data, leave the dates the same and keep all entries even with the same time.
    props_avail <- data.frame(db$properties) %>%
      filter(time != 'year',phase_id == datasets_to_query$phase_id[1]) %>% 
      arrange(desc(time))
          
    if(length(props_avail) == 0L) {
      warning("No interval or monthly outputs for the same phase as the query target are present in the solution to be used to set dates for the yearly query,\n",
              "so only one solution's output will be used per solution, overwriting the others.", 
              call = FALSE)
      # TODO: Fix the above so all entries are kept. No good solution for this. Probably should pass a flag column back to query_master.
    } else {
      timestamps_for_year <- data.frame(time = h5read(file = db$filename, 
                                                  name =  paste0('/metadata/times/',props_avail$time[1]))) %>% 
        mutate(time = lubridate::ymd_hms(time, quiet = TRUE))
      period_offset_for_year <- h5readAttributes(file = db$filename,
                       name = props_avail$dataset_name[1])$period_offset
      
      out <- mutate(out, time = timestamps_for_year[period_offset_for_year + 1,]) 
      }
  }
  
  
  # BEGIN Dynamic selection of the columns to be included (this used to be in its own function, select_rplexos(), 
  # but constructing the columns from an h5 requires many of the local variables in this main function):
  columns.dots <- c("collection", "property", "unit", setdiff(columns, "time"), "time", "value")
  
  relations <- data.frame(t(data.frame(strsplit(metadata_to_query$name,"_"))))
  names(relations) <- c('parent','child')
  
  if("category" %in% columns.dots){
    if("category" %in% names(objects)) {
      warning(sprintf("within select_rplexos, dim objects$category: %s   dim num_timestamps_in_query: %s   dim out: ",dim(objects$category),dim(num_timestamps_in_query),dim(out)[1]), call. = FALSE)
      out <- mutate(out, category = rep(objects$category,
                                    each = num_timestamps_in_query,
                                    length.out =dim(out)[1]))
    } else {
      warning("This collection is a relation rather than an object, so it doesn't have 'categories' defined. Returning its children in that column instead.", call. = FALSE)
      out <- mutate(out, category = rep(objects$child,
                                      each = num_timestamps_in_query,
                                      length.out =dim(out)[1]))
    } 
  }
  
  if("region" %in% columns.dots){
    if(any(grepl("region",relations$parent))) {
      objects <- left_join(out, h5read(file = db$filename, 
                                       name = metadata_to_query[grepl("region",relations$parent),]$dataset_name),
                           by = c('name' = 'child'))
    } else if(any(grepl("region",relations$child))) {
      objects <- left_join(out, h5read(file = db$filename, 
                                       name = metadata_to_query[grepl("region",relations$child),]$dataset_name),
                           by = c('name' = 'parent'))
    }
    warning("Participation factors aren't reported in the h5 database,\n",
            "so any object that has memberships in multiple regions will appear multiple times with its full value\n",
            "(i.e. don't blindly sum up this table)", call. = FALSE)
  }
  
  if("zone" %in% columns.dots){
    if(any(grepl("zone",relations$parent))) {
      objects <- left_join(out, h5read(file = db$filename, 
                                       name = metadata_to_query[grepl("zone",relations$parent),]$dataset_name),
                           by = c('name' = 'child'))
    } else if(any(grepl("zone",relations$child))) {
      objects <- left_join(out, h5read(file = db$filename, 
                                       name = metadata_to_query[grepl("zone",relations$child),]$dataset_name),
                           by = c('name' = 'parent'))
    }
    warning("Participation factors aren't reported in the h5 database,\n",
            "so any object that has memberships in multiple zones will appear multiple times with its full value\n",
            "(i.e. don't blindly sum up this table)", call. = FALSE)  }
  
  if(any(c("period_type","band","sample","timeslice") %in% columns.dots)) {
    stop("The columns 'period_type,' 'band,' 'sample,' and 'timeslice' aren't implemented in the HDF5 version of rplexos yet.", call. = FALSE)
  }
  # END column selection
  
  print(sprintf('out before select_rplexos call for %s:',db$filename))
  print(tail(out))
  out <- out %>%
    filter_rplexos(filter) %>%
    filter_rplexos_time(time.range) %>%
    collect(n=Inf)
  
  # Close H5
  H5close()
  
  print('out AFTER select_rplexos call:')
  print(tail(out))
  # Return value
  return(out)
}


# Deal with repeats
solve_ties <- function(x, opt = getOption("rplexos.tiebreak")) {
  # Get option to see how to deal with ties (defaults to last)
  if (is.null(opt)) {
    opt <- "last"
  } else if (!opt %in% c("first", "last", "all")) {
    warning("Invalid 'rplexos.tiebreak' option (must be one of: first, last, all). Using last instead", call. = FALSE)
    opt <- "last"
  }
  
  if (opt %in% c("first", "last")) {
    # Group by time
    x2 <- x %>%
      ungroup() %>%
      group_by(scenario, time)
    
    if (identical(opt, "last")) {
      # If there are repeats, use the latter entry
      x2 <- x2 %>%
        filter(position == max(position))
    } else {
      # If there are repeats, use the latter entry
      x2 <- x2 %>%
        filter(position == min(position))
    }
    
    # Ungroup and delete path column
    x2 <- x2 %>%
      ungroup() %>%
      select(-position)
  }
  
  x2
}

#' @rdname query_master
#' @export
query_interval <- function(db, ...) query_master(db, "interval", ...)
#' @rdname query_master
#' @export
query_day      <- function(db, ...) query_master(db, "day", ...)
#' @rdname query_master
#' @export
query_week     <- function(db, ...) query_master(db, "week", ...)
#' @rdname query_master
#' @export
query_month    <- function(db, ...) query_master(db, "month", ...)
#' @rdname query_master
#' @export
query_year     <- function(db, ...) query_master(db, "year", ...)


# Aggregation ***************************************************************************

#' @rdname query_master
#' @export
sum_master <- function(db, time, col, prop, columns = "name", time.range = NULL, filter = NULL, phase = 4, multiply.time = FALSE) {
  # Check inputs to unique
  stopifnot(is.logical(multiply.time), length(multiply.time) == 1L)
  
  # Make sure to include time
  columns2 <- c(setdiff(columns, "time"), "time")
  
  # Run query_master to get the raw data
  out <- query_master(db, time, col, prop, columns2, time.range, filter, phase)
  
  # If empty query is returned, return empty data.frame
  if(nrow(out) == 0L)
    return(data.frame())
  
  if (identical(time, "interval") && (!"time" %in% columns) && multiply.time) {
    # Get length of intervals in hours
    times <- get_table_scenario(db, "time")
    delta <- times %>%
      group_by(scenario) %>%
      mutate(time = lubridate::ymd_hms(time, quiet = TRUE)) %>%
      summarize(interval = difftime(lead(time), time, units = "hours") %>%
                  min(na.rm = TRUE) %>%
                  as.numeric)
    
    # Add interval duration to the sum
    out <- out %>%
      inner_join(delta, by = "scenario") %>%
      group_by_char(c("scenario", "collection", "property", columns)) %>%
      summarise(value = sum(value * interval))
    
    # If unit is a column, modify column
    if ("unit" %in% names(out)) {
      out <- out %>%
        mutate(unit = paste(unit, "* h"))
    }
  } else {
    # Sum values
    out <- out %>%
      group_by_char(c("scenario", "collection", "property", columns)) %>%
      summarise(value = sum(value))
  }
  
  out
}

#' @rdname query_master
#' @export
sum_interval <- function(db, ...) sum_master(db, "interval", ...)
#' @rdname query_master
#' @export
sum_day      <- function(db, ...) sum_master(db, "day", ...)
#' @rdname query_master
#' @export
sum_week     <- function(db, ...) sum_master(db, "week", ...)
#' @rdname query_master
#' @export
sum_month    <- function(db, ...) sum_master(db, "month", ...)
#' @rdname query_master
#' @export
sum_year     <- function(db, ...) sum_master(db, "year", ...)


# Filtering *****************************************************************************

# Time filter
filter_rplexos_time <- function(out, time.range, modified = FALSE) {
  # Do nothing if time.range is empty
  if (!is.null(time.range)) {
    if (modified) {
      out <- filter(out, time_from <= time.range[2], time_to >= time.range[1])
    } else {
      out <- filter(out, between(as.Date(time), as.Date(time.range[1]), as.Date(time.range[2])))
    }
  }
  
  out
}

# Other filters
filter_rplexos <- function(out, filt) {
  # Do nothing if filter is empty
  if (is.null(filt))
    return(out)
  if (length(filt) == 0L)
    return(out)
  
  # Split in positive and negative filters
  filt_out <- lapply(filt, function(x){
    neg <- substr(x, 1, 1)=='-'
    x <- x[neg]
    x <- sapply(x, function(y) substr(y,2,nchar(y)+1), USE.NAMES = F) # remove the minus
    x
  })
  filt_out <- filt_out[lapply(filt_out,length)>0] # remove empty categories
  
  filt_in <- lapply(filt, function(x){
    pos <- substr(x, 1, 1)!='-'
    x[pos]
  })
  filt_in <- filt_in[lapply(filt_in,length)>0] # remove empty categories
  
  # Write the condition as text
  if(length(filt_out)>0){
    vals_out <- lapply(filt_out, function(x)
      paste0("\"", x, "\"", collapse = ", ")) %>%
      paste0("c(", ., ")")
    cons_out <- ifelse(lapply(filt_out, length) == 1L, "==", "%in%")
    cond_out <- paste('!(', names(filt_out), cons_out, vals_out, ')')
    out <- out %>%
      filter_(.dots = cond_out) # Apply condition
  }
  
  if(length(filt_in)>0){
    vals_in <- lapply(filt_in, function(x)
      paste0("\"", x, "\"", collapse = ", ")) %>%
      paste0("c(", ., ")")
    cons_in <- ifelse(lapply(filt_in, length) == 1L, "==", "%in%")
    cond_in <- paste(names(filt_in), cons_in, vals_in)
    out <- out %>%
      filter_(.dots = cond_in) # Apply condition
  }
  
  out
}


