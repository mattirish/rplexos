# Functions to quickly query certain information from the solution

#' Get summary information from all databases
#'
#' Get the list of phases, samples, timeslices and bands  that are available in each database.
#'
#' @inheritParams query_master
#'
#' @family special queries
#' 
#' @export


# Shortcut to add phase names to a result 
# (note: in H5PLEXOS we start with the phase names, not the IDs, so this is just for compatibility)
add_phase_ids <- function(x) {
  phases <- c("LT", "PASA", "MT", "ST")
  phases.df <- data.frame(phase = factor(phases, levels = phases), phase_id = 1:4)
  x %>% left_join(phases.df, by = "phase")
}

# And the inverse function, from original rplexos:
add_phase_names <- function(x) {
  phases <- c("LT", "PASA", "MT", "ST")
  phases.df <- data.frame(phase_id = 1:4, phase = factor(phases, levels = phases))
  x %>% left_join(phases.df, by = "phase_id")
}
