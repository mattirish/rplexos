% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{query_log}
\alias{query_log}
\alias{query_log_steps}
\title{Query log file information}
\usage{
query_log(db)

query_log_steps(db)
}
\arguments{
\item{db}{PLEXOS database object}
}
\description{
During the processing of the PLEXOS databases, information from the log file is saved
into the database. This includes solution times and infeasibilities for the different phases.
}
\examples{
# Process the folder with the solution file provided by rplexos
location <- location_solution_rplexos()
process_folder(location)

# Query data
db <- plexos_open(location)
query_log(db)
query_log_steps(db)

}
\seealso{
\code{\link{plexos_open}} to create the PLEXOS database object

Other special queries: \code{\link{query_class}},
  \code{\link{query_config}}, \code{\link{query_phase}},
  \code{\link{query_property}}, \code{\link{query_sql}},
  \code{\link{query_time}}
}
