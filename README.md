# rplexos for H5PLEXOS

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/NREL/rplexos?branch=master&svg=true)](https://ci.appveyor.com/project/NREL/rplexos)
[![Coverage status](https://codecov.io/gh/NREL/rplexos/branch/master/graph/badge.svg)](https://codecov.io/github/NREL/rplexos?branch=master)

rplexos is an R package developed to read and analyze PLEXOS solutions. This version currently only contains functions to query HDF5 databases created by H5PLEXOS. Future versions may enable creation and querying of SQLite databases as well.

## Getting started
This HDF5-compatible version of rplexos isn't yet configured for installation via `install_packages()`. Instead, you'll source its four scripts and use its functions wherever you like.

Start by cloning the h5-enabled version of rplexos:
```bash
git clone https://github.com/mattirish/rplexos
cd rplexos; git checkout h5plexos
```

Then in R, you'll need to install rhdf5 from bioconductor:
```R
source("https://bioconductor.org/biocLite.R")
biocLite("rhdf5")
```
Then, each time you load a new workspace, don't load your normal rplexos package if you have that installed already. Instead, you'll have to source the scripts used in the h5-enabled rplexos:
```
library(rhdf5)
library(dplyr)
library(tidyr)

rplexos_repo_path = <path to your rplexos repo>
source(file.path(rplexos_repo_path,"R","plexos_open.R"))
source(file.path(rplexos_repo_path,"R","query.R"))
source(file.path(rplexos_repo_path,"R","query_summary.R"))
source(file.path(rplexos_repo_path,"R","auxiliary.R"))
```

Then you should be good to call `plexos_open()` and any of the `query_...` functions offered by the regular SQLite database-compatible rplexos. If there are any funky errors when you try to make queries, create an issue or email Matt Irish at matthew.irish@nrel.gov.

## What this H5PLEXOS-based rplexos offers that the old SQLite-based rplexos doesn't:
- HDF5s created by H5PLEXOS can be accessed in R. Currently, RSQLite databases cannot be queried without switching to the original rplexos. Queries are slightly faster since dramatically less data are read in for each query.

## What this H5PLEXOS-based rplexos lacks that the old version has:
- The "class group" and "class" of each property (e.g. "Electric" and "Generator" for the collection "Generator" and property "Generation") aren't currently included in H5PLEXOS databases, although the "collection" is, so those two fromer parameters don't appear in the H5PLEXOS-based version of rplexos's `query_property()`.
- The inclusion of multiple bands, samples, and/or timeslices is not yet included: just one of each is assumed for each property. 
- The "parent" column is not included in h5plexos query results, since it's almost always "System" and parent information is not tracked in H5PLEXOS databases for all object types. Queries for special relations included in h5plexos may be included in a future release.

## Use notes
- This has only been tested using dplyr v1.0.0 and rlang v0.4.7. Any other versions of those packages may result in a sad, broken package.
- The format of collection names is different from vintage RSQLite-based rplexos due to the different naming scheme used by H5PLEXOS. Examples below:

	| vintage rplexos       | H5PLEXOS-based rplexos (this version) |
	------------------------|----------------------------------------
	| "Generator"		        | "generators"                          |
	| "Reserve.Generators"	| "reserves_generators"                 |
	
	In other words, to change your vintage rplexos queries to enable them for this h5 version, just make everything lowercase and plural and switch periods to underscores. An internal mapping to accomodate the old format may be added in the future.
- Any file with a ".h5" extension is assumed to be an h5plexos solution.
- Note that yearly queries will currently erroneously contain the reporting from one model per scenario if there are no monthly- or interval-reported data in the solutions (this seems unlikely, but could be an issue).
- None of the vignettes have been edited to refer to this new H5PLEXOS-based version yet.
