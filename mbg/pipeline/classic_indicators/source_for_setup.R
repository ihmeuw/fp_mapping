message("Beginning of source_for_setup.R")
# Set up MKLROOT directory (needed if using RStudio)
Sys.setenv(MKLROOT = "<<<< FILEPATH REDACTED >>>>")
# get slightly better debug messages
options(error = NULL)

# load lbd.loader tool
library(lbd.loader, lib.loc = "<<<< FILEPATH REDACTED >>>>")

# load newest version of lbd.mbg
suppressMessages(
  library("lbd.mbg", lib.loc = lbd.loader::pkg_loc("lbd.mbg"))
)
