
library(data.table)
library(jsonlite)

get_population <- function(age_group_id='22', single_year_age=FALSE, location_id='1', location_set_id=35, location_set_version_id=0, year_id=NULL, sex_id='3', gbd_round_id=NULL, run_id=NULL, with_ui=FALSE, decomp_step=NULL, release_id=NULL, use_rotation=TRUE, forecasted_pop=FALSE, status='best', population_group_id=NULL) {


  null_args <- Filter(is.null,c(as.list(environment())))
  num_null <- length(null_args)
  lhs <- paste(names(null_args), sep="")
  rhs <- paste(rep("'None'", num_null), sep="")
  eq <- paste(paste(lhs, rhs, sep="<-"), collapse=";")
  eval(parse(text=eq))


  callable <- "db_queries get_population R"
  kws <- "--keywords age_group_id single_year_age location_id location_set_id location_set_version_id year_id sex_id gbd_round_id run_id with_ui decomp_step release_id use_rotation forecasted_pop status population_group_id"
  types <- "--types intlist bool intlist int int intlist intlist int int bool str int bool bool str intlist"
  values <- '--values "%s" %s "%s" %s %s "%s" "%s" %s %s %s "%s" %s %s %s "%s" "%s"'
  r_type <- "--return_type dataframe"
  args <- paste(callable, kws, types, values, r_type)


  if(.Platform$OS.type == "unix") {
    py_env <- "<<<< FILEPATH REDACTED >>>>"
    unwrap <- ""
  } else {
    py_env <- "<<<< FILEPATH REDACTED >>>>"
    unwrap <- "<<<< FILEPATH REDACTED >>>>"
  }


  cmd <- c(py_env, unwrap, sprintf(args, paste(age_group_id, collapse='---'), paste(single_year_age, collapse='---'), paste(location_id, collapse='---'), paste(location_set_id, collapse='---'), paste(location_set_version_id, collapse='---'), paste(year_id, collapse='---'), paste(sex_id, collapse='---'), paste(gbd_round_id, collapse='---'), paste(run_id, collapse='---'), paste(with_ui, collapse='---'), paste(decomp_step, collapse='---'), paste(release_id, collapse='---'), paste(use_rotation, collapse='---'), paste(forecasted_pop, collapse='---'), paste(status, collapse='---'), paste(population_group_id, collapse='---')))

  if (.Platform$OS.type == "unix") {
    if (packageVersion("data.table") >= "1.11.6") {
      df <- fread(cmd=paste(cmd, collapse=' '))
    } else {
      df <- fread(paste(cmd, collapse=' '))
    }
  } else {
    file_name <- suppressWarnings(shell(paste(cmd, collapse=' '), intern = TRUE))
    if (file.exists(file_name[1])) {
      df <- fread(file_name)
      file.remove(file_name)
    } else {
      message(paste(file_name, collapse = ""))
      df <- data.table(NULL)
    }
  }

  if (length(df) == 0) stop("Error occured trying to use get_population.")
  return(df)
}
