
library(data.table)
library(jsonlite)

get_outputs <- function(topic, drop_restrictions=FALSE, measure_id='1', version='best', location_id='1', year_id=NULL, year_start_id=NULL, year_end_id=NULL, age_group_id='22', sex_id='3', cause_id='294', rei_id='169', sequela_id='1', expected=FALSE, metric_id='1', gbd_round_id=NULL, decomp_step=NULL, rei_set_id=1, cause_set_id=3, location_set_id=35, compare_version_id=NULL, process_version_id=NULL, release_id=NULL) {


  null_args <- Filter(is.null,c(as.list(environment())))
  num_null <- length(null_args)
  lhs <- paste(names(null_args), sep="")
  rhs <- paste(rep("'None'", num_null), sep="")
  eq <- paste(paste(lhs, rhs, sep="<-"), collapse=";")
  eval(parse(text=eq))


  callable <- "db_queries get_outputs R"
  kws <- "--keywords topic drop_restrictions measure_id version location_id year_id year_start_id year_end_id age_group_id sex_id cause_id rei_id sequela_id expected metric_id gbd_round_id decomp_step rei_set_id cause_set_id location_set_id compare_version_id process_version_id release_id"
  types <- "--types str bool intlist str intlist intlist intlist intlist intlist intlist intlist intlist intlist bool intlist int str int int int int intlist int"
  values <- '--values "%s" %s "%s" "%s" "%s" "%s" "%s" "%s" "%s" "%s" "%s" "%s" "%s" %s "%s" %s "%s" %s %s %s %s "%s" %s'
  r_type <- "--return_type dataframe"
  args <- paste(callable, kws, types, values, r_type)


  if(.Platform$OS.type == "unix") {
    py_env <- "<<<< FILEPATH REDACTED >>>>"
    unwrap <- ""
  } else {
    py_env <- "<<<< FILEPATH REDACTED >>>>"
    unwrap <- "<<<< FILEPATH REDACTED >>>>"
  }


  cmd <- c(py_env, unwrap, sprintf(args, paste(topic, collapse='---'), paste(drop_restrictions, collapse='---'), paste(measure_id, collapse='---'), paste(version, collapse='---'), paste(location_id, collapse='---'), paste(year_id, collapse='---'), paste(year_start_id, collapse='---'), paste(year_end_id, collapse='---'), paste(age_group_id, collapse='---'), paste(sex_id, collapse='---'), paste(cause_id, collapse='---'), paste(rei_id, collapse='---'), paste(sequela_id, collapse='---'), paste(expected, collapse='---'), paste(metric_id, collapse='---'), paste(gbd_round_id, collapse='---'), paste(decomp_step, collapse='---'), paste(rei_set_id, collapse='---'), paste(cause_set_id, collapse='---'), paste(location_set_id, collapse='---'), paste(compare_version_id, collapse='---'), paste(process_version_id, collapse='---'), paste(release_id, collapse='---')))

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
      message(paste(file_name, collapse = "
"))
      df <- data.table(NULL)
    }
  }

  if (length(df) == 0) stop("Error occured trying to use get_outputs.")
  return(df)
}
