
library(data.table)
library(jsonlite)

get_covariate_estimates <- function(covariate_id=NULL, age_group_id='-1', location_id='-1', year_id='-1', sex_id='-1', gbd_round_id=NULL, status='best', decomp_step=NULL, release_id=NULL, model_version_id=NULL, location_set_version_id=0, location_set_id=22) {


  null_args <- Filter(is.null,c(as.list(environment())))
  num_null <- length(null_args)
  lhs <- paste(names(null_args), sep="")
  rhs <- paste(rep("'None'", num_null), sep="")
  eq <- paste(paste(lhs, rhs, sep="<-"), collapse=";")
  eval(parse(text=eq))


  callable <- "db_queries get_covariate_estimates R"
  kws <- "--keywords covariate_id age_group_id location_id year_id sex_id gbd_round_id status decomp_step release_id model_version_id location_set_version_id location_set_id"
  types <- "--types int intlist intlist intlist intlist int str str int int int int"
  values <- '--values %s "%s" "%s" "%s" "%s" %s "%s" "%s" %s %s %s %s'
  r_type <- "--return_type dataframe"
  args <- paste(callable, kws, types, values, r_type)


  if(.Platform$OS.type == "unix") {
    py_env <- "<<<< FILEPATH REDACTED >>>>"
    unwrap <- ""
  } else {
    py_env <- "<<<< FILEPATH REDACTED >>>>"
    unwrap <- "<<<< FILEPATH REDACTED >>>>"
  }


  cmd <- c(py_env, unwrap, sprintf(args, paste(covariate_id, collapse='---'), paste(age_group_id, collapse='---'), paste(location_id, collapse='---'), paste(year_id, collapse='---'), paste(sex_id, collapse='---'), paste(gbd_round_id, collapse='---'), paste(status, collapse='---'), paste(decomp_step, collapse='---'), paste(release_id, collapse='---'), paste(model_version_id, collapse='---'), paste(location_set_version_id, collapse='---'), paste(location_set_id, collapse='---')))

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

  if (length(df) == 0) stop("Error occured trying to use get_covariate_estimates.")
  return(df)
}
