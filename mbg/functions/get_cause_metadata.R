
library(data.table)
library(jsonlite)

get_cause_metadata <- function(cause_set_id, cause_set_version_id=NULL, gbd_round_id=NULL, decomp_step=NULL, release_id=NULL) {


  null_args <- Filter(is.null,c(as.list(environment())))
  num_null <- length(null_args)
  lhs <- paste(names(null_args), sep="")
  rhs <- paste(rep("'None'", num_null), sep="")
  eq <- paste(paste(lhs, rhs, sep="<-"), collapse=";")
  eval(parse(text=eq))


  callable <- "db_queries get_cause_metadata R"
  kws <- "--keywords cause_set_id cause_set_version_id gbd_round_id decomp_step release_id"
  types <- "--types int int int str int"
  values <- '--values %s %s %s "%s" %s'
  r_type <- "--return_type dataframe"
  args <- paste(callable, kws, types, values, r_type)


  if(.Platform$OS.type == "unix") {
    py_env <- "<<<< FILEPATH REDACTED >>>>"
    unwrap <- ""
  } else {
    py_env <- "<<<< FILEPATH REDACTED >>>>"
    unwrap <- "<<<< FILEPATH REDACTED >>>>"
  }


  cmd <- c(py_env, unwrap, sprintf(args, paste(cause_set_id, collapse='---'), paste(cause_set_version_id, collapse='---'), paste(gbd_round_id, collapse='---'), paste(decomp_step, collapse='---'), paste(release_id, collapse='---')))

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

  if (length(df) == 0) stop("Error occured trying to use get_cause_metadata.")
  return(df)
}
