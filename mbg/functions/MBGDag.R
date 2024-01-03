#' @title MBG DAG Class for describing job dependencies.
#'
#' @description MBG DAG helps users create nodes (SGE jobs) and manage the job dependency graph.
#'
#' @section DAG Table:
#' The DAG table will consist of the following information about the
#' nodes (the jobs):
#' \itemize{
#' \item{\code{nodename}}: Name of node in DAG
#'
#' \item{\code{node.id}: Auto-incrementing counter specifying the
#' node number in the graph
#' }
#'
#' \item{\code{jobscript}: Path to the R script being run
#' }
#'
#' \item{\code{job_name}: Name of job
#' }
#'
#' \item{\code{hold_on_job}: list of job(s) this job has a hold on
#' }
#'
#' \item{\code{project}: Cluster project
#' }
#'
#' \item{\code{queue}: Queue
#' }
#'
#' \item{\code{shell_script}: shell script invoked to start the job
#' }
#'
#' \item{\code{pipeline}: MBGPipeline instance associated with this job
#' }
#'
#' \item{\code{loopvar_index}: index value in pipeline$loopvars associated with this job. May be NA
#' }
#'
#' \item{\code{error_log_dir}: Directory or file that stderr will be saved to
#' }
#'
#' \item{\code{output_log_dir}: Directory or file that stdout will be saved to
#' }
#'
#' \item{\code{cores}: Number of CPU cores to request for job
#' }
#'
#' \item{\code{ram_gb}: Amount of RAM, in GB, to request for job
#' }
#'
#' \item{\code{runtime}: Maximum runtime allowed for job to execute in
#' }
#'
#' \item{\code{singularity_version}: version of Singularity image to use
#' }
#'
#' \item{\code{singularity_opts}: Options to provide to singularity
#' }
#'
#' \item{\code{qsub_str}: The qsub string
#' }
#'
#' \item{\code{status}: Job status
#' }
#'
#' \item{\code{job_id}: Job ID (after job has been submitted)
#' }
#'
#' }
#'
MBGDag <- R6::R6Class( # nolint
  classname = "MBGDag",

  # lock_objects = FALSE is necessary for being able to add more
  # to self after initializing
  lock_objects = FALSE,
  public = list(
    #' @field save_file path the DAG should be saved to. Computed from save_dir arg.
    save_file = NULL,

    #' @field dag_hash A string used to hash the nodes. Randomly generated if not supplied.
    dag_hash = NULL,

    #' @field user A string, the username. Slurm's squeue requires a user otherwise it
    #' will print all jobs. This is populated on node creation - it is easier to have it
    #' available here than to dig through the dag and pipeline to get to it.
    user = NULL,

    #' @description
    #' initialize new MBGDag instance. This is called automatically with `MBGDag$new`
    #' @param save_dir directory to save the MBGDag instance to when calling `MBGDag$save()`
    #' @param dag_hash character identifier. A 5 character random string will be generated if NULL. Default NULL.
    initialize = function(save_dir,
                          dag_hash = NULL) {
      self$id_counter <- 0
      self$DAG <- private$empty_dag()

      if (is.null(dag_hash)) {
        self$dag_hash <- stringi::stri_rand_strings(1, 5)
      } else {
        self$dag_hash <- dag_hash
      }
      self$save_file <- "<<<< FILEPATH REDACTED >>>>"
    },

    #' @description
    #' Return string suitable for parsing with argparse
    #' @param val value to place on command line
    #' @param flag flag to prefix arg with
    #' @return A string of the form "--FLAG VAL"
    argparse_append = function(val, flag) {
      sprintf("--%s %s", flag, val)
    },

    #' @description
    #' Empties the DAG object
    clean_dag = function() {
      self$DAG <- private$empty_dag()
    },

    #' @description
    #' Create a node in the DAG
    #' @param pipeline MBGPipeline instance providing context to this job
    #' @param base_name character base name of the job
    #' @param jobscript character path to the R script to be run
    #' @param loopvar_index numeric loopvar index in the associated MBGPipeline. Defaults NA
    #' @param hold_job vector of character job names or integer job numbers to hold on. Default NULL (no holds)
    #' @param project character project name to run the job under.
    #' @param cores integer number of cores to run the job with.
    #' @param ram_gb numeric number of Gigabytes of RAM to allocate for the job.
    #' @param runtime character maximum runtime for job to execute in.
    #' @param log_location character.
    #' @param combine_logs boolean. Should the output and error logs for this script be combined?
    #' @param queue character queue to run in.
    #' @param singularity_version character path to singularity image to run R with.
    #' @param singularity_opts list of environment variables to pass to singularity.
    #' @param shell_script character path to singularity invoking shell script.
    #' @return numeric id value for the node
    create_node = function( # things we must have: base name of the job within the DAG, loopvar_index, and the job script
                           pipeline,
                           base_name,
                           jobscript,
                           # defaults that will probably be modified
                           loopvar_index = NA_integer_,
                           hold_job = NULL,
                           project = "ihme_general",
                           cores = 1,
                           ram_gb = 5,
                           runtime = "01:00:00",
                           # defaults that are probably OK
                           log_location = "sharedir",
                           combine_logs = T,
                           queue = "long.q",
                           machine_set = 0,
                           singularity_version = "default",
                           singularity_opts = list(
                             SET_OMP_THREADS = cores,
                             SET_MKL_THREADS = cores,
                             LBDLOADER_lbd_mbg_VERSION = Sys.getenv("LBDLOADER_lbd_mbg_VERSION")
                           ),
                           shell_script = "default") { # nolint

      # increment counter
      self$id_counter <- self$id_counter + 1
      node_id <- self$id_counter
      self$user <- pipeline$user

      if(shell_script == "default") {
          shell_script <- "<<<< FILEPATH REDACTED >>>>"
      }

      # nodes associated with a loopvar_index include naming components to make it obvious
      if (is.na(loopvar_index)) {
        nodename <- base_name
      } else {
        nodename <- pipeline$generate_job_name(base_name, loopvar_index = loopvar_index)
      }
      # uniqueness of job names is very important to this working predictably.
      if (nodename %in% self$DAG$nodename) {
        msg <- sprintf("nodename %s already exists - provide a different base_name than '%s'", nodename, base_name)
        if (is.na(loopvar_index)) {
          msg <- sprintf("%s OR be sure to add the loopvar_index arg", msg)
        } else {
          msg <- sprintf("%s with loopvar_index %s", msg, loopvar_index)
        }
        stop(msg)
      }
      # Job name
      job_name <- sprintf("%s-%s", nodename, self$dag_hash)

      # Set up error and output locations
      output_err <- setup_log_location(
        log_location = log_location,
        user = pipeline$user,
        indicator = pipeline$indicator,
        indicator_group = pipeline$indicator_group,
        run_date = pipeline$run_date,
        combine_logs = combine_logs
      )
      output_log_dir <- output_err[[1]]
      error_log_dir <- output_err[[2]]

      ## Job holder
      if (is.null(hold_job)) {
        hold_on_job <- NA
      } else {
        if (is.numeric(hold_job)) {
          hold_on_job <- self$get_dag_value(node_id = hold_job, field = "job_name")
        } else if (is.character(hold_job)) {
          if (length(hold_job) == 1 && hold_job == "previous") {
            hold_on_job <- self$get_dag_value(node_id = max(self$DAG$node.id), field = "job_name")
          } else {
            hold_on_job <- self$get_dag_value(job.name = hold_job, field = "job_name")
          }
        } else {
          stop(sprintf("Not sure how to hold on job '%s'", hold_job))
        }
        if (length(hold_on_job) == 0) {
          stop(sprintf("Hold lookup failed for '%s'", paste(hold_job, collapse = ";")))
        }
      }

      self$DAG <- rbindlist(list(
        self$DAG,
        data.table(
          "nodename" = nodename,
          "node.id" = node_id,
          "jobscript" = jobscript,
          "job_name" = job_name,
          "hold_on_job" = I(list(hold_on_job)),
          "project" = project,
          "queue" = queue,
          "shell_script" = shell_script,
          "loopvar_index" = loopvar_index,
          "pipeline" = I(list(pipeline)),
          "error_log_dir" = error_log_dir,
          "output_log_dir" = output_log_dir,
          "project" = project,
          "cores" = cores,
          "ram_gb" = ram_gb,
          "runtime" = runtime,
          "machine_set" = machine_set,
          "singularity_version" = singularity_version,
          "singularity_opts" = I(list(singularity_opts)),
          "qsub_str" = NA_character_,
          "status" = "unsubmitted"
        )
      ), use.names = TRUE, fill = TRUE)

      qsub_str <- private$generate_qsub_str(node_id)
      self$update_node(node_id = node_id, field = "qsub_str", value = qsub_str)

      return(node_id)
      # End of create_node() #
    },

    #' @description
    #' Get field from the DAG given the nodename. Provide either node_id or job.name to identify node.
    #' @param node_id numeric id of node to get value for
    #' @param job.name character matching a node's \code{job_name}
    #' @param field character name of field in DAG to return.
    get_dag_value = function(node_id = NULL, job.name = NULL, field) {
      if (xor(is.null(job.name), is.null(node_id))) {
        if (is.null(job.name)) {
          return(self$DAG[node.id %in% node_id, get(field)])
        } else {
          return(self$DAG[job_name %in% job.name, get(field)])
        }
      }
      stop("get_dag_value requires either job.name or node_id to be provided.")
    },

    #' @description get ancestors of node
    #' @details
    #'  Get parent node id and each successive parent id to beginning of DAG. This
    #'  is helpful if you want to examine each node that this node transitively
    #'  depended on.
    #' @param node_id numeric node id
    #' @return
    #' vector of ancestors node_id beginning with direct parent \emph{OR}
    #' NULL if there are no parents or the id value is invalid \emph{OR}
    #' NA if this node or any parent has multiple parents
    get_simple_ancestor_ids = function(node_id) {
      result <- vector("numeric")

      parents <- self$DAG[node.id == node_id, hold_on_job]
      # is `id` valid?
      if (length(parents) == 0 || is.na(parents)) {
        return(NULL)
      }
      while (!is.na(parents)) {
        if (length(parents[[1]]) > 1) {
          return(NA_integer_)
        }
        parent.id <- self$DAG[job_name == parents[1], node.id]
        result <- c(result, parent.id)
        parents <- self$DAG[node.id == parent.id, hold_on_job]
      }
      return(result)
    },

    #' @description get direct node descendents (children)
    #' @param node_id numeric node id
    #' @return
    #' vector of child node ids
    #' NULL if the id value is invalid
    #' NA if there are no children
    get_descendant_ids = function(node_id) {
      to.visit <- self$DAG[node.id == node_id, job_name]

      if (length(to.visit) == 0) {
        return(NULL) # node_id not in DAG
      }

      visited <- character()

      repeat {
        # pop next node to visit
        cur.name <- to.visit[[1]]
        to.visit <- tail(to.visit, -1)

        # mark this as visited
        visited <- c(visited, cur.name)

        # determine children
        mask <- sapply(self$DAG$hold_on_job, FUN = function(holds) {
          cur.name %in% holds
        })
        children <- self$DAG[mask, job_name]

        # add any unvisited children to list of nodes to visit
        to.visit <- c(to.visit, setdiff(children, visited))

        if (length(to.visit) == 0) {
          break
        }
      }
      # first visited value is our original node; omit
      descendant.names <- tail(visited, -1)

      if (length(descendant.names) == 0) {
        NA
      } else {
        self$DAG[job_name %in% descendant.names, node.id]
      }
    },

    #' @description get direct node ancestors (parents)
    #' @param node_id numeric node id
    #' @return
    #' vector of parent node ids
    #' NULL if the id value is invalid
    #' NA if there are no parents
    get_direct_ancestor_ids = function(node_id) {
      parents.asis <- self$DAG[node.id == node_id, hold_on_job]
      if (length(parents.asis) == 0) {
        return(NULL) # invalid arg: id not in DAG
      } else {
        parent.job.names <- unlist(parents.asis)
        if (length(parent.job.names) == 1 && is.na(parent.job.names)) {
          return(parent.job.names)
        } else {
          return(self$DAG[job_name %in% parent.job.names, node.id])
        }
      }
    },

    #' @description Mark a node as having started
    #' @param node_id integer id value of node
    mark_node_started = function(node_id) {
      pipeline <- self$DAG[node.id == node_id, pipeline][[1]]
      file.create("<<<< FILEPATH REDACTED >>>>")
    },

    #' @description Mark a node as having finished
    #' @param node_id integer id value of node
    mark_node_finished = function(node_id) {
      file.create(private$node_finished_file(node_id))
    },

    #' @description Update any entries in the DAG dt. Provide job.name OR node_id to identify node
    #' @param job.name character name of node
    #' @param node_id integer node id
    #' @param field character field from DAG to update
    #' @param value appropriate value to update DAG with
    update_node = function(job.name = NULL,
                           node_id = NULL,
                           field,
                           value) {
      if (xor(is.null(job.name), is.null(node_id))) {
        if (is.null(node_id)) {
          node_ids <- self$DAG[job_name %in% job.name, node.id]
        } else {
          node_ids <- node_id
        }
      } else {
        stop("Provide a valid job.name or node_id")
      }

      # update field as value (paren cause field to be evaluated as variable instead of as "field")
      self$DAG[node.id %in% node_ids, (field) := value]
      # update qsub_str for each effected node
      for (node_id in node_ids) {
        self$DAG[node.id == node_id, "qsub_str"] <- private$generate_qsub_str(node_id)
      }
    },

    #' @description Save MBGDag as RData file
    save = function() {
      path <- self$save_file
      dag <- self # save uses the variable name; save as "dag"
      if (file.exists(path)) {
        # save as a tempoary file and overwrite the existing one
        # this prevents a related job from attempting the load the file while it's being saved
        temp.path <- tempfile(pattern = basename(path), tmpdir = dirname(path), fileext = ".RData")
        save(dag, file = temp.path)
        file.rename(temp.path, path) # this should be atomic (preventing read issues from jobs)
      } else {
        save(dag, file = path)
      }
    },

    #' @description Submit all jobs of the DAG to the scheduler.
    #' @details
    #' Submit all jobs to scheduler. Update DAG with scheduler job id ("job_id") and job status ("submitted")
    #' @param node_ids the node.id values to submit. Submits all nodes if NULL. Default NULL.
    submit_jobs = function(node_ids = NULL) {
      if (is.null(node_ids)) {
        node_ids <- self$DAG$node.id
      }

      for (node_id in node_ids) {
        qsub <- self$get_dag_value(node_id = node_id, field = "qsub_str")

        # SLURM can only do job holds on id (rather than name) so
        # replace the job_name with the job_id in the submission command
        if(grepl("--dependency=afterany:", qsub)) {
          # Pulls a 1x2 matrix where the second column has the value matched to
          # (\\S+)\\s
          dependencies <- str_match(qsub, "--dependency=afterany:(\\S+)\\s")[1,2]
          # If multiple dependencies, split into a list for processing
          depend_split <- strsplit(dependencies, ",")[[1]]
          # for each dependency, look up the job id from the dag table
          ids <- c()
          for(job in depend_split) {
            hold_id <- self$get_dag_value(job.name = job, field = "job_id")
            if (is.na(hold_id)) {
              node_name <- self$get_dag_value(node_id = node_id, field = "job_name")
              stop("Failed to get the job id of job_name: ", job, " to hold on for job: ",
                   node_name, " (node_id: ", node_name, ")")
            }

            ids <- c(ids, hold_id)
          }

          # concatenate the ids back into a string and replace the job names with ids
          id_replacement <- paste0(ids, collapse = ",")
          qsub <- gsub(dependencies, id_replacement, qsub, fixed = TRUE)
        }


        #system returns e.g. "Submitted batch job 13373"
        job.id <- system(qsub, intern = TRUE)
        job.id <- as.numeric(gsub("Submitted batch job ", "", job.id))
        message(sprintf("Job %s submitted", job.id))
        self$update_node(node_id = node_id, field = "job_id", value = job.id)
        self$update_node(node_id = node_id, field = "status", value = "submitted")
      }
    },

    #' @description Resubmit all jobs with "errored" status and all child jobs of those jobs.
    resubmit_jobs = function() {

      to.resubmit <- integer()

      failed.node.ids <- self$DAG[status == "errored", node.id]
      for (failed.id in failed.node.ids) {
        to.resubmit <- c(to.resubmit, failed.id, self$get_descendant_ids(failed.id))
      }
      # fromLast: so that child jobs are always queued after all respective parent jobs
      to.resubmit <- base::unique(na.omit(to.resubmit), fromLast = TRUE)

      # Update qsub strings in case user e.g., modified RAM usage
      for (node_id in to.resubmit) {
        qsub_str <- private$generate_qsub_str(node_id)
        self$update_node(node_id = node_id, field = "qsub_str", value = qsub_str)
      }

      self$submit_jobs(node_ids = to.resubmit)
    },

    #' @description Print status of existing jobs
    #' @param update logical whether to update job status prior to printing. Default TRUE.
    #' @param mask DAG mask
    #' @param fields character vector of fields to print. Default `c("job_id", "nodename", "status")`
    print_job_status = function(update = TRUE, mask = NULL, fields = c("job_id", "nodename", "status")) {
      if (is.null(mask)) {
        mask <- seq_len(self$DAG)
      }
      if (update) {
        self$update_job_status()
      }
      cat(sprintf("Current time: %s\n", date()))
      print(self$DAG[mask, ..fields])
    },

    #' @description Update job "status" flag by checking file markers and SGE via `qstat`
    update_job_status = function() {
      valid.jobs <- c(na.omit(self$DAG$job_id))

      related.jobs <- function(command) {
        jobs <- private$squeue_jobs(command)
        valid <- intersect(valid.jobs, jobs) # only use subset of valid.jobs
        self$DAG[job_id %in% valid, node.id]
      }

      scheduled <- related.jobs("squeue")
      pending <- related.jobs("squeue -t PD") # -t PD show pending jobs
      running <- related.jobs("squeue -t R") # -t R show running jobs
      unknown <- setdiff(scheduled, c(pending, running))


      done <- setdiff(self$DAG$node.id, scheduled)

      # partition done jobs into finished and errored
      if (length(done) > 0) {
        finished.mask <- sapply(done, function(node_id) {
          file.exists(private$node_finished_file(node_id))
        })

        finished <- done[finished.mask]
        errored <- done[!finished.mask]
      } else {
        finished <- errored <- integer()
      }

      self$update_node(node_id = pending, field = "status", value = "queued")
      self$update_node(node_id = running, field = "status", value = "running")
      self$update_node(node_id = finished, field = "status", value = "finished")
      self$update_node(node_id = errored, field = "status", value = "errored")
      self$update_node(node_id = unknown, field = "status", value = "unknown")
    },

    #' @description Remove node from DAG. Does not cancel job or effect holds
    #' @param node_erase character to match job \code{nodename}
    remove_node = function(node_erase = "") {
      self$DAG <- self$DAG[!(nodename %in% node_erase)]
    },

    #' @description Pause execution until specified job(s) are complete
    #' @param node_id node_id numeric id(s) of node(s) to wait on. Default NULL waits on all nodes
    #' @param sleeptime integer seconds to sleep between updates. Default 100
    wait_on_node = function(node_id = NULL, sleeptime = 100) {
      if (is.null(node_id)) {
        nodes.to.wait.on <- self$DAG$node.id
      } else {
        nodes.to.wait.on <- node_id
      }

      mask <- self$DAG$node.id %in% nodes.to.wait.on

      completed.statuses <- c("finished", "errored")

      repeat {
        self$print_job_status(update = TRUE, mask = mask)

        if (all(self$DAG[mask, status] %in% completed.statuses)) {
          break
        } else {
          Sys.sleep(sleeptime)
        }
      }

      return(0)
    },

    #' @description Get node_id and job metadata according to desired params, as well
    #'   as initialized objectstore for loading in data from the jobs
    #' @param job_str a string to regex match against the job name
    #' @param region region string to subset table to
    #' @param indicator indicator to subset table to. Only needed if a DAG run has
    #'   multiple indicators included
    #' @param holdout holdout to subset to. Only needed if holdouts are turned on
    #' @param age age to subset to. Only needed if age is included as a loopvar in
    #'   the modeling process.
    #'
    #' @return a data.table with rows subset based on the given params, and an
    #'   ObjectStore object for fetching data from those jobs
    get_node_id_table = function(job_str = NULL, region = NULL, indicator = NULL,
                                 holdout = NULL, age = NULL) {
      dag_df <- self$DAG[, .(job_name, node.id, jobscript, pipeline, loopvar_index)]

      # renaming function args to not conflict with df cols
      reg <- region
      ind <- indicator
      hold <- holdout
      a <- age

      if (!is.null(job_str)) {
        dag_df <- dag_df[job_name %like% job_str, ]
        if (nrow(dag_df) == 0) {
          stop(paste0(
            "No job_names were found matching the provided job_str: ", job_str,
            ". Note that job string can be a partial match e.g. j01."
          ))
        }
      }

      # extract indicator, region, holdout, and age for each row from its pipeline object
      # these values have to be extracted from the pipeline in each row because a dag can
      # hold multiple different pipelines (e.g. if multiple indicators are included)
      dag_df[, `:=`(
        indicator = pipeline[[1]]$indicator,
        region = pipeline[[1]]$loopvars[loopvar_index]$region,
        holdout = pipeline[[1]]$loopvars[loopvar_index]$holdout,
        age = pipeline[[1]]$loopvars[loopvar_index]$age
      )]

      # subset table down based on provided region/indicator/holdout/age
      if (!is.null(region)) {
        dag_df <- dag_df[region == reg, ]
      }

      if (!is.null(indicator)) {
        dag_df <- dag_df[indicator == ind, ]
      }

      if (!is.null(holdout)) {
        dag_df <- dag_df[holdout == hold, ]
      }

      if (!is.null(age)) {
        dag_df <- dag_df[age == a, ]
      }

      if (nrow(dag_df) == 0) {
        stop(paste0(
          "No jobs were found matching the provided criteria: ",
          if (!is.null(job_str)) paste0("\n\t job_str: ", job_str),
          if (!is.null(indicator)) paste0("\n\t indicator: ", ind),
          if (!is.null(region)) paste0("\n\t region: ", reg),
          if (!is.null(holdout)) paste0("\n\t holdout: ", hold),
          if (!is.null(age)) paste0("\n\t age: ", a)
        ))
      }

      obj_store_list <- list()
      for (row in 1:nrow(dag_df)) {
        obj_store_list[[row]] <- ObjectStore$from_pipeline_and_node_id(dag_df[row, ]$pipeline[[1]], dag_df[row, ]$node.id)
      }
      dag_df[, obj_store := obj_store_list]

      cols_to_keep <- c("node.id", "job_name", "indicator", "region", "holdout", "age", "obj_store")
      dag_df <- dag_df[, ..cols_to_keep]

      return(dag_df)
    }
  ), # end public

  private = list(
    generate_qsub_str = function(node_id) {
      C <- self$DAG[node.id == node_id, ] # (C)ontext data
      P <- C$pipeline[[1]] # (P)ipeline
      ## Create qsub string

      # Set up resources
      resources <- get_resources(
        cores = C$cores,
        ram_gb = C$ram_gb,
        runtime = C$runtime,
      )

      # this is the CLI call. That the arguments are provided by name is incidental,
      # only the values are used in the resulting paste call
      script.cli <- c(
        shell = C$shell_script,
        code_path = C$jobscript,
        self$argparse_append(node_id, "node_id"),
        # if loopvar_index is null this expression returns NULL, which is removed from script.cli
        (if (!is.na(C$loopvar_index)) {
          c(
            self$argparse_append(C$loopvar_index, "loopvar_index")
          )
        }),
        self$argparse_append(P$run_date, "run_date"),
        self$argparse_append(P$indicator, "indicator"),
        self$argparse_append(P$indicator_group, "indicator_group"),
        self$argparse_append(C$nodename, "nodename"),
        self$argparse_append(self$save_file, "dag_path")
      )

      qsub.func.args <- as.list(script.cli)
      qsub.func.args$job_name <- C$job_name
      qsub.func.args$stderr_log <- C$error_log_dir
      qsub.func.args$stdout_log <- C$output_log_dir

      if (!is.na(C$hold_on_job)) {
        qsub.func.args$hold_jid <- paste(C$hold_on_job[[1]], collapse = ",")
      }
      qsub.func.args$project <- C$project
      qsub.func.args$resources <- resources
      qsub.func.args$queue <- C$queue
      qsub.func.args$machine_set <- C$machine_set
      qsub.func.args$singularity_str <- qsub_sing_envs("", C$singularity_opts[[1]], C$singularity_version)
      do.call(generate_sbatch_command, qsub.func.args)
    },
    node_finished_file = function(node_id) {
      pipeline <- self$DAG[node.id == node_id, pipeline][[1]]
      file.path("<<<< FILEPATH REDACTED >>>>")
    },
    qstat_jobs = function(qstat) {
      shell.cmd <- sprintf("%s | awk '{print $1}'", qstat)
      shell.out <- system(shell.cmd, intern = TRUE)
      # remove first 2 lines (report header) and convert job ids to numeric
      as.numeric(tail(shell.out, -2))
    },
    squeue_jobs = function(squeue) {
      # squeue needs to be done by user otherwise everyone's jobs print out
      shell.cmd <- sprintf("%s -u %s | awk '{print $1}'", squeue, self$user)
      shell.out <- system(shell.cmd, intern = TRUE)
      # remove first line ("JOB_ID") and convert job ids to numeric
      as.numeric(tail(shell.out, -1))
    },

    ## Empty DAG dt maker
    empty_dag = function() {
      return(
        data.table(
          "nodename" = character(),
          "node.id" = integer(),
          "jobscript" = character(),
          "job_name" = character(),
          "hold_on_job" = I(list()),
          "project" = character(),
          "queue" = character(),
          "shell_script" = character(),
          "pipeline" = I(list()), # AsIs; hold pointer to R6 object and don't try and unpack/interpret
          "loopvar_index" = integer(),
          "error_log_dir" = character(),
          "output_log_dir" = character(),
          "cores" = integer(),
          "ram_gb" = numeric(),
          "runtime" = character(),
          "singularity_version" = character(),
          "singularity_opts" = I(list()),
          "qsub_str" = character(),
          "status" = character(),
          "job_id" = integer()
        )
      )
    }
  )
)
