#' @title MBG Pipeline Class
#' @docType class
#' @keywords pipeline MBG
#' @importFrom R6 R6Class
#' @export
#' @return Object of \code{\link{R6Class}} with the pipeline.
#' @format \code{\link{R6Class}} object.
#'
#' @concept pipeline
MBGPipeline <- R6::R6Class("MBGPipeline",
  # lock_objects = FALSE is necessary to add additional attributes to instances
  public = list(
    #' @field indicator_group Category of indicator
    indicator_group = NULL,
    #' @field indicator Specific outcome to be modeled within indicator category
    indicator = NULL,
    #' @field folder where configs live. If [config_name] is used, configs must be in the pattern
    #' [config_repo]/[indicator_group]/config_[indicator].csv. Conversly, [config_file] can be
    #' used to provide a full filepath to the config file.
    config_repo = NULL,
    #' @field config_name Name of configuration file in the indicator folder, Default: NULL
    config_name = NULL,
    #' @field config_file Full path to configuration file that overrides \code{config_name}, Default: NULL
    config_file = NULL,
    #' @field covs_name Name of covariates configuration file, Default: NULL
    covs_name = NULL,
    #' @field covs_file Full path to covariates configuration file that overrides \code{covs_name}, Default: NULL
    covs_file = NULL,
    #' @field user the user running the script. Defaults to Sys.info()["user"]
    user = NULL,
    #' @field run_date Run date, Default: NULL (will be created based off of current timestamp)
    run_date = NULL,
    #' @field sharedir the location of model run outputs for the given indicator_group/indicator
    sharedir = NULL,
    #' @field config loaded configuration, populated in setup_conf
    config = NULL,
    #' @field fixed_effects_config the configuration for which fixed effects covariates to load
    fixed_effects_config = NULL,
    #' @field gadm_list the admin 0 codes for the regions in config (`config_list$Regions`)
    gadm_list = NULL,
    #' @field config_list list with all configuration options present
    config_list = NULL,
    #' @field outputdir the output directory data will be saved to. Populated in setup_rundate
    outputdir = NULL,
    #' @field df input data. Populated in make_holdouts
    df = NULL,
    #' @field stratum_ho output of make_folds call
    stratum_ho = NULL,
    #' @field loopvars the region/age/holdout matrix we will run models for. Populated in create_loopvars
    loopvars = NULL,
    #' @field fp_list key value list of filepath name -> absolute path
    fp_list = NULL,

    # __init__
    #' @param indicator_group character Category of indicator
    #' @param indicator character Specific outcome to be modeled within indicator category
    #' @param config_repo folder where configs live. Used in conjunction with config_name and covs_name
    #' @param config_name character Name of configuration file in the indicator folder, Default: NULL
    #' @param config_file character Full path to configuration file that overrides \code{config_name}, Default: NULL
    #' @param covs_name character Name of covariates configuration file, Default: NULL
    #' @param covs_file character Full path to covariates configuration file that overrides \code{covs_name}, Default: NULL
    #'
    #' @param run_date character Run date, Default: NULL (will be created based off of current timestamp)
    #' @param print logical whether to print a config summary after creation. Default: TRUE
    initialize = function(indicator_group,
                          indicator,
                          config_repo = NULL,
                          config_name = NULL,
                          config_file = NULL,
                          covs_name = NULL,
                          covs_file = NULL,
                          run_date = NULL,
                          print = TRUE) {
      stopifnot(!is.null(indicator_group))
      stopifnot(!is.null(indicator))

      # Stop if both _name and _file for configs are NULL
      stopifnot(!is.null(config_name) | !is.null(config_file))
      stopifnot(!is.null(covs_name) | !is.null(covs_file))

      # If config/covs_name is used, config_repo must be given as well
      if (!is.null(config_name) | !is.null(covs_name)) {
        stopifnot(!is.null(config_repo))
      }

      # Add to self
      self$indicator_group <- indicator_group
      self$indicator <- indicator

      self$config_repo <- config_repo
      self$config_name <- config_name
      self$config_file <- config_file
      self$covs_name <- covs_name
      self$covs_file <- covs_file
      self$user <- Sys.info()["user"]
      self$run_date <- run_date

      if (print) {
        self$print()
      }
    },

    #' @description print the class name and relevant arguments
    print = function() {
      cat("MBG Pipeline: \n")
      cat("  Indicator Group  :  ", self$indicator_group, "\n", sep = "")
      cat("  Indicator        :  ", self$indicator, "\n", sep = "")
      if (!is.null(self$config_repo)) {
        cat("  Config Path        :  ", self$config_repo, "\n", sep = "")
      }
      if (is.null(self$config_name)) {
        cat("  Model Path       :  ", self$config_file, "\n", sep = "")
      } else {
        cat("  Model Name       :  ", self$config_name, "\n", sep = "")
      }

      if (is.null(self$covs_name)) {
        cat("  Covs Path        :  ", self$covs_file, "\n", sep = "")
      } else {
        cat("  Covs Config      :  ", self$covs_name, "\n", sep = "")
      }
      data("lbd_core.hash", package = utils::packageName(), envir = environment())
      cat("  lbd_core hash    :  ", lbd_core.hash, "\n", sep = "")
    },



    #' @description helper function: parses values from a typical config data.table
    #' @param conf_key character key name
    #' @param conf_dt configuration data.table to parse. Default: self$config
    parse_conf = function(conf_key, conf_dt = self$config) {
      return(conf_dt[V1 == paste0(conf_key), V2])
    },

    #' @description helper function to eval and parse a config key
    #' @param conf_key character key name
    #' @param conf_dt configuration data.table to parse. Default: self$config
    eval_conf = function(conf_key, conf_dt = self$config) {
      return(eval(parse(text = self$parse_conf(conf_key, conf_dt))))
    },

    #' @description set up configuration by parsing configuration
    #' @details calls \code{\link{set_up_config}} internally
    #' @param push_to_global_env logical whether to push config values to global environment. Default: FALSE
    #' @param run_tests logical whether to run tests on configuration. Default: TRUE
    setup_conf = function(push_to_global_env = FALSE, run_tests = TRUE) {

      # populate filepath list
      # this should be loaded in on package load (zzz.R) into the global env
      if(exists("fp_list", envir = .GlobalEnv)) {
        self$fp_list <- fp_list
      } else {
        self$fp_list <- load_filepath_config()
        assign("fp_list", self$fp_list, envir = .GlobalEnv)
      }

      # Set up config first
      print("Config Setup: [1/3] Getting config info and running checks")
      config_both <- set_up_config(
        indicator_group = self$indicator_group,
        indicator = self$indicator,
        config_repo = self$config_repo,
        config_name = self$config_name,
        config_file = self$config_file,
        covs_name = self$covs_name,
        covs_file = self$covs_file,
        push_to_global_env = push_to_global_env,
        run_tests = run_tests,
        return_list = TRUE
      )

      # Separate out regular and FE configs
      self$config <- config_both[["config"]]
      self$fixed_effects_config <- config_both[["fixed_effects_config"]]

      # Create a few objects from the config file loaded above
      print(paste0(
        "Config Setup: [2/3] Checks for values of Regions,",
        " year_list and summstats"
      ))

      # Push the above fixes to global env if supplied
      if (push_to_global_env) {
        if (class(self$eval_conf("Regions")) == "character" &
          length(self$eval_conf("Regions")) == 1) {
          assign("Regions", parse_conf("Regions"), envir = .GlobalEnv)
        }

        if (class(self$eval_conf("year_list")) == "character") {
          assign("year_list", parse_conf("year_list"), envir = .GlobalEnv)
        }

        if (length(self$eval_conf("summstats")) == 1 &
          grepl(",", self$parse_conf("summstats"))) {
          assign("summstats", parse_conf("summstats"), envir = .GlobalEnv)
        }
      }


      # Load GADM list
      print("Config Setup: [3/3] Loading list of GADM codes")
      self$gadm_list <- get_adm0_codes(self$eval_conf("Regions"),
        shapefile_version = self$parse_conf("modeling_shapefile_version")
      )

      # Finally, let's create a named list out of the config into 'config_list'
      self$config_list <- as.list(self$config[, V2])
      names(self$config_list) <- self$config[, V1]

      # SPECIAL: Parse year_list and z_list because it's used as the vector and
      # we want to make sure there's no ad-hoc conflict in types
      self$config_list$year_list <- eval(
        parse(
          text = self$config_list$year_list
        )
      )
      self$config_list$z_list <- eval(
        parse(
          text = self$config_list$z_list
        )
      )
      self$config_list$seed <- eval(
        parse(
          text = self$config_list$seed
        )
      )
      # support "NULL" in config to suppress using PARDISO solver
      if (identical(self$config_list$pardiso_license, "NULL")) {
        self$config_list$pardiso_license <- NULL
      }
    },

    #' @description configures run date and related output directories for this pipeline
    #' @param run_date character name of output. If NULL, uses self$run_date.
    #' If that is NULL, creates one off of the current timestamp. Default: NULL
    #' @param full_cleanup logical whether to delete the contents of the output directory (if any exist). Default FALSE
    setup_rundate = function(run_date = NULL,
                             full_cleanup = FALSE) {
      if (is.null(self$run_date) & is.null(run_date)) {
        print("Setting up run_date...")
        self$run_date <- paste0(make_time_stamp(TRUE))
        print(self$run_date)
      } else if (!is.null(self$run_date) & is.null(run_date)) {
        run_date <- self$run_date
        print(paste0("run_date supplied as value in initializing pipeline: ", self$run_date))
      } else if (is.null(self$run_date) & !is.null(run_date)) {
        self$run_date <- run_date
        print(paste0("run_date supplied as value in this function: ", run_date))
      } else {
        warning(paste0(
          "Run-date supplied from both initializing pipeline",
          " and from this function. Using only the newly",
          " supplied run_date: ", run_date
        ))
        self$run_date <- run_date
      }

      self$sharedir <- paste(self$fp_list$mbg_root,
                             self$indicator_group, self$indicator,
                             sep = "/"
      )

      # Set up outputdir as sge logs folders based off of run_date
      self$outputdir <- file.path(
        self$fp_list$mbg_root,
        self$indicator_group,
        self$indicator,
        "output",
        self$run_date
      )

      if (full_cleanup) {
        unlink(self$outputdir, recursive = TRUE)
      }

      # Create the output and error log folders
      dir.create(self$outputdir, showWarnings = FALSE, recursive = TRUE)
    },

    #' @description Make holdouts
    #' @param withdate logical delegated to \code{\link{load_input_data}}
    #' @param date character delegated to \code{\link{load_input_data}}
    make_holdouts = function(withdate = FALSE,
                             date = self$run_date) {
      # NOTE: make_holdouts() will not run if set to false in config
      if (self$eval_conf("makeholdouts")) {
        with_globals(new = list(run_date = date), {
          # Load the full input data
          self$df <- load_input_data(
            indicator = self$indicator,
            indicator_group = self$indicator_group,
            removeyemen = TRUE,
            years = self$parse_conf("yearload"),
            yl = self$eval_conf("year_list"),
            withtag = self$eval_conf("withtag"),
            datatag = self$parse_conf("datatag"),
            use_share = self$parse_conf("use_share"),
            withdate = withdate,
            date = self$run_date,
            poly_ag = self$eval_conf("poly_ag"),
            zcol_ag = self$eval_conf("zcol_ag")
          )
        })

        # Add in location information
        self$df <- merge_with_ihme_loc(self$df,
          re = self$eval_conf("Regions"),
          shapefile_version = self$parse_conf("modeling_shapefile_version")
        )

        # temporary bugfix for "NULL" causing problems in make_folds
        spat_strat <- if (identical(self$parse_conf("spat_strat"), "NULL")) NULL else self$parse_conf("spat_strat")
        temp_strat <- if (identical(self$parse_conf("temp_strat"), "NULL")) NULL else self$parse_conf("temp_strat")
        spte_strat <- if (identical(self$parse_conf("spte_strat"), "NULL")) NULL else self$parse_conf("spte_strat")
        # Make a list of dfs for each region,
        # with 5 qt folds identified in each
        self$stratum_ho <- make_folds(
          data = self$df,
          n_folds = self$eval_conf("n_ho_folds"),
          save.file = paste0(
            self$fp_list$mbg_root,
            self$indicator_group, "/",
            self$indicator, "/output/",
            self$run_date, "/stratum.rds"
          ),
          spat_strat = spat_strat,
          temp_strat = temp_strat,
          spte_strat = spte_strat,
          indicator = self$indicator,
          indicator_group = self$indicator_group,
          run_date = self$run_date,
          strat_cols = "region",
          ts = self$eval_conf("ho_ts"),
          yr_col = self$parse_conf("yr_col"),
          ss_col = self$parse_conf("ss_col"),
          lat_col = self$parse_conf("lat_col"),
          long_col = self$parse_conf("long_col")
        )
      } else {
        print("makeholdouts in config is set to FALSE. No holdouts created")
      }
    },

    #' @description Create \code{loopvars} matrix for easy looping on model runs
    create_loopvars = function() {
      # Make loopvars aka strata grid (format = regions, ages, holdouts)
      if (self$eval_conf("makeholdouts")) {
        self$loopvars <- expand.grid(
          self$eval_conf("Regions"),
          0, 0:self$parse_conf("n_ho_folds")
        )
      } else {
        self$loopvars <- expand.grid(self$eval_conf("Regions"), 0, 0)
      }

      # Clean up names
      self$loopvars <- data.table(self$loopvars)
      colnames(self$loopvars) <- c("region", "age", "holdout")
    },

    #' @description Utility method to return a character "path addin" for naming
    #' @param loopvar_index integer index of \code{self$loopvars} to reference.
    #' @details returns a character like "_bin0_ken_0"
    get_path_addin = function(loopvar_index) {
      sprintf(
        "_bin%s_%s_%s",
        self$loopvars[loopvar_index, age],
        self$loopvars[loopvar_index, region],
        self$loopvars[loopvar_index, holdout]
      )
    },

    #' @description Utility method to help generate job names
    #' @param base_name base name for the job e.g., "prep_data"
    #' @param loopvar_index integer index of \code{self$loopvars} to reference.
    #' @details reutrns a character like "prep_data_bin0_ken_0"
    generate_job_name = function(base_name, loopvar_index) {
      sprintf("%s%s", base_name, self$get_path_addin(loopvar_index))
    }
  ) ## End of class ##
)
