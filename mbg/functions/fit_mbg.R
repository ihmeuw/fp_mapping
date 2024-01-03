#' @title Fit MBG model in INLA
#' @description Fit MBG model in INLA
#' @param indicator_family indicator family
#' @param stack.obs INLA data stack, from \code{build_mbg_data_stack()}
#' @param spde NOT USED
#' @param cov NOT USED
#' @param N number of samples/observations for each data point
#' @param int_prior_mn prior for mean for intercept
#' @param int_prior_prec prior for precision for intercept, Default: 1
#' @param f_mbg INLA model formulae, from \code{build_mbg_formula_with_priors()}
#' @param run_date run date
#' @param keep_inla_files Logical. Keep intermediate INLA files
#' @param cores Maximum number of threads the inla-program will use
#' @param verbose_output oolean indicating if the inla-program should run in a verbose mode, Default: FALSE
#' @param wgts fixed (optional) weights parameters of the likelihood, so the log-likelihood[i]
#' is changed into weights[i]*log-likelihood[i]. Due to the danger of mis-interpreting
#' the results (see below), this option is DISABLED by default. You can enable
#' this option for the rest of your R session, doing inla.setOption(enable.inla.argument.weights=TRUE).
#' WARNING: The normalizing constant for the likelihood is NOT recomputed, so
#' ALL marginals (and the marginal likelihood) must be interpreted with great care.
#' Possibly, you may want to set the prior for the hyperparameters to "uniform"
#' and the integration strategy to "eb" to mimic a maximum-likelihood approach.
#' Default: 1
#' @param intstrat String: Integration strategy of hyperparmeters in INLA.
#' 'eb'=Empirical Bayes is the fastest by far, but only uses modal values of
#' hyperparams and doesn't integrate over them. 'ccd' performs a smart numerical
#' grid integration strategy but takes much longer then 'eb' and the time scales
#' poorly with the number of hyperparameters in model., Default: 'eb'
#' @param fe_mean_prior prior for mean for fixed effects, Default: 0
#' @param fe_sd_prior prior for precision (NOT sd) for fixed effects, Default: 1
#' @param omp_start OpenMP strategy to be used, one of \code{c('small', 'medium', 'large', 'huge', 'default', 'pardiso.serial', 'pardiso.parallel')}.
#' Default: \code{'huge'}, which is the INLA default for TAUCS solver.
#' @param blas_cores Number of BLAS threads to use. Default: 1.
#' @param pardiso_license Path to PARDISO license. Must be provided if using a Pardiso startegy in \code{omp_strat}. Default: NULL
#' @param sparse_ordering boolean: should the output precision matrix have a
#' sparse (metis) ordering?
#' @return an object of class "inla"-- see \code{?inla} for more details.
#' @export
#'
#' @concept mbg
fit_mbg <- function(indicator_family,
                    stack.obs,
                    spde,
                    cov,
                    N,
                    int_prior_mn,
                    int_prior_prec = 1,
                    f_mbg,
                    run_date,
                    keep_inla_files,
                    cores,
                    verbose_output = FALSE,
                    wgts = 1,
                    intstrat = "eb",
                    fe_mean_prior = 0,
                    fe_sd_prior = 1,
                    omp_strat = "huge",
                    blas_cores = 1,
                    pardiso_license = NULL,
                    sparse_ordering = TRUE) {
  if (length(wgts) == 1 && wgts == 1) {
    wgts <- rep(1, length(N))
  }

  ## Assert that omp_strat is in the domains of what is possible
  stopifnot(omp_strat %in% c("small", "medium", "large", "huge", "default", "pardiso.serial", "pardiso.parallel"))


  message("Checking to see if PARDISO strategy is used")
  if (grepl("pardiso", omp_strat)) {

    ## Pardiso strategy NEEDS a license
    if (is.null(pardiso_license) || !file.exists(pardiso_license)) {
      stop(sprintf("PARDISO license not found or not configured properly (%s). If wanting to use PARDISO, instructions for getting the license is here - https://hub.ihme.washington.edu/display/GT/PARDISO+Solver+with+R-INLA. If you already have a license, please make sure the location of the license file matches with the path passed to the pardiso_license argument of fit_mbg.", if (is.null(pardiso_license)) "not set in config" else pardiso_license))
    }

    ## Set up and test PARDISO solver
    inla.setOption("pardiso.license", pardiso_license)
    (success_msg <- inla.pardiso.check())

    if (!grepl("SUCCESS", success_msg)) {
      warning("PARDISO solver was not set up properly. Reverting to 'huge' strategy")
      omp_strat <- "huge"
      smtp <- "taucs"
    } else {
      message(paste0("Pardiso solver being used with strategy ", omp_strat))
      smtp <- "pardiso"
    }
  } else {
    smtp <- "taucs"
  }


  # Determine control.inla settings
  control.inla.list <- list(int.strategy = intstrat, h = 1e-3, tolerance = 1e-6)
  if (sparse_ordering == TRUE) control.inla.list$reordering <- "METIS"

  # ~~~~~~~~~~~~~~~~
  # fit the model
  # inla weight option removed from inla package in 4.1.3 and later, only run for
  # earlier versions to enable weights
  if(utils::compareVersion(paste0(R.version$major, ".", R.version$minor),"4.1.3") == -1) {
    inla.setOption("enable.inla.argument.weights", TRUE)
  }

  message("Fitting INLA model")

  # set a prior variance of 1.96 on the intercept as this is
  # roughly as flat as possible on logit scale without >1 inflection

  # code just to fit the model (not predict)
  inla_working_dir <- paste0("<<<< FILEPATH REDACTED >>>>", run_date)
  dir.create(inla_working_dir, showWarnings = FALSE)
  if (indicator_family == "binomial") {
    system.time(
      res_fit <- inla(f_mbg,
        data = inla.stack.data(stack.obs),
        control.predictor = list(
          A = inla.stack.A(stack.obs),
          link = 1,
          compute = FALSE
        ),
        control.fixed = list(
          expand.factor.strategy = "inla",
          mean = list(
            int = int_prior_mn,
            default = fe_mean_prior
          ),
          prec = list(
            int = int_prior_prec,
            default = fe_sd_prior
          )
        ),
        control.compute = list(
          dic = TRUE,
          waic = TRUE,
          cpo = TRUE,
          config = TRUE,
          openmp.strategy = omp_strat,
          smtp = smtp
        ),
        control.inla = control.inla.list,
        family = "binomial",
        num.threads = cores,
        blas.num.threads = blas_cores,
        Ntrials = N,
        verbose = verbose_output,
        working.directory = inla_working_dir,
        weights = wgts,
        keep = TRUE
      )
    )
  }
  if (indicator_family == "gaussian") {
    system.time(
      res_fit <- inla(f_mbg,
        data = inla.stack.data(stack.obs),
        control.predictor = list(
          A = inla.stack.A(stack.obs),
          compute = FALSE
        ),
        control.fixed = list(
          expand.factor.strategy = "inla",
          prec.intercept = 1,
          mean.intercept = int_prior_mn,
          mean = 0,
          prec = 2
        ),
        control.compute = list(
          dic = TRUE,
          waic = TRUE,
          cpo = TRUE,
          config = TRUE,
          openmp.strategy = omp_strat,
          smtp = smtp
        ),
        control.inla = control.inla.list,
        family = "gaussian",
        num.threads = cores,
        blas.num.threads = blas_cores,
        verbose = TRUE,
        working.directory = inla_working_dir,
        weights = wgts,
        scale = N,
        keep = TRUE
      )
    )
  }
  if (indicator_family == "beta") {
    system.time(
      res_fit <- inla(f_mbg,
        data = inla.stack.data(stack.obs),
        control.predictor = list(
          A = inla.stack.A(stack.obs),
          compute = FALSE
        ),
        control.fixed = list(
          expand.factor.strategy = "inla",
          prec.intercept = 1,
          mean.intercept = int_prior_mn,
          mean = 0,
          prec = 2
        ),
        control.compute = list(
          dic = TRUE,
          waic = TRUE,
          cpo = TRUE,
          config = TRUE,
          openmp.strategy = omp_strat,
          smtp = smtp
        ),
        control.inla = control.inla.list,
        family = "beta",
        num.threads = cores,
        blas.num.threads = blas_cores,
        verbose = TRUE,
        working.directory = inla_working_dir,
        weights = wgts,
        scale = N,
        keep = TRUE
      )
    )
  }

  return(res_fit)
}
