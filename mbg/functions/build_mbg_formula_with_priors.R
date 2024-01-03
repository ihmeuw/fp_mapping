#' @title Build an INLA formula for mbg
#' @description construct a formula object for use with R-INLA::inla() model fitting in conjunction with the prepped data object from build_mbg_data_stack()
#'
#' @param fixed_effects a string of model fixed effects of form 'var1 + var2 + var3' or as string vector
#' corresponding to column names in df
#' @param positive_constrained_variables vector containing names of the fixed effects that are positively contrained, Default: NULL
#' @param interact_with_year which fixed effects should be interacted with year, Default: NULL
#' @param int should there be an intercept in the model? Default: TRUE
#' @param nullmodel should the model include fixed effects? Default: FALSE
#' @param add_nugget should an iid nugget term be added? Default: FALSE
#' @param nugget_prior prior for nugget random effect, written in INLA notation, Default: 'list(prior = 'loggamma', param = c(2, 1))'
#' @param add_ctry_res should a country random effect be added?, Default: FALSE
#' @param ctry_re_prior prior for country random effect, written in INLA notation, Default: 'list(prior = 'loggamma', param = c(2, 1))'
#' @param temporal_model_type what should the model for the temporal portion of the space-time gp be?, Default: ''ar1''
#' @param temporal_model_theta1_prior prior for ar1 transformed rho parameter, Default: 'list(prior = 'normal', param = c(0, 1/(2.58^2)))'
#' @param no_gp should a space-time GP be added?, Default: FALSE
#' @param stacker_names string vector of child model names, Default: child_model_names
#' @param coefs.sum1 Logical. If TRUE, add a constraint to ensure
#' covariate coefs sum to 1 in fitted model, Default: FALSE
#' @param subnat_RE Additional admin-1 random effect. Default: FALSE
#' @param subnat_country_to_get Vector of iso3 codes to get subnat REs
#' @param subnat_re_prior String that evaluates to list, prior for subnat REs
#' @param timebycountry_RE Logical. include a time only gp by country
#' @param adm0_list Vector containing the adm0 codes for the modeling region
#' @param use_space_only_gp Logical. include a space only (time stationary) gp. Default: FALSE
#' @param use_time_only_gmrf Logical. include a time only gp. Default: FALSE
#' @param time_only_gmrf_type String indicating name of what type of temporal random should be used
#'
#' @return INLA model formula
#'
#' @export
build_mbg_formula_with_priors <- function(fixed_effects,
                                          positive_constrained_variables = NULL,
                                          interact_with_year = NULL,
                                          int = TRUE,
                                          nullmodel = FALSE,
                                          add_nugget = FALSE,
                                          nugget_prior = "list(prior = 'loggamma', param = c(2, 1))",
                                          add_ctry_res = FALSE,
                                          ctry_re_prior = "list(prior = 'loggamma', param = c(2, 1))",
                                          temporal_model_type = "ar1",
                                          temporal_model_theta1_prior = "list(prior = 'normal', param = c(0, 1/(2.58^2)))",
                                          no_gp = FALSE,
                                          stacker_names = child_model_names,
                                          coefs.sum1 = FALSE,
                                          subnat_RE = FALSE,
                                          subnat_country_to_get = NULL,
                                          subnat_re_prior = "list(prior = 'loggamma', param = c(1, 5e-5))",
                                          timebycountry_RE = FALSE,
                                          adm0_list = NULL,
                                          use_space_only_gp = FALSE,
                                          use_time_only_gmrf = FALSE,
                                          time_only_gmrf_type = "rw2",
                                          use_timebyadm1 = FALSE,
                                          use_timebyadm2 = FALSE, 
                                          timebyadm_type = "iid",
                                          adj_mat_adm1 = NULL,
                                          adj_mat_adm2 = NULL,
                                          use_nid_res = FALSE,
                                          nid_re_prior = "list(prior = 'loggamma', param = c(1, 5e-5))") {
  if (nchar(stacker_names[1]) == 0 & coefs.sum1 == TRUE) {
    message("WARNING! You've chosen sum-to-1 but don't appear to be using any stackers. Unless you have a very good reason to do this, it probably doesn't make sense. As such, we're setting coefs.sum1 <- FALSE")
    coefs.sum1 <- FALSE
  }

  # Set up model equation
  if (int == TRUE) intercept <- "+int" else intercept <- ""
  f_null <- formula(paste0("covered~-1", intercept))

  if (!is.null(interact_with_year)) {
    for (iwy in interact_with_year) {
      fixed_effects <- gsub(iwy, paste0(iwy, "* factor(period)"), fixed_effects)
    }
    if (!is.null(positive_constrained_variables)) {
      stop("Cannot Both Constrain and Interact, one must be NULL.")
    }
  }

  if (!is.null(positive_constrained_variables)) {
    if (!all(positive_constrained_variables %in% strsplit(fixed_effects, " \\+ ")[[1]])) {
      stop("Not all your positive_constrained_variables match fixed_effects")
    }
    for (pcv in positive_constrained_variables) {
      v <- sprintf("f(%s, model='clinear',range=c(0,Inf),initial=0)", pcv)
      fixed_effects <- gsub(pcv, v, fixed_effects)
    }
  }

  f_nugget <- as.formula(paste0('~f(IID.ID, model = "iid", hyper = list(theta=', nugget_prior, "))"))
  f_res <- as.formula(paste0('~f(CTRY.ID, model = "iid", hyper = list(theta=', ctry_re_prior, "))"))
  f_subnat <- as.formula(paste0('~f(SUBNAT.ID, model = "iid", constr=TRUE, hyper = list(theta=', subnat_re_prior, "))"))
  f_nid <- as.formula(paste0('~f(NID.ID, model = "iid", hyper = list(theta=', nid_re_prior, "))"))

  # add subnat RE term for each country specified
  if (subnat_RE == TRUE) {
    f_subnat <- list()
    if ("all" %in% subnat_country_to_get) {
      subnat_country_codes <- na.omit(unique(df$subnat_re_ADM0_CODE))
    } else {
      subnat_country_codes <- get_adm0_codes(subnat_country_to_get, shapefile_version = modeling_shapefile_version)
    }
    for (i in 1:length(subnat_country_codes)) {
      f_subnat <- c(f_subnat, as.formula(paste0("~f(SUBNAT.ID", i, ', model = "iid", constr=TRUE, hyper = list(theta=', subnat_re_prior, "))")))
    }
  }

  ## spatial gps correlated across grouping (usually time)
  test_rho_priors(temporal_model_theta1_prior) ## Report how priors for theta1 (Rho) are being used in Rho space.
  if (temporal_model_type == "ar1") {
    f_space_time <- as.formula(paste0("~f(space, model = spde, group = space.group, control.group = ",
                                      "list(model = '", temporal_model_type, "', ",
                                      "hyper = list(theta1 = ", temporal_model_theta1_prior, ")))"))    
    
  } else if (temporal_model_type %in% c("rw1", "rw2")) {
    f_space_time <- as.formula(paste0("~f(space, model = spde, group = space.group, control.group = ",
                                      "list(model = '", temporal_model_type, "'))"))   
  }
  
  ## space only gp
  f_space <- as.formula("~f(sp.no.t, model = spde.sp)")

  ## time only gmrf
  f_time <- list(as.formula(paste0('~f(t.no.sp, model = "', time_only_gmrf_type, '", hyper = list(theta = list(prior = "pc.prec", param = c(3, 0.05))))')))

  ## time only gp by country
  if (timebycountry_RE == TRUE) {
    f_time <- list()
    for (adm0_code in adm0_list) {
      f_time <- c(f_time, as.formula(paste0("~f(ADM0.ID", adm0_code, ', model = "', time_only_gmrf_type, '", constr=TRUE)')))
    }
  }
  
  # time by adm1 
  if (use_timebyadm1 == TRUE) {
    if (timebyadm_type == "iid") {
      f_time_adm1 <- as.formula(paste0("~f(timeby_ADM1_CODE, model = 'iid',", 
                                       "hyper = list(theta = list(prior = 'pc.prec', param = c(3, 0.05))),", 
                                       "group = t.by.adm1, control.group = list(model = 'rw2'))"))       
    } else if (timebyadm_type == "besag") {
      f_time_adm1 <- as.formula(paste0("~f(timeby_ADM1_CODE, model = 'besag', graph = adj_mat_adm1,", 
                                       "hyper = list(theta = list(prior = 'pc.prec', param = c(3, 0.05))),", 
                                       "group = t.by.adm1, control.group = list(model = 'rw2'))"))
    } else {
      stop(paste("timebyadm_type = ", timebyadm_type, "is not implemented"))
    }  
  }
  
  # time by adm2
  if (use_timebyadm2 == TRUE) {
    if (timebyadm_type == "iid") {
      f_time_adm2 <- as.formula(paste0("~f(timeby_ADM2_CODE, model = 'iid',", 
                                       "hyper = list(theta = list(prior = 'pc.prec', param = c(3, 0.05))),", 
                                       "group = t.by.adm2, control.group = list(model = 'rw2'))"))       
    } else if (timebyadm_type == "besag") {
      f_time_adm2 <- as.formula(paste0("~f(timeby_ADM2_CODE, model = 'besag', graph = adj_mat_adm2,", 
                                       "hyper = list(theta = list(prior = 'pc.prec', param = c(3, 0.05))),", 
                                       "group = t.by.adm2, control.group = list(model = 'rw2'))"))
    } else {
      stop(paste("timebyadm_type = ", timebyadm_type, "is not implemented"))
    }     
  }
  
  # fixed effects
  f.e.v <- base::strsplit(all_fixed_effects, " + ", fixed = T)[[1]] ## fixed effect vector
  f_sum1 <- as.formula(paste0("~f(covar,
                              model = 'iid',
                              extraconstr = list(A = matrix(1, 1, ", length(f.e.v), "), e = 1),
                              hyper=list(theta=list(initial=log(inla.set.control.fixed.default()$prec),
                              fixed=TRUE)))"))

  if (nchar(fixed_effects) <= 1) { ## then we don't have any fixed effects, so run the nullmodel
    nullmodel <- TRUE
  }

  ## build formula starting with f_null
  f_mbg <- f_null

  if (!nullmodel) {
    if (coefs.sum1) {
      f_lin <- f_sum1 ## use the sum1 formula instead of fixed effects.
      # the'fixed' effects may now be
      # found @ inla$fit$summary.random
    } else {
      f_lin <- reformulate(fixed_effects)
    }

    f_mbg <- f_mbg + f_lin
  }

  if (!no_gp) f_mbg <- f_mbg + f_space_time
  if (use_space_only_gp) f_mbg <- f_mbg + f_space
  if (use_time_only_gmrf == TRUE) {
    for (i in 1:length(f_time)) {
      f_mbg <- f_mbg + f_time[[i]]
    }
  }
  if (use_timebyadm1) f_mbg <- f_mbg + f_time_adm1
  if (use_timebyadm2) f_mbg <- f_mbg + f_time_adm2  
  if (add_nugget == TRUE) f_mbg <- f_mbg + f_nugget
  if (add_ctry_res == TRUE) f_mbg <- f_mbg + f_res
  if (subnat_RE == TRUE) {
    for (i in 1:length(f_subnat)) {
      f_mbg <- f_mbg + f_subnat[[i]]
    }
  }
  if (use_nid_res == TRUE) f_mbg <- f_mbg + f_nid

  message(f_mbg)
  return(f_mbg)
}
