#' @title Build an INLA data stack for modeling
#'
#' @description Generates an INLA stack that contains information
#' mapping data to various INLA effects (GP, random effects, ...)
#'
#' @param df data.frame/table containing observational data. must have
#' latitude and longitude columns as well as extracted values for
#' all covariates that will be used in modeling
#'
#' @param fixed_effects string containing fixed effects used in
#' res_fit separated by " + "
#'
#' @param mesh_s An 'inla.mesh' object used to fit the spatial SPDE GP
#' approximation in res_fit
#'
#' @param mesh_t An 'inla.mesh' object used to fit the temporal
#' correlation structure in res_fit.
#'
#' @param exclude_cs vector of strings detailing covariates that are
#' not center-scaled
#'
#' @param spde_prior String containing a list. Specifies the type of prior used
#' for the Matern model parameters. If type = "nonpc" then use non pc
#' prior. User can specify nominal prior means: spde_prior$prior$range.nominal
#' and spde_prior$prior$variance.nominal. Defaults are INLA defaults, that is
#' 20\% of the range of the mesh and 1, respectively. If type = "pc" then use
#' pc prior.  User can specify spde_prior$prior$range = (range0, Prange) i.e.
#' P(range < range0) = Prange and spde_prior$prior$sigma = (sigma0, Psigma)
#' i.e. P(sigma > sigma0) = Psigma. Defaults for Prange and Psigma are 0.05.
#' Default for range0 is 5\% of the range of the mesh and sigma0=3.
#'
#' @param coefs.sum1 Logical. If TRUE, add a constraint to ensure
#' covariate coefs sum to 1 in fitted model
#'
#' @param use_ctry_res Logical. If TRUE, include country random
#' effects
#'
#' @param use_subnat_res Logical. If TRUE, include subnational
#' (admin-1) random effects
#'
#' @param use_nid_res Logical. If TRUE, include nid random effects
#'
#' @param use_nugget Logical. If TRUE, include a nugget effect
#'
#' @param stacker_names string vector of child model names
#'
#' @param yl numeric vector of years in model
#'
#' @param zl numeric vector of third dimension in kronecker
#' product. Only implemented in TMB
#'
#' @param zcol column name of z values associated with zl. Only
#' implemented in TMB
#'
#' @param scale_gaussian_variance_N Logical. Do you want to scale
#' gaussian variance by sample size? Only implemented in TMB.
#'
#' @param tmb Logical. use tmb?
#'
#' @param cov_constraints named int vector. integer vector indexed by covariate names
#'
#' @param use_gp Logical. include a space x time (x age) gp
#'
#' @param use_space_only_gp Logical. include a space only (time stationary) gp
#'
#' @param use_time_only_gmrf Logical. include a time only (space stationary) gp
#'
#' @param use_timebyctry_res Logical. include a time only gp by country
#'
#' @param adm0_list Vector containing the adm0 codes for the modeling region
#'
#' @param shapefile_version character. Version of shape file to use
#'
#' @param st_gp_int_zero Logical. Should the space-time GP be forced to interate to 0? Default: FALSE
#'
#' @param s_gp_int_zero Logical. Should the space GP be forced to interate to 0? Only used if use_space_only_gp=TRUE. Default: FALSE
#'
#' @param use_age_only_gmrf Logical. include an age only gp?
#'
#' @param seed Seed to set for RNG
#'
#' @param ... used to pass in config args to \code{\link{build_mbg_data_stack_tmb}} for the package which doesn't keep the config args in global env
#'
#' @return List containing 1) 'inla.stack' object, 2) 'inla.spde'
#' object for interaction gp, 3) cs_df: a center-scale dataframe containing info on
#' mean and SD used to scale covariates in design matrix 4) 'inla.spde'
#' object for space only gp
#'
#' @export
build_mbg_data_stack <- function(df,
                                 fixed_effects,
                                 mesh_s,
                                 mesh_t,
                                 exclude_cs = "",
                                 spde_prior = "list(type='nonpc')",
                                 coefs.sum1 = FALSE,
                                 use_ctry_res = FALSE,
                                 use_subnat_res = FALSE,
                                 use_nid_res = FALSE,
                                 use_nugget = FALSE,
                                 stacker_names = use_global_if_missing("child_model_names"),
                                 yl = year_list,
                                 zl = z_list,
                                 zcol = zcol,
                                 scale_gaussian_variance_N = TRUE,
                                 tmb = FALSE,
                                 cov_constraints = use_global_if_missing("cov_constraints"),
                                 use_gp = TRUE,
                                 use_space_only_gp = FALSE,
                                 use_time_only_gmrf = FALSE,
                                 use_timebyctry_res = FALSE,
                                 use_timebyadm1 = FALSE,
                                 use_timebyadm2 = FALSE,
                                 adm0_list = NULL,
                                 shapefile_version = "current",
                                 st_gp_int_zero = FALSE,
                                 s_gp_int_zero = FALSE,
                                 use_age_only_gmrf = FALSE,
                                 seed = NULL,
                                 ...) {
  set.seed(seed)
  increment_seed(seed)

  if (nchar(stacker_names[1]) == 0 & coefs.sum1 == TRUE) {
    message("WARNING! You've chosen sum-to-1 but don't appear to be using any stackers. Unless you have a very good reason to do this, it probably doesn't make sense. As such, we're setting coefs.sum1 <- FALSE")
    coefs.sum1 <- FALSE
  }

  if (use_gp & use_space_only_gp & !st_gp_int_zero & !s_gp_int_zero) {
    message("WARNING! You've chosen to use a s-t and a s-only gp and have set both integration constraints to FALSE. This presents identifiability issues so we ase turning the space-only integrate-to-0 constraint to TRUE (i.e. s_gp_int_zero <- TRUE)")
    s_gp_int_zero <- TRUE
  }

  # if fitting with tmb, punt this over to
  if (tmb == TRUE) {
    message("Returning a TMB model stack.")
    return(
      build_mbg_data_stack_tmb(
        d = df,
        yl = yl, # year list
        fes = fixed_effects, # fixed effects in the model
        indic = indicator, # indicator
        exclude_cs = exclude_cs,
        nugget = use_nugget,
        country_re = use_ctry_res,
        nid_re = use_nid_res,
        zl = zl,
        zcol = zcol,
        shapefile_version = shapefile_version,
        scale_gaussian_variance_N = scale_gaussian_variance_N,
        mesh_s = mesh_s, # spatial mesh
        spde_prior = use_global_if_missing("spde_prior"),
        cov_constraints = cov_constraints,
        main_space_effect = as.logical(use_space_only_gp),
        main_time_effect = as.logical(use_time_only_gmrf),
        main_age_effect = as.logical(use_age_only_gmrf),
        full_interacting_effect = as.logical(use_gp),
        use_timebyctry_res = as.logical(use_timebyctry_res),
        coefs.sum1 = coefs.sum1,
        stacker_names = stacker_names,
        ...
      )
    )

    # else do the inla version
  } else {

    # construct an SPDE model with a Matern kernel for the space-time GP
    message("Building SPDE...")

    spde_prior <- eval(parse(text = spde_prior)) # convert from string to list

    spde_list <- build_spde_prior(spde_prior, mesh_s, st_gp_int_zero)
    spde_prior <- spde_list$spde_prior
    spde <- spde_list$spde

    ## Build projector matrix between data locs and spatial mesh
    data.locs <- as.matrix(df[, c("longitude", "latitude"), with = F])
    if (mesh_s$manifold == "S2") {
      ## then the mesh is on the sphere and we need to use 3d coords
      data.locs <- lonlat3D(data.locs[, 1], data.locs[, 2])
    }

    ## here we actually build the projector matrix, A
    ## it is grouped across periods in time
    A <- inla.spde.make.A(
      mesh = mesh_s,
      loc = data.locs,
      group = df$period,
      group.mesh = mesh_t
    )

    if (coefs.sum1) {
      ## make A matrix comprised of covariate fixed_effects column vectors
      f.e.v <- stacker_names ## fixed eff. vec.
      A.covar <- as.matrix(df[, f.e.v, with = FALSE])
    }

    ## confusingly, this 'space' index is actually space indices in time
    ## and it is a space index if only one time period is used
    if (is.null(mesh_t)) {
      space <- inla.spde.make.index("space",
        n.spde = spde$n.spde
      )
    } else {
      space <- inla.spde.make.index("space",
        n.spde = spde$n.spde,
        n.group = mesh_t$m
      )
    }

    ## make another set of objects just for space only
    ## make another projection matrix that is fixed across all time
    A.sp <- inla.spde.make.A(
      mesh = mesh_s,
      loc = data.locs
    )

    ## make another spde object in case you want one (st or s) to
    ## int to 0 but the other one to be unconstrained
    if (spde_prior$type == "pc") { # PC prior
      spde.sp <- inla.spde2.pcmatern(
        mesh = mesh_s,
        alpha = 2,
        prior.range = spde_prior$prior$range,
        prior.sigma = spde_prior$prior$sigma,
        constr = s_gp_int_zero
      )
    } else { # Non PC prior
      spde.sp <- inla.spde2.matern(
        mesh = mesh_s, alpha = 2, constr = s_gp_int_zero,
        prior.range.nominal = spde_prior$prior$range.nominal,
        prior.variance.nominal = spde_prior$prior$variance.nominal
      )
    }

    ## here we make the actual space (time stationary) index
    ## which can be used even if there are multiple time points
    sp.no.t <- inla.spde.make.index("sp.no.t",
      n.spde = spde.sp$n.spde
    )

    # find cov indices
    if (fixed_effects != "NONE" & nchar(fixed_effects) > 0) {
      f_lin <- reformulate(fixed_effects)
      message("Indexing covariates...")
      covs_indices <- unique(c(match(all.vars(f_lin), colnames(df))))

      # make design matrix, center the scaling
      design_matrix <- data.frame(
        int = 1,
        df[, covs_indices, with = F]
      )

      cs_df <- getCentreScale(design_matrix, exclude = c("int", exclude_cs))

      design_matrix <- centreScale(design_matrix,
        df = cs_df
      )

      # floating points cause seeding to fail in model_fit, rounding to 12 digits
      if (!is.null(seed) & (stacker_names[1] != "")) {
        message("Seed is set; rounding stacking covs to 12 decimals to avoid floating point discrepancies")
        design_matrix <- data.table(design_matrix)
        design_matrix[, (stacker_names) := round(.SD, 12), .SDcols = stacker_names]
        design_matrix <- data.frame(design_matrix)
      }
    } else {
      design_matrix <- data.frame(int = rep(1, nrow(df)))
      cs_df <- getCentreScale(design_matrix, exclude = c("int", "rates"))
    }

    # construct a 'stack' object for observed data
    cov <- df[[indicator]] # N+_i
    N <- df$N # N_i

    if (use_ctry_res) {
      ## add an numeric gaul code to use in random effects
      design_matrix$CTRY.ID <- gaul_convert(df$country, shapefile_version = shapefile_version)
    }

    if (use_subnat_res) {
      ## add subnat IDs to use in random effects
      for (i in 1:length(unique(na.omit(df$subnat_re_ADM1_CODE)))) {
        design_matrix[[paste0("SUBNAT.ID", i)]] <- df[[paste0("SUBNAT", i)]]
      }
    }



    if (use_nugget) {
      design_matrix$IID.ID <- 1:nrow(design_matrix)
    }

    if (use_nid_res) {
      design_matrix$NID.ID <- df$nid
    }

    if (use_time_only_gmrf) {
      if (use_timebyctry_res) {
        ## add in years by ADM0.ID to use in random effects
        for (adm0_code in adm0_list) {
          design_matrix[[paste0("ADM0.ID", adm0_code)]] <- NA
          design_matrix[[paste0("ADM0.ID", adm0_code)]][df$adm0code == adm0_code] <- df$year[df$adm0code == adm0_code]
        }
      } else {
        design_matrix$t.no.sp <- df$year
      }
    }

    if (use_timebyadm1) {
      design_matrix$timeby_ADM1_CODE <- df$timeby_ADM1_CODE
      design_matrix$t.by.adm1 <- df$period
    }
    if (use_timebyadm2) {
      design_matrix$timeby_ADM2_CODE <- df$timeby_ADM2_CODE
      design_matrix$t.by.adm2 <- df$period
    }

    ## initialize some lists for the stacking function
    ## this assumes you alway have the s-t gp and the covariates
    A.list <- list(A, 1) ## this list contains the objects that map between the effects and the data
    e.list <- list(space, design_matrix) ## this list contains the effects [or their indices for things in f()s]

    ## add on sum-to-1 for covars if selected
    if (coefs.sum1 == TRUE & nchar(fixed_effects) > 1) {
      A.list <- c(A.list, list(A.covar = A.covar))
      e.list <- c(e.list, list(covar = 1:ncol(A.covar)))
    }

    ## add on space only selected
    if (use_space_only_gp) {
      A.list <- c(A.list, list(A.sp = A.sp))
      e.list <- c(e.list, list(sp.no.t = sp.no.t))
    }

    message("Stacking data...")
    ## combine all the different pieces
    stack.obs <- inla.stack(
      data = list(covered = cov),
      A = A.list,
      effects = e.list,
      tag = "est"
    )

    return_list <- list(stack.obs, spde, cs_df, spde.sp)

    return(return_list)
  }
}
