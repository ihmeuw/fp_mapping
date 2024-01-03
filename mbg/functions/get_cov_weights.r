####################################################################################################
## Description:   Get covariate "importance" scores.
##
## Inputs:        Raked mean rasters (files ending in "_raked_mean_raster.tif", in
##                  <<<< FILEPATH REDACTED >>>>')
##
## Output:        PDF of maps ('<<<< FILEPATH REDACTED >>>>/[indicator]_raked_mean.pdf')
####################################################################################################

get_cov_weights <- function(indicator, indicator_group, run_date, regions, outdir) {

  all_plots <- lapply(regions, function(r) {

    # calculate weights (this auto-saves the output)
    cov.wts <- get.cov.wts(rd = run_date,
                           ind = indicator,
                           ind_gp = indicator_group,
                           reg = r,
                           age = 0,
                           holdout = 0)

    # make plots
    cov.plots <- plot.cov.wts(rd = run_date,
                              ind = indicator,
                              ind_gp = indicator_group,
                              reg = r,
                              age = 0,
                              holdout = 0)
    cov.plots <- cov.plots + labs(x="", y="", title=r)
    return(cov.plots)
  })

  # save plots (combined, and individually)
  pdf(paste0(outdir, "/cov_wts_all_regions.pdf"), width=10, height=7)
  do.call("grid.arrange", all_plots)
  for (ii in 1:length(regions)) print(all_plots[[ii]])
  dev.off()

  return("Plots saved!")
}
