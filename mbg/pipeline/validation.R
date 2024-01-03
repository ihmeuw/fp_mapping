source("source_for_setup.R")

user <- Sys.info()["user"]
inputs <-
  list(
    run_date = "<<<<RUN DATE>>>>",
    indicator = "<<<<INDICATOR>>>>",
    indicator_group = "<<<<INDICATOR GROUP>>>>",
    node_id = 105,
    node_name = "j06_MBG_postest",
    dag_path = "<<<< FILEPATH REDACTED >>>>",
    loopvar_index = 3
  )

pipeline_preamble(inputs = inputs)


output_path <- "<<<< FILEPATH REDACTED >>>>"

strata <- unique(as.character(pipeline$loopvars[,1]))

# Combine input data csv files
csvs <- list.files(output_path,
                   pattern = "input_data(.*).csv",
                   full.names = T)

csv_master <- lapply(csvs, fread) %>%
  rbindlist %>%
  subset(., select = names(.) != "V1")
write.csv(csv_master, file = file.path(output_path, 'input_data.csv'))

# Get in and out of sample draws
with_globals(new = list(Regions = eval(parse(text=pipeline$config_list$Regions)),
                        modeling_shapefile_version = pipeline$config_list$modeling_shapefile_version), {
                          run_in_oos <- get_is_oos_draws(ind_gp = pipeline$indicator_group,
                                                         ind = pipeline$indicator,
                                                         rd = pipeline$run_date,
                                                         ind_fm = 'binomial',
                                                         age = 0,
                                                         nperiod = 23,
                                                         yrs = 1998:2020,
                                                         year_col = "year",
                                                         get.oos = as.logical(pipeline$config_list$makeholdouts),
                                                         write.to.file = TRUE,
                                                         shapefile_version = pipeline$config_list$modeling_shapefile_version)
                        })


## set out_dir
out_dir <- file.path(output_path, "summary_metrics")
dir.create(out_dir, recursive = T, showWarnings = F)

## for admin0/1/2
draws.df <- fread(file.path(output_path, 'output_draws_data.csv'))


country.pvtable <- get_pv_table(d = draws.df,
                                indicator_group = pipeline$indicator_group,
                                rd = pipeline$run_date,
                                indicator=pipeline$indicator,
                                aggregate_on='country',
                                draws = 1000,
                                out.dir = out_dir,
                                result_agg_over = c("oos", "region"))

write.csv(country.pvtable,
          file = file.path(output_path, 'summary_metrics', 'country_metrics.csv'))

ad1.pvtable <- get_pv_table(d = draws.df,
                            indicator_group = pipeline$indicator_group,
                            rd = pipeline$run_date,
                            indicator=pipeline$indicator,
                            aggregate_on='ad1',
                            draws = 1000,
                            out.dir = out_dir,
                            result_agg_over = c("oos", "region"))
write.csv(ad1.pvtable,
          file = file.path(output_path, 'summary_metrics', 'ad1_metrics.csv'))

ad2.pvtable <- get_pv_table(d = draws.df,
                            indicator_group = pipeline$indicator_group,
                            rd = pipeline$run_date,
                            indicator=pipeline$indicator,
                            aggregate_on='ad2',
                            draws = 1000,
                            out.dir = out_dir,
                            result_agg_over = c("oos", "region"))
write.csv(ad2.pvtable,
          file = file.path(output_path, 'summary_metrics', 'ad2_metrics.csv'))
