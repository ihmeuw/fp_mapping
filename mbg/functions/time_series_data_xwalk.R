time_series_data_xwalk <- function (model_data, input_data, admin = 0, val_range = c(0,1), title_plot_size = 30, ind_title = "")
{
  carto_discrete <- rep(c("#7F3C8D", "#11A579", "#F2B701",
                          "#E73F74", "#3969AC", "#80BA5A", "#E68310", "#008695",
                          "#CF1C90", "#f97b72", "#4b4b8f", "#A5AA99"), 3)

  gg_admin <- ggplot() + geom_ribbon(data = model_data, aes(x = year, ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(data = model_data, aes(x = year, y = mean)) + theme_bw(base_size = 16) +
    geom_point(data = input_data[round(input_data$outcome,5) != round(input_data$outcome_pre,5)], aes(x = year, y = outcome_pre, size = N_pre, shape = as.factor(point), color = as.factor(source)),
               alpha = 0.8, fill = NA) + scale_color_manual("Survey", values = carto_discrete, drop = F) +
    geom_point(data = input_data, aes(x = year, y = outcome, size = N, shape = as.factor(point), fill = as.factor(source)),
               alpha = 0.7) + scale_fill_manual("Survey", values = carto_discrete, drop = F) +
    geom_point(data=input_data[!is.na(flag)],  aes(x = year, y = outcome, size = N), shape = 4, color = "black", alpha = 0.7) +
    scale_shape_manual("Type", breaks = c("0","1", "2", "3","4","5"), values = c(22, 21, 12, 10, 22, 21),
                       label = c("polygon crosswalked","point crosswalked", "Subnationally \n Representative", "Subnationally \n Representative","polygon initial","point initial"), drop = F) +
    facet_wrap(~plot_name) + coord_cartesian(ylim = val_range) +
    scale_size(range = c(1, 7)) + theme(strip.background = element_blank(), plot.caption = element_text(hjust = 0.5),
                                        plot.title = element_text(size = title_plot_size,face = "bold"),
                                        axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_text(size = 10),
                                        legend.text = element_text(size = 7), legend.justification = "top") +
    guides(color = "none", size= guide_legend("N", order=3), fill = guide_legend(order = 1, override.aes = list(size = 5, shape = 21)),
           shape = guide_legend(order = 2, override.aes = list(size = 5, alpha = 0.7, fill=c("black", "black", "transparent","transparent"), shape=c(22,21,22,21)))) +

    labs(y = ind_title, x = "Year") + {
      if (admin == 0)
        labs(title = paste0(ind_title, " by Country"))
    } + {
      if (admin == 1) {
        labs(title = paste0(ind_title, " by First-level Administrative Unit"),
             caption = paste0("Time series depict first-level administrative units except for NATIONAL,\n",
                              "which shows the time series for the entire country"))
      }
    } + {
      if (admin == 2) {
        labs(title = paste0(ind_title, " by Second-level Administrative Unit"),
             caption = paste0("Plots shown are for second-level administrative units except for ADMIN 1,\n",
                              "which shows the time series for the entire first-level administrative unit"))
      }
    }
  return(gg_admin)
}
