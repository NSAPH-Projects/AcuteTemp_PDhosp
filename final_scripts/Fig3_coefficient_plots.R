
# libraries
library(data.table)
library(fst)
library(ggplot2)
library(tidyverse)

# paths
dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
dir_figs <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/figures/"

# load models from male and female sub-analyses
load(paste0(dir_data, "aic_models_main.rda")) 
load(paste0(dir_data, "aic_models_F.rda"))
load(paste0(dir_data, "aic_models_M.rda"))

# note: cp is by climate type, cp_all is national

# percentiles
perc <- 1:999/1000

# n isn't needed
# s is subgroup (e.g. male/female) but those are separate here (combine data later)
# c is climate subtype

#---------- data by climate type for sex = female

plot_erc_F <- #rbindlist(lapply(names(cp_F), function(s) {
  rbindlist(lapply(names(cp_F), function(c) {
    rbindlist(lapply(c(500, 900, 950, 990), function(p) {
      data.frame(#subgroup = s,
                 ctype = c,
                 #n = cp_F[[s]]$n,
                 perc = perc[p],
                 val = cp_F[[c]]$predvar[p],
                 lag = 0:14,
                 fit = cp_F[[c]]$cumfit[p,],
                 se = cp_F[[c]]$cumse[p,])
    }))
  }))
#}))
plot_erc_F$sex <- "Female"


#---------- data by climate type for sex = male

plot_erc_M <- #rbindlist(lapply(names(cp_M), function(s) {
  rbindlist(lapply(names(cp_M), function(c) {
    rbindlist(lapply(c(500, 900, 950, 990), function(p) {
      data.frame(#subgroup = s,
        ctype = c,
        #n = cp_F[[s]]$n,
        perc = perc[p],
        val = cp_F[[c]]$predvar[p],
        lag = 0:14,
        fit = cp_M[[c]]$cumfit[p,],
        se = cp_M[[c]]$cumse[p,])
    }))
  }))
#}))
plot_erc_M$sex <- "Male"


# combine female and male
plot_erc_FM <- rbind(plot_erc_F, plot_erc_M)

# restrict to rows of interest
plot_erc_FM <- plot_erc_FM[lag == 3 & val == 0.990]



#---------- data across all climate types and both sexes
plot_erc_all <- #rbindlist(lapply(names(cp_M), function(s) {
  #rbindlist(lapply(names(cp_all), function(c) {
  rbindlist(lapply(c(500, 900, 950, 990), function(p) {
    data.frame(#subgroup = s,
      #ctype = c,
      #n = cp_all[[s]]$n,
      perc = perc[p],
      val = cp_all$predvar[p],
      lag = 0:14,
      fit = cp_all$cumfit[p,],
      se = cp_all$cumse[p,])
  }))
#}))
#}))

# get overall OR
combined_fit_OR <- exp(plot_erc_all[lag == 3 & val == 0.990, fit])



########################################################################
########################### NEW FIGURE 3 ###############################
########################################################################

# plot
pdf(paste0(dir_figs, "fig3_coefficients.pdf"), height = 4, width = 6)
plot_erc_FM[lag == 3 & val == 0.990] |>
  ggplot(aes(x = sex, 
             y = exp(fit), 
             #color = sex, 
             group = sex,
             ymin = exp(fit - 2*se), ymax = exp(fit + 2*se))) +
  geom_hline(yintercept = combined_fit_OR, linetype = 3, size = 0.5) +
  geom_hline(yintercept = 1, linetype = 1, size = 0.5, color = "red") +
  geom_errorbar(size = 0.7, width = 0) +
  geom_point(size = 2) +
  theme_minimal(
    #base_size = 24
    ) +
  theme(legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(),
    axis.ticks = element_line()) +
  facet_grid(~ ctype#, 
             #scales = "free_x", space = "free_x"
  ) +
  # scale_y_continuous(limits = c(0.99, 1.18)) +
  labs(x = "", y = "Odds ratio of hospitalization", color = "", fill = "")
dev.off()


