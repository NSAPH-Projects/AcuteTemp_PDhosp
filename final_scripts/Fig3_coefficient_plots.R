
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

dat_F <- #rbindlist(lapply(names(cp_F), function(s) {
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
dat_F$sex <- "Female"


#---------- data by climate type for sex = male

dat_M <- #rbindlist(lapply(names(cp_M), function(s) {
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
dat_M$sex <- "Male"


# # combine female and male
# plot_erc_FM <- rbind(plot_erc_F, plot_erc_M)


#---------- data for the whole country for sex = female

dat_all_F <- #rbindlist(lapply(names(cp_F), function(s) {
  #rbindlist(lapply(names(cp_F), function(c) {
    rbindlist(lapply(c(500, 900, 950, 990), function(p) {
      data.frame(#subgroup = s,
        ctype = "Nationwide",
        #n = cp_F[[s]]$n,
        perc = perc[p],
        val = cp_all_F$predvar[p],
        lag = 0:14,
        fit = cp_all_F$cumfit[p,],
        se = cp_all_F$cumse[p,])
    }))
  #}))
#}))
dat_all_F$sex <- "Female"


#---------- data for the whole country for sex = male

dat_all_M <- #rbindlist(lapply(names(cp_M), function(s) {
  #rbindlist(lapply(names(cp_M), function(c) {
  rbindlist(lapply(c(500, 900, 950, 990), function(p) {
    data.frame(#subgroup = s,
      ctype = "Nationwide",
      #n = cp_M[[s]]$n,
      perc = perc[p],
      val = cp_all_M$predvar[p],
      lag = 0:14,
      fit = cp_all_M$cumfit[p,],
      se = cp_all_M$cumse[p,])
  }))
#}))
#}))
dat_all_M$sex <- "Male"



#-----------

# bind all dfs
dat_fig3 <- rbind(dat_F, dat_M, dat_all_F, dat_all_M)

# new columns for OR and 95% CI
dat_fig3 <- dat_fig3 %>%
  mutate(or = exp(fit),
         or_low = or - 1.96*se,
         or_high = or + 1.96*se)

# restrict to rows of interest
dat_fig3 <- dat_fig3[lag == 2 & val == 0.990]



#---------- data across all climate types and both sexes

# this was previously the black dashed line

# plot_erc_all <- #rbindlist(lapply(names(cp_M), function(s) {
#   #rbindlist(lapply(names(cp_all), function(c) {
#   rbindlist(lapply(c(500, 900, 950, 990), function(p) {
#     data.frame(#subgroup = s,
#       #ctype = c,
#       #n = cp_all[[s]]$n,
#       perc = perc[p],
#       val = cp_all$predvar[p],
#       lag = 0:14,
#       fit = cp_all$cumfit[p,],
#       se = cp_all$cumse[p,])
#   }))
# #}))
# #}))
# 
# # get overall OR
# combined_fit_OR <- exp(plot_erc_all[lag == 3 & val == 0.990, fit])



########################################################################
########################### NEW FIGURE 3 ###############################
########################################################################

# make climate type factor to set order
dat_fig3$ctype <- factor(dat_fig3$ctype,
                       levels = c("Nationwide", "Temperate", "Continental", "Arid", "Tropical"))

# plot
pdf(paste0(dir_figs, "fig3_coefficients.pdf"), height = 2.5, width = 6)
dat_fig3 |>
  ggplot(aes(x = sex, y = or, 
             color = sex, group = sex,
             ymin = or_low, ymax = or_high)) +
  #geom_hline(yintercept = combined_fit_OR, linetype = 3, size = 0.5) +
  #geom_hline(yintercept = 1, linetype = 1, linewidth = 0.5, color = "red") +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.5, color = "gray50") +
  geom_errorbar(linewidth = 0.7, width = 0) +
  geom_point(size = 2) +
  scale_fill_manual(values = c("Female" = "gold3",
                               "Male" = "blue3")) +
  scale_color_manual(values = c("Female" = "gold3",
                                "Male" = "blue3")) +
  #theme_minimal(base_size = 12) +
  theme_light(base_size = 10) +
  theme(
    #legend.position = "none",
    #axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    #axis.line = element_line(),
    #axis.ticks = element_line(),
    #panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(color = "black")
  ) +
  facet_grid(~ ctype#, 
             #scales = "free_x", space = "free_x"
  ) +
  # scale_y_continuous(limits = c(0.99, 1.18)) +
  labs(x = "", 
       y = "Odds ratio", 
       color = "", 
       fill = "")
dev.off()


