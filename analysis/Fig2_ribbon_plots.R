
# libraries
library(data.table)
library(fst)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(cowplot)

# load data
dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
data <- read_fst(paste0(dir_data, 'data_complete_climate.fst'), as.data.table = TRUE)

# path to figures
dir_figs <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/figures/"

# load models from main analysis
load(paste0(dir_data, 'aic_models_main.rda'))

# select percentile of interest (some tables show 950 and 990 --> need to run this script with both)
pctile <- 950

# plotting colors
ctype_cols <- c("Nationwide" = "orangered", 
                "Temperate" = "#4f974f",
                "Continental" = "#7627bf",
                "Arid" = "#db3d89",
                "Tropical" = "skyblue3")



################################################################################
########################### FIGURE 2A (lag-response) ########################### 
################################################################################

### lag response plots

# by climate type
dat_2a_ctype <- rbindlist(lapply(names(cp), function(c) {
  data.frame(ctype = c,
             lag = 0:14,
             fit = cp[[c]]$matfit[pctile, ],
             se = cp[[c]]$matse[pctile, ])
}))

# nationwide
dat_2a_all <- data.frame(ctype = "Nationwide",
                         lag = 0:14,
                         fit = cp_all$matfit[pctile, ],
                         se = cp_all$matse[pctile, ])


# bind rows
dat_2a <- rbind(dat_2a_ctype, dat_2a_all)

# new columns for OR and 95% CI
dat_2a <- dat_2a %>%
  mutate(or = exp(fit),
         or_low = exp(fit - 1.96 * se),
         or_high = exp(fit + 1.96 * se))

# make climate type factor to set order
dat_2a$ctype <- factor(dat_2a$ctype,
                       levels = c("Nationwide", "Temperate", "Continental", "Arid", "Tropical"))

# plot
plot_2a <- dat_2a %>%
  ggplot(aes(x = lag, y = or)) +
  geom_hline(yintercept = 1, col = "gray50", lty = 2) +
  geom_line(aes(col = ctype)) +
  geom_ribbon(aes(ymin = or_low, ymax = or_high, fill = ctype), alpha = 0.3) +
  labs(title = "Figure 2(a). Lag-response relationship for one day of exposure to the 99th versus the 50th percentile of heat index \ndistributions.",
       x = "Lag days (days after exposure)",
       y = "Odds ratio") +
  facet_grid(~ctype) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                   breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = ctype_cols) +
  scale_color_manual(values = ctype_cols) +
  coord_cartesian(ylim = c(0.95, 1.04)) +
  theme_light() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10),
        plot.margin = margin(10, 10, 25, 10)) # extra space below

# get these results in a table for Supplement Table 2
# table has results for 95th percentile and 99th percentile (change percentile at the top)
dat_2a |>
  filter(lag %in% 0:2) |>
  mutate(or = paste0(round(or, 3), " (", round(or_low, 3), ", ", round(or_high, 3), ")")) |>
  select(ctype, lag, or)



#################################################################
#################### FIGURE 2B (cumulative) ##################### 
#################################################################

### cumulative effect plots

# by climate type
dat_2b_ctype <- rbindlist(lapply(names(cp), function(c) {
  data.frame(ctype = c,
             lag = 0:14,
             fit = cp[[c]]$cumfit[pctile, ],
             se = cp[[c]]$cumse[pctile, ])
}))

# nationwide
dat_2b_all <- data.frame(ctype = "Nationwide",
                         lag = 0:14,
                         fit = cp_all$cumfit[pctile, ],
                         se = cp_all$cumse[pctile, ])

# bind rows
dat_2b <- rbind(dat_2b_ctype, dat_2b_all)

# new columns for OR and 95% CI
dat_2b <- dat_2b %>%
  mutate(or = exp(fit),
         or_low = exp(fit - 1.96 * se),
         or_high = exp(fit + 1.96 * se))

# make climate type factor to set order
dat_2b$ctype <- factor(dat_2b$ctype,
                       levels = c("Nationwide", "Temperate", "Continental", "Arid", "Tropical"))

# plot
plot_2b <- dat_2b %>%
  ggplot(aes(x = lag, y = or)) +
  geom_hline(yintercept = 1, col = "gray50", lty = 2) +
  geom_line(aes(col = ctype)) +
  geom_ribbon(aes(ymin = or_low, ymax = or_high, fill = ctype), alpha = 0.3) +
  labs(title = "Figure 2(b). Cumulative effects of multiple days of sustained exposure to the 99th versus 50th percentile of heat \nindex distributions.",
       x = "Cumulative days of sustained exposure",
       y = "Odds ratio") +
  facet_grid(~ctype) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(0.9, 1, 1.1)) +
  scale_fill_manual(values = ctype_cols) +
  scale_color_manual(values = ctype_cols) +
  coord_cartesian(ylim = c(0.85, 1.12)) +
  theme_light() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10),
        plot.margin = margin(10, 10, 25, 10)) # extra space below

# get these results in a table for Table 2
# table has results for 95th percentile and 99th percentile (change percentile at the top)
dat_2b |>
  filter(lag %in% 0:2) |>
  mutate(or = paste0(round(or, 3), " (", round(or_low, 3), ", ", round(or_high, 3), ")")) |>
  select(ctype, lag, or)


#################################################################
############# FIGURE 2C (percentiles) ########################### 
#################################################################

# percentiles for comparison to 500
perc <- 500:999

# data by climate type
dat_2c_ctype <- rbindlist(lapply(names(cp), function(c) {
    rbindlist(lapply(perc, function(p) {
      data.frame(ctype = c,
                 val = cp[[c]]$predvar[p],
                 lag = 0:14,
                 fit = cp[[c]]$cumfit[p,],
                 se = cp[[c]]$cumse[p,])
    }))
}))

# nationwide
dat_2c_all <- rbindlist(lapply(perc, function(p) {
      data.frame(ctype = "Nationwide",
                 val = cp_all$predvar[p],
                 lag = 0:14,
                 fit = cp_all$cumfit[p,],
                 se = cp_all$cumse[p,])
}))

# bind rows
dat_2c <- rbind(dat_2c_ctype, dat_2c_all)

# filter to lag = 2
dat_2c <- dat_2c[lag == 2]

# new columns for OR and 95% CI
dat_2c <- dat_2c %>%
  mutate(or = exp(fit),
         or_low = exp(fit - 1.96 * se),
         or_high = exp(fit + 1.96 * se))

# make climate type factor to set order
dat_2c$ctype <- factor(dat_2c$ctype,
                       levels = c("Nationwide", "Temperate", "Continental", "Arid", "Tropical"))

# plot
plot_2c <- dat_2c %>%
  ggplot(aes(x = val, y = or)) +
  geom_hline(yintercept = 1, col = "gray50", lty = 2) +
  geom_line(aes(col = ctype)) +
  geom_ribbon(aes(ymin = or_low, ymax = or_high, fill = ctype), alpha = 0.3) +
  labs(title = "Figure 2(c). Exposure-response relationship after three days of sustained exposure to elevated heat index \npercentiles versus the 50th percentile of heat index distributions.",
       x = "Heat index percentiles",
       y = "Odds ratio") +
  facet_grid(~ctype) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = scales::pretty_breaks()) +
  scale_fill_manual(values = ctype_cols) +
  scale_color_manual(values = ctype_cols) +
  coord_cartesian(ylim = c(0.93, 1.12)) +
  theme_light() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10))


#################################################################
########################### ALL PANELS ########################## 
#################################################################

# save figure with all panels (only for 99th percentile)
if(pctile == 0.990){
  pdf(paste0(dir_figs, "fig2_ribbons.pdf"), height = 9, width = 8)
  
  # align plots (so the plotting space is the same)
  fig2 <- cowplot::align_plots(plot_2a, plot_2b, plot_2c, align = "hv")
  plot_grid(fig2[[1]], fig2[[2]], fig2[[3]], ncol = 1)
  
  dev.off()
  
}

