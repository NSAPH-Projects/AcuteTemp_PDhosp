
# libraries
library(data.table)
library(fst)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

# paths
dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
dir_figs <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/figures/"

# load models from race-specific sub-analyses
files <- c(paste0("data/aic_models_", 
                  c("White", "Black", "Other", "Asian", "Hispanic"),
                  ".rda"))

# Initialize list
cp_list <- list()

# Load each file into a temporary environment
for (f in files) {
  load(f)
  race_name <- race_name <- sub("^.*aic_models_(.*)\\.rda$", "\\1", f)
  
  cp_list[[race_name]] <- c(cp, Nationwide = list(cp_all))
  rm(cp, cp_all)
}

# select percentile of interest
pctile <- 990


################################################################################
########################### FIGURE S1A (lag-response) ##########################
################################################################################

# nationwide
dat_s1a <- rbindlist(lapply(names(cp_list), function(r) {
  data.frame(race = r,
             lag = 0:14,
             fit = cp_list[[r]][["Nationwide"]]$matfit[pctile, ],
             se = cp_list[[r]][["Nationwide"]]$matse[pctile, ])
}))

dat_s1a$race <- factor(dat_s1a$race,
                       levels = c("White", "Black", "Hispanic", "Asian", "Other"))

# new columns for OR and 95% CI
dat_s1a <- dat_s1a %>%
  mutate(or = exp(fit),
         or_low = exp(fit - 1.96 * se),
         or_high = exp(fit + 1.96 * se))

# plot lag response
plot_s1a <- dat_s1a %>%
  ggplot(aes(x = lag, y = or)) +
  geom_hline(yintercept = 1, col = "gray50", lty = 2) +
  geom_line(col = "orangered") +
  geom_ribbon(aes(ymin = or_low, ymax = or_high), fill = "orangered", alpha = 0.3) +
  labs(title = "Figure S1(a). Lag-response relationship for one day of exposure to the 99th versus the 50th percentile of heat index \ndistributions.",
    x = "Lag days (days after exposure)",
    y = "Odds ratio") +
  facet_grid(~race) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = scales::pretty_breaks()) +
  #coord_cartesian(ylim = c(0.77, 1.81)) +
  theme_light() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10),
        plot.margin = margin(10, 10, 25, 10)) # extra space below


#################################################################
#################### FIGURE S1B (cumulative) ##################### 
#################################################################

#---------- get data by climate type for each race

dat_s1b <- rbindlist(lapply(names(cp_list), function(r) {
      data.frame(race = r,
                 lag = 0:14,
                 fit = cp_list[[r]][["Nationwide"]]$cumfit[pctile,],
                 se = cp_list[[r]][["Nationwide"]]$cumse[pctile,])
}))

# new columns for OR and 95% CI
dat_s1b <- dat_s1b %>%
  mutate(or = exp(fit),
         or_low = exp(fit - 1.96 * se),
         or_high = exp(fit + 1.96 * se))

dat_s1b$race <- factor(dat_s1b$race,
                             levels = c("White", "Black", "Hispanic", "Asian", "Other"))

# # plot cumulative effects after 3 days of sustained exposure
# dat_cum[lag == 2] |>
#   ggplot(aes(x = race, y = or, 
#              #color = race, group = race,
#              ymin = or_low, ymax = or_high)) +
#   geom_hline(yintercept = 1, linetype = 2, linewidth = 0.5, color = "gray50") +
#   geom_errorbar(linewidth = 0.7, width = 0, color = "orangered") +
#   geom_point(size = 2, color = "orangered") +
#   #scale_color_brewer(palette = "Dark2") +
#   #coord_cartesian(ylim = c(0, 2.5)) +
#   theme_light(base_size = 12) +
#   theme(
#     #axis.text.x = element_text(angle = 45, hjust = 1),
#     #axis.ticks.x = element_blank(),
#     #axis.text.x = element_blank(),
#     #axis.line = element_line(),
#     #axis.ticks = element_line(),
#     #panel.spacing = unit(1, "lines"),
#     strip.background = element_blank(),
#     strip.text = element_text(color = "black")
#   ) +
#   #facet_grid(~ ctype) +
#   labs(x = "", 
#        y = "Odds ratio", 
#        color = "", 
#        fill = "")

# plot
plot_s1b <- dat_s1b %>%
  ggplot(aes(x = lag, y = or)) +
  geom_hline(yintercept = 1, col = "gray50", lty = 2) +
  geom_line(color = "orangered") +
  geom_ribbon(aes(ymin = or_low, ymax = or_high), alpha = 0.3, fill = "orangered") +
  labs(title = "Figure S1(b). Cumulative effects of multiple days of sustained exposure to the 99th versus 50th percentile of heat \nindex distributions.",
       x = "Cumulative days of sustained exposure",
       y = "Odds ratio") +
  facet_grid(~race) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  #coord_cartesian(ylim = c(0.77, 1.81)) +
  theme_light() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10),
        plot.margin = margin(10, 10, 25, 10)) # extra space below

# get cumulative effects for 1, 2, 3 days
table_cum <- dat_s1b[lag %in% c(0:2)] |>
  mutate(or = paste0(round(or, 3), " (", round(or_low, 3), ", ", round(or_high, 3), ")")) |>
  select(race, lag, or) %>%
  pivot_wider(names_from = lag, values_from = or)
# and order by race levels
table_cum$race <- factor(table_cum$race, levels = levels(table_cum$race))
table_cum <- table_cum[order(table_cum$race), ]


#################################################################
################### FIGURE 2C (percentiles) ##################### 
#################################################################

# percentiles for comparison to 500
perc <- 500:999

dat_s1c <- rbindlist(lapply(names(cp_list), function(r) {
    rbindlist(lapply(perc, function(p) {
      data.frame(race = r,
                 perc = cp_list[[r]][["Nationwide"]]$predvar[p],
                 lag = 0:14,
                 fit = cp_list[[r]][["Nationwide"]]$cumfit[p,],
                 se = cp_list[[r]][["Nationwide"]]$cumse[p,])
    }))
}))


# filter to lag = 2 (3rd day of sustained exposure)
dat_s1c <- dat_s1c[lag == 2]

dat_s1c$race <- factor(dat_s1c$race,
                       levels = c("White", "Black", "Hispanic", "Asian", "Other"))


# new columns for OR and 95% CI
dat_s1c <- dat_s1c %>%
  mutate(or = exp(fit),
         or_low = exp(fit - 1.96 * se),
         or_high = exp(fit + 1.96 * se))

# plot
plot_s1c <- dat_s1c %>%
  ggplot(aes(x = perc, y = or)) +
  geom_hline(yintercept = 1, col = "gray50", lty = 2) +
  geom_line(color = "orangered") +
  geom_ribbon(aes(ymin = or_low, ymax = or_high), alpha = 0.3, fill = "orangered") +
  labs(title = "Figure S1(c). Exposure-response relationship after three days of sustained exposure to elevated heat index \npercentiles versus the 50th percentile of heat index distributions.",
       x = "Heat index percentiles",
       y = "Odds ratio") +
  facet_grid(~race) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0.5, 1, by = 0.1)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = scales::pretty_breaks()) +
  #coord_cartesian(ylim = c(0.77, 1.81)) +
  theme_light() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10))




#################################################################
########################### ALL PANELS ########################## 
#################################################################

# save figure with all panels (only for 99th percentile)
pdf(paste0(dir_figs, "figS1_ribbons.pdf"), height = 9, width = 8)
  
# align plots (so the plotting space is the same)
fig_s1 <- cowplot::align_plots(plot_s1a, plot_s1b, plot_s1c, align = "hv")
plot_grid(fig_s1[[1]], fig_s1[[2]], fig_s1[[3]], ncol = 1)

dev.off()




