
# libraries
library(data.table)
library(fst)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

# paths
dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
dir_figs <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/figures/"

# load models from race-specific sub-analyses
files <- c(paste0("data/aic_models_", 
                  c("white", "black", "other", "asian", "hispanic", "native"),
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

# percentiles
perc <- 1:999/1000


#---------- get data by climate type for each race

dat <- rbindlist(lapply(names(cp_list), function(r) {
  rbindlist(lapply(names(cp_list[[r]]), function(c) {
  rbindlist(lapply(c(500, 900, 950, 990), function(p) {
    data.frame(ctype = c,
               race = r,
               perc = perc[p],
               val = cp_list[[r]][[c]]$predvar[p],
               lag = 0:14,
               fit = cp_list[[r]][[c]]$cumfit[p,],
               se = cp_list[[r]][[c]]$cumse[p,])
    }))
  }))
}))



########################################################################
####################### cumulative OR ############################
########################################################################


# restrict to rows of interest
dat_cum <- dat[lag == 2 & val == 0.990 & ctype == "Nationwide"]

# new columns for OR and 95% CI
dat_cum <- dat_cum %>%
  mutate(or = exp(fit),
         or_low = exp(fit - 1.96 * se),
         or_high = exp(fit + 1.96 * se))

# # make climate type factor to set order
# dat_cum$ctype <- factor(dat_cum$ctype,
#                          levels = c("Nationwide", "Temperate", "Continental", "Arid", "Tropical"))

dat_cum$race <- factor(dat_cum$race,
                             levels = c("white", "black", "asian", "hispanic", "native", "other"),
                             labels = c("White", "Black", "Asian/PI", "Hispanic", "AI/AN", "Other"))

# plot
pdf(paste0(dir_figs, "race_cumulative.pdf"), height = 3.5, width = 4.5)
dat_cum |>
  ggplot(aes(x = race, y = or, 
             #color = race, group = race,
             ymin = or_low, ymax = or_high)) +
  geom_hline(yintercept = 1, linetype = 2, linewidth = 0.5, color = "gray50") +
  geom_errorbar(linewidth = 0.7, width = 0, color = "orangered") +
  geom_point(size = 2, color = "orangered") +
  #scale_color_brewer(palette = "Dark2") +
  #coord_cartesian(ylim = c(0, 2.5)) +
  theme_light(base_size = 12) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    #axis.ticks.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.line = element_line(),
    #axis.ticks = element_line(),
    #panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(color = "black")
  ) +
  #facet_grid(~ ctype) +
  labs(x = "", 
       y = "Odds ratio", 
       color = "", 
       fill = "")
dev.off()

# get in a table
table_cum <- dat_cum |>
  mutate(or = paste0(round(or, 3), " (", round(or_low, 3), ", ", round(or_high, 3), ")")) |>
  select(race, or)



########################################################################
####################### lag plots ############################
########################################################################

# select percentile of interest
pctile <- 990

# nationwide
dat_lag <- rbindlist(lapply(names(cp_list), function(r) {
  data.frame(race = r,
             lag = 0:14,
             fit = cp_list[[r]][["Nationwide"]]$matfit[pctile, ],
             se = cp_list[[r]][["Nationwide"]]$matse[pctile, ])
}))

dat_lag$race <- factor(dat_lag$race,
                       levels = c("white", "black", "asian", "hispanic", "native", "other"),
                       labels = c("White", "Black", "Asian/PI", "Hispanic", "AI/AN", "Other"))

# new columns for OR and 95% CI
dat_lag <- dat_lag %>%
  mutate(or = exp(fit),
         or_low = exp(fit - 1.96 * se),
         or_high = exp(fit + 1.96 * se))

pdf(paste0(dir_figs, "race_lag.pdf"), height = 3.5, width = 8)
dat_lag %>%
  ggplot(aes(x = lag, y = or)) +
  geom_hline(yintercept = 1, col = "gray50", lty = 2) +
  geom_line(col = "orangered") +
  geom_ribbon(aes(ymin = or_low, ymax = or_high), fill = "orangered", alpha = 0.3) +
  labs(#title = "Figure 2(a). Lag-response relationship for one day of exposure to the 99th versus the 50th percentile of heat index \ndistributions.",
       x = "Lag days (days after exposure)",
       y = "Odds ratio") +
  facet_grid(~race) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = scales::pretty_breaks()) +
  #scale_fill_manual(values = ctype_cols) +
  #scale_color_manual(values = ctype_cols) +
  #coord_cartesian(ylim = c(0.95, 1.04)) +
  theme_light() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(color = "black"),
        plot.title = element_text(size = 10),
        plot.margin = margin(10, 10, 25, 10)) # extra space below
dev.off()
