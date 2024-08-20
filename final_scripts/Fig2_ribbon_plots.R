
# libraries
library(data.table)
library(fst)
library(ggplot2)
library(tidyverse)
library(gridExtra)

# load data
dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
data <- read_fst(paste0(dir_data, 'data_complete_climate.fst'), as.data.table = TRUE)

# path to new figures
dir_figs <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/figures/"


########################################################################
########################### REVISED FIGURE 2 ########################### 
########################################################################

# based on old figure 1 (created in main_plots_NC_C.R)

# load models from main analysis
load(paste0(dir_data, 'aic_models_main.rda'))

#select percentiles of interest, here we use the 75th, 90th, 95th, & 99th, but change them as needed
pctile <- 990

# empty lists to fill with plots
plot_list <- list()
plot_list_cum <- list()



# gg object to add to all plots (for formatting)
gg_format <- list(
  geom_hline(yintercept = 1, lty = 2, col = "gray60"),
  scale_x_continuous(expand = c(0, 0)),
  scale_y_continuous(expand = c(0, 0),
                     lim = c(0.95, 1.07),
                     breaks = scales::pretty_breaks()),
  theme_light()
)
gg_format_cum <- list(
  geom_hline(yintercept = 1, lty = 2, col = "gray60"),
  scale_x_continuous(expand = c(0, 0)),
  scale_y_continuous(expand = c(0, 0),
                     lim = c(0.77, 1.23),
                     breaks = scales::pretty_breaks()),
  theme_light()
)


#TEMPERATE
##Temperate non cumulative

# Iterate over each value and create a ggplot
main_line <- exp(cp$Temperate$matfit[pctile, ])
confidence_interval <- 1.96 * cp$Temperate$matse[pctile, ]

plot_data <- data.frame(
  x = 1:length(main_line),
  y = main_line,
  lower = exp(cp$Temperate$matfit[pctile, ] - confidence_interval),
  upper = exp(cp$Temperate$matfit[pctile, ] + confidence_interval),
  value = as.character(pctile)
)

plot <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightgreen") +
  labs(title = paste("Temperate"),
       x = "Lag Days",
       y = "OR") +
  gg_format


# Store the ggplot object in the list
plot_list[["temp"]] <- plot



##Temperate Cumulative

# Iterate over each value and create a ggplot
main_line <- exp(cp$Temperate$cumfit[pctile, ])
confidence_interval <- 1.96 * cp$Temperate$cumse[pctile, ]

plot_data <- data.frame(
  x = 1:length(main_line),
  y = main_line,
  lower = exp(cp$Temperate$cumfit[pctile, ] - confidence_interval),
  upper = exp(cp$Temperate$cumfit[pctile, ] + confidence_interval),
  value = as.character(pctile)
)

plot <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightgreen") +
  labs(title = paste("Temperate (cumulative)"),
       x = "Lag Days",
       y = "OR") +
  gg_format_cum

# Store the ggplot object in the list
plot_list_cum[["temp"]] <- plot




#TROPICAL

# Iterate over each value and create a ggplot
main_line <- exp(cp$Tropical$matfit[pctile, ])
confidence_interval <- 1.96 * cp$Tropical$matse[pctile, ]

plot_data <- data.frame(
  x = 1:length(main_line),
  y = main_line,
  lower = exp(cp$Tropical$matfit[pctile, ] - confidence_interval),
  upper = exp(cp$Tropical$matfit[pctile, ] + confidence_interval),
  value = as.character(pctile)
)

plot <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
  labs(title = paste("Tropical"),
       x = "Lag Days",
       y = "OR") +
  gg_format

# Store the ggplot object in the list
plot_list[["trop"]] <- plot



##Tropical Cumulative plots


# Iterate over each value and create a ggplot
main_line <- exp(cp$Tropical$cumfit[pctile, ])
confidence_interval <- 1.96 * cp$Tropical$cumse[pctile, ]

plot_data <- data.frame(
  x = 1:length(main_line),
  y = main_line,
  lower = exp(cp$Tropical$cumfit[pctile, ] - confidence_interval),
  upper = exp(cp$Tropical$cumfit[pctile, ] + confidence_interval),
  value = as.character(pctile)
)

plot <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
  labs(title = paste("Tropical (cumulative)"),
       x = "Lag Days",
       y = "OR") +
  gg_format_cum

# Store the ggplot object in the list
plot_list_cum[["trop"]] <- plot



#ARID

##Arid plots non_cumulative


# Iterate over each value and create a ggplot
main_line <- exp(cp$Arid$matfit[pctile, ])
confidence_interval <- 1.96 * cp$Arid$matse[pctile, ]

plot_data <- data.frame(
  x = 1:length(main_line),
  y = main_line,
  lower = exp(cp$Arid$matfit[pctile, ] - confidence_interval),
  upper = exp(cp$Arid$matfit[pctile, ] + confidence_interval),
  value = as.character(pctile)
)

plot <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "plum2") +
  labs(title = paste("Arid"),
       x = "Lag Days",
       y = "OR") +
  gg_format

# Store the ggplot object in the list
plot_list[["arid"]] <- plot


##arid cumulative

# Iterate over each value and create a ggplot
main_line <- exp(cp$Arid$cumfit[pctile, ])
confidence_interval <- 1.96 * cp$Arid$cumse[pctile, ]

plot_data <- data.frame(
  x = 1:length(main_line),
  y = main_line,
  lower = exp(cp$Arid$cumfit[pctile, ] - confidence_interval),
  upper = exp(cp$Arid$cumfit[pctile, ] + confidence_interval),
  value = as.character(pctile)
)

plot <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "plum2") +
  labs(title = paste("Arid (cumulative)"),
       x = "Lag Days",
       y = "OR") +
  gg_format_cum

# Store the ggplot object in the list
plot_list_cum[["arid"]] <- plot



#CONTINENTAL
##continental non-cumulative

# Iterate over each value and create a ggplot
main_line <- exp(cp$Continental$matfit[pctile, ])
confidence_interval <- 1.96 * cp$Continental$matse[pctile, ]

plot_data <- data.frame(
  x = 1:length(main_line),
  y = main_line,
  lower = exp(cp$Continental$matfit[pctile, ] - confidence_interval),
  upper = exp(cp$Continental$matfit[pctile, ] + confidence_interval),
  value = as.character(pctile)
)

plot <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkorchid") +
  labs(title = paste("Continental"),
       x = "Lag Days",
       y = "OR") +
  gg_format

# Store the ggplot object in the list
plot_list[["cont"]] <- plot




##Continental cumulative

# Iterate over each value and create a ggplot
main_line <- exp(cp$Continental$cumfit[pctile, ])
confidence_interval <- 1.96 * cp$Continental$cumse[pctile, ]

plot_data <- data.frame(
  x = 1:length(main_line),
  y = main_line,
  lower = exp(cp$Continental$cumfit[pctile, ] - confidence_interval),
  upper = exp(cp$Continental$cumfit[pctile, ] + confidence_interval),
  value = as.character(pctile)
)

plot <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkorchid") +
  labs(title = paste("Continental (cumulative)"),
       x = "Lag Days",
       y = "OR") +
  gg_format_cum

# Store the ggplot object in the list
plot_list_cum[["cont"]] <- plot



#NATIONAL

##national plot non-cumulative

# Iterate over each value and create a ggplot
main_line <- exp(cp_all$matfit[pctile, ])
confidence_interval <- 1.96 * cp_all$matse[pctile, ]

plot_data <- data.frame(
  x = 1:length(main_line),
  y = main_line,
  lower = exp(cp_all$matfit[pctile, ] - confidence_interval),
  upper = exp(cp_all$matfit[pctile, ] + confidence_interval),
  value = as.character(pctile)
)

plot <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "orangered") +
  labs(title = paste("National"),
       x = "Lag Days",
       y = "OR") +
  gg_format

# Store the ggplot object in the list
plot_list[["nat"]] <- plot


##National cumulative

# Iterate over each value and create a ggplot
main_line <- exp(cp_all$cumfit[pctile, ])
confidence_interval <- 1.96 * cp_all$cumse[pctile, ]

plot_data <- data.frame(
  x = 1:length(main_line),
  y = main_line,
  lower = exp(cp_all$cumfit[pctile, ] - confidence_interval),
  upper = exp(cp_all$cumfit[pctile, ] + confidence_interval),
  value = as.character(pctile)
)

plot <- ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "orangered") +
  labs(title = paste("National (cumulative)"),
       x = "Lag Days",
       y = "OR") +
  gg_format_cum

# Store the ggplot object in the list
plot_list_cum[["nat"]] <- plot



# arrange all the plots and save figs as PDF
pdf(paste0(dir_figs, "revised_fig2_noncum.pdf"), height = 6, width = 9)
do.call(grid.arrange, c(plot_list, ncol = 3))
dev.off()

pdf(paste0(dir_figs, "revised_fig2_cum.pdf"), height = 6, width = 9)
do.call(grid.arrange, c(plot_list_cum, ncol = 3))
dev.off()




