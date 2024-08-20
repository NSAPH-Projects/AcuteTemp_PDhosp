library(data.table)
library(fst)
library(ggplot2)
library(survival)
library(dlnm)
library(ggrepel)
library(cowplot)

dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
data <- load(paste0(dir_data, 'aic_models_secondary.rda'))

##plots with 95% CIs
plot_values <- c(990, 950, 900, 750)
plot_list <- list()

##plots with 95% CIs

#TEMPERATE
##Temperate non cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_air$Temperate$matfit[value, ])
  confidence_interval <- 1.96 * cp_air$Temperate$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_air$Temperate$matfit[value, ] - confidence_interval),
    upper = exp(cp_air$Temperate$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightgreen") +
    labs(title = paste("TempAQI 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))


##Temperate Cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_air$Temperate$cumfit[value, ])
  confidence_interval <- 1.96 * cp_air$Temperate$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_air$Temperate$cumfit[value, ] - confidence_interval),
    upper = exp(cp_air$Temperate$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightgreen") +
    labs(title = paste("TempAQI Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))

##Tropical

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_air$Tropical$matfit[value, ])
  confidence_interval <- 1.96 * cp_air$Tropical$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_air$Tropical$matfit[value, ] - confidence_interval),
    upper = exp(cp_air$Tropical$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    labs(title = paste("TropAQI 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))

##Tropical Cumulative plots

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_air$Tropical$cumfit[value, ])
  confidence_interval <- 1.96 * cp_air$Tropical$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_air$Tropical$cumfit[value, ] - confidence_interval),
    upper = exp(cp_air$Tropical$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    labs(title = paste("TropAQI Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))

##Arid plots non_cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_air$Arid$matfit[value, ])
  confidence_interval <- 1.96 * cp_air$Arid$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_air$Arid$matfit[value, ] - confidence_interval),
    upper = exp(cp_air$Arid$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "plum2") +
    labs(title = paste("AridAQI 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))


##arid cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_air$Arid$cumfit[value, ])
  confidence_interval <- 1.96 * cp_air$Arid$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_air$Arid$cumfit[value, ] - confidence_interval),
    upper = exp(cp_air$Arid$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "plum2") +
    labs(title = paste("AridAQI Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))


##continental non-cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_air$Continental$matfit[value, ])
  confidence_interval <- 1.96 * cp_air$Continental$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_air$Continental$matfit[value, ] - confidence_interval),
    upper = exp(cp_air$Continental$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkorchid") +
    labs(title = paste("ContAQI 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))

##Continental cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_air$Continental$cumfit[value, ])
  confidence_interval <- 1.96 * cp_air$Continental$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_air$Continental$cumfit[value, ] - confidence_interval),
    upper = exp(cp_air$Continental$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkorchid") +
    labs(title = paste("ContAQI Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))

##national plot non-cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_all_air$matfit[value, ])
  confidence_interval <- 1.96 * cp_all_air$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_all_air$matfit[value, ] - confidence_interval),
    upper = exp(cp_all_air$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "orangered") +
    labs(title = paste("NatAQI 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))

##National cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_all_air$cumfit[value, ])
  confidence_interval <- 1.96 * cp_all_air$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_all_air$cumfit[value, ] - confidence_interval),
    upper = exp(cp_all_air$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "orangered") +
    labs(title = paste("NatAQI Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))
