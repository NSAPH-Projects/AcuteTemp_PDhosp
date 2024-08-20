library(dplyr)
library(data.table)
library(fst)
library(ggplot2)
library(survival)
library(dlnm)
library(ggrepel)
library(cowplot)

dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
data <- load(paste0(dir_data, 'aic_models_main.rda'))

##plots with 95% CIs

#select percentiles of interest, here we use the 75th, 90th, 95th, & 99th, but change them as needed
plot_values <- c(990, 950, 900, 750)
plot_list <- list()

#TEMPERATE
##Temperate non cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp$Temperate$matfit[value, ])
  confidence_interval <- 1.96 * cp$Temperate$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp$Temperate$matfit[value, ] - confidence_interval),
    upper = exp(cp$Temperate$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightgreen") +
    labs(title = paste("Temp 95% CI (", value, ")"),
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
  main_line <- exp(cp$Temperate$cumfit[value, ])
  confidence_interval <- 1.96 * cp$Temperate$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp$Temperate$cumfit[value, ] - confidence_interval),
    upper = exp(cp$Temperate$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightgreen") +
    labs(title = paste("Temp Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))



#TROPICAL

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp$Tropical$matfit[value, ])
  confidence_interval <- 1.96 * cp$Tropical$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp$Tropical$matfit[value, ] - confidence_interval),
    upper = exp(cp$Tropical$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    labs(title = paste("Trop 95% CI (", value, ")"),
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
  main_line <- exp(cp$Tropical$cumfit[value, ])
  confidence_interval <- 1.96 * cp$Tropical$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp$Tropical$cumfit[value, ] - confidence_interval),
    upper = exp(cp$Tropical$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    labs(title = paste("Trop Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))

#ARID

##Arid plots non_cumulative


# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp$Arid$matfit[value, ])
  confidence_interval <- 1.96 * cp$Arid$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp$Arid$matfit[value, ] - confidence_interval),
    upper = exp(cp$Arid$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "plum2") +
    labs(title = paste("Arid 95% CI (", value, ")"),
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
  main_line <- exp(cp$Arid$cumfit[value, ])
  confidence_interval <- 1.96 * cp$Arid$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp$Arid$cumfit[value, ] - confidence_interval),
    upper = exp(cp$Arid$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "plum2") +
    labs(title = paste("Arid Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))


#CONTINENTAL
##continental non-cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp$Continental$matfit[value, ])
  confidence_interval <- 1.96 * cp$Continental$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp$Continental$matfit[value, ] - confidence_interval),
    upper = exp(cp$Continental$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkorchid") +
    labs(title = paste("Cont 95% CI (", value, ")"),
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
  main_line <- exp(cp$Continental$cumfit[value, ])
  confidence_interval <- 1.96 * cp$Continental$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp$Continental$cumfit[value, ] - confidence_interval),
    upper = exp(cp$Continental$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkorchid") +
    labs(title = paste("Cont Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))

#NATIONAL

##national plot non-cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_all$matfit[value, ])
  confidence_interval <- 1.96 * cp_all$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_all$matfit[value, ] - confidence_interval),
    upper = exp(cp_all$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "orangered") +
    labs(title = paste("Nat 95% CI (", value, ")"),
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
  main_line <- exp(cp_all$cumfit[value, ])
  confidence_interval <- 1.96 * cp_all$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_all$cumfit[value, ] - confidence_interval),
    upper = exp(cp_all$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "orangered") +
    labs(title = paste("Nat Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))