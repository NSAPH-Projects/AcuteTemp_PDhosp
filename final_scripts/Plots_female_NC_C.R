library(data.table)
library(fst)
library(ggplot2)
library(survival)
library(dlnm)
library(ggrepel)
library(cowplot)

dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
data <- load(paste0(dir_data, 'aic_models_F.rda'))

##plots with 95% CIs
plot_values <- c(990, 950, 900, 750)
plot_list <- list()

#TEMPERATE
##Temperate non cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_F$Temperate$matfit[value, ])
  confidence_interval <- 1.96 * cp_F$Temperate$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_F$Temperate$matfit[value, ] - confidence_interval),
    upper = exp(cp_F$Temperate$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightgreen") +
    labs(title = paste("Temp(F) 95% CI (", value, ")"),
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
  main_line <- exp(cp_F$Temperate$cumfit[value, ])
  confidence_interval <- 1.96 * cp_F$Temperate$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_F$Temperate$cumfit[value, ] - confidence_interval),
    upper = exp(cp_F$Temperate$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightgreen") +
    labs(title = paste("Temp(F) Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))

#TROPICAL

##Tropical non cumulative plots 

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_F$Tropical$matfit[value, ])
  confidence_interval <- 1.96 * cp_F$Tropical$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_F$Tropical$matfit[value, ] - confidence_interval),
    upper = exp(cp_F$Tropical$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    labs(title = paste("Trop(F) 95% CI (", value, ")"),
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
  main_line <- exp(cp_F$Tropical$cumfit[value, ])
  confidence_interval <- 1.96 * cp_F$Tropical$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_F$Tropical$cumfit[value, ] - confidence_interval),
    upper = exp(cp_F$Tropical$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    labs(title = paste("Trop(F) Cumulative 95% CI (", value, ")"),
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
  main_line <- exp(cp_F$Arid$matfit[value, ])
  confidence_interval <- 1.96 * cp_F$Arid$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_F$Arid$matfit[value, ] - confidence_interval),
    upper = exp(cp_F$Arid$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "plum2") +
    labs(title = paste("Arid(F) 95% CI (", value, ")"),
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
  main_line <- exp(cp_F$Arid$cumfit[value, ])
  confidence_interval <- 1.96 * cp_F$Arid$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_F$Arid$cumfit[value, ] - confidence_interval),
    upper = exp(cp_F$Arid$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "plum2") +
    labs(title = paste("Arid(F) Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))

#CONTINENTAL
##continental noncumulative plots

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_F$Continental$matfit[value, ])
  confidence_interval <- 1.96 * cp_F$Continental$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_F$Continental$matfit[value, ] - confidence_interval),
    upper = exp(cp_F$Continental$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkorchid") +
    labs(title = paste("Cont(F) 95% CI (", value, ")"),
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
  main_line <- exp(cp_F$Continental$cumfit[value, ])
  confidence_interval <- 1.96 * cp_F$Continental$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_F$Continental$cumfit[value, ] - confidence_interval),
    upper = exp(cp_F$Continental$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkorchid") +
    labs(title = paste("Cont(F) Cumulative 95% CI (", value, ")"),
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
  main_line <- exp(cp_all_F$matfit[value, ])
  confidence_interval <- 1.96 * cp_all_F$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_all_F$matfit[value, ] - confidence_interval),
    upper = exp(cp_all_F$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "orangered") +
    labs(title = paste("Nat(F) 95% CI (", value, ")"),
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
  main_line <- exp(cp_all_F$cumfit[value, ])
  confidence_interval <- 1.96 * cp_all_F$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_all_F$cumfit[value, ] - confidence_interval),
    upper = exp(cp_all_F$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "orangered") +
    labs(title = paste("Nat(F) Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))

