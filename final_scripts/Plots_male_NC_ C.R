library(dplyr)
library(data.table)
library(fst)
library(ggplot2)
library(survival)
library(dlnm)
library(ggrepel)
library(cowplot)

dir_data <- "/n/dominici_nsaph_l3/Lab/projects/cdinehart_PDhosp_temperature/data/"
data <- load(paste0(dir_data, 'aic_models_M.rda'))


##plots with 95% CIs
#select percentiles of interest

plot_values <- c(990, 950, 900, 750)
plot_list <- list()

#TEMPERATE

##Temperate non cumulative

# Iterate over each value and create a ggplot
for (value in plot_values) {
  main_line <- exp(cp_M$Temperate$matfit[value, ])
  confidence_interval <- 1.96 * cp_M$Temperate$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_M$Temperate$matfit[value, ] - confidence_interval),
    upper = exp(cp_M$Temperate$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightgreen") +
    labs(title = paste("Temp(M) 95% CI (", value, ")"),
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
  main_line <- exp(cp_M$Temperate$cumfit[value, ])
  confidence_interval <- 1.96 * cp_M$Temperate$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_M$Temperate$cumfit[value, ] - confidence_interval),
    upper = exp(cp_M$Temperate$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "lightgreen") +
    labs(title = paste("Temp(M) Cumulative 95% CI (", value, ")"),
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
  main_line <- exp(cp_M$Tropical$matfit[value, ])
  confidence_interval <- 1.96 * cp_M$Tropical$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_M$Tropical$matfit[value, ] - confidence_interval),
    upper = exp(cp_M$Tropical$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    labs(title = paste("Trop(M) 95% CI (", value, ")"),
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
  main_line <- exp(cp_M$Tropical$cumfit[value, ])
  confidence_interval <- 1.96 * cp_M$Tropical$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_M$Tropical$cumfit[value, ] - confidence_interval),
    upper = exp(cp_M$Tropical$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "skyblue") +
    labs(title = paste("Trop(M) Cumulative 95% CI (", value, ")"),
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
  main_line <- exp(cp_M$Arid$matfit[value, ])
  confidence_interval <- 1.96 * cp_M$Arid$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_M$Arid$matfit[value, ] - confidence_interval),
    upper = exp(cp_M$Arid$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "plum2") +
    labs(title = paste("Arid(M) 95% CI (", value, ")"),
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
  main_line <- exp(cp_M$Arid$cumfit[value, ])
  confidence_interval <- 1.96 * cp_M$Arid$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_M$Arid$cumfit[value, ] - confidence_interval),
    upper = exp(cp_M$Arid$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "plum2") +
    labs(title = paste("Arid(M) Cumulative 95% CI (", value, ")"),
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
  main_line <- exp(cp_M$Continental$matfit[value, ])
  confidence_interval <- 1.96 * cp_M$Continental$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_M$Continental$matfit[value, ] - confidence_interval),
    upper = exp(cp_M$Continental$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkorchid") +
    labs(title = paste("Cont(M) 95% CI (", value, ")"),
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
  main_line <- exp(cp_M$Continental$cumfit[value, ])
  confidence_interval <- 1.96 * cp_M$Continental$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_M$Continental$cumfit[value, ] - confidence_interval),
    upper = exp(cp_M$Continental$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "darkorchid") +
    labs(title = paste("Cont(M) Cumulative 95% CI (", value, ")"),
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
  main_line <- exp(cp_all_M$matfit[value, ])
  confidence_interval <- 1.96 * cp_all_M$matse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_all_M$matfit[value, ] - confidence_interval),
    upper = exp(cp_all_M$matfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "orangered") +
    labs(title = paste("Nat(M) 95% CI (", value, ")"),
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
  main_line <- exp(cp_all_M$cumfit[value, ])
  confidence_interval <- 1.96 * cp_all_M$cumse[value, ]
  
  plot_data <- data.frame(
    x = 1:length(main_line),
    y = main_line,
    lower = exp(cp_all_M$cumfit[value, ] - confidence_interval),
    upper = exp(cp_all_M$cumfit[value, ] + confidence_interval),
    value = as.character(value)
  )
  
  plot <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "orangered") +
    labs(title = paste("Nat(M) Cumulative 95% CI (", value, ")"),
         x = "Lag Days",
         y = "OR")
  
  # Store the ggplot object in the list
  plot_list[[as.character(value)]] <- plot
}

# Combine and arrange the plots side by side
plot_grid(plotlist = plot_list, ncol = length(plot_values))
