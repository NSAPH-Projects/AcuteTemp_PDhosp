









##plotting models by climate subtype 

##Temperate Models @ 75th, 90th, 95th, & 99th percentiles 

climate_Temperate_plots <- function(cp, percentiles = c(990, 950, 900, 750)) {
  lapply(percentiles, function(value) {
    data_to_plot_Temperate <- exp(cp$Temperate$matfit[value,])
    
    cat("value Temperate data", value, ":\n")
    print(data_to_plot_Temperate)
    
    plot(data_to_plot_Temperate, type = 'l', main = paste("value Temperate plot", value),
         xlab = "Days of Lag", ylab = "OR")
  })
}

##cumulative plots

cumulative_Temperate_plots <- function(cp, percentiles = c(990, 950, 900, 750)) {
  lapply(percentiles, function(value) {
    data_to_plot_Temperate <- exp(cp$Temperate$cumfit[value,])
    
    cat("value Temperate data", value, ":\n")
    print(data_to_plot_Temperate)
    
    plot(0:14, data_to_plot_Temperate, type = 'l', main = paste("value Temperate plot", value),
         xlab = "Days of Lag", ylab = "OR")
  })
}



cumulative_Temperate_plots(cp)

##Tropical Models @ 75th, 90th, 95th, & 99th percentiles 

climate_Tropical_plots <- function(cp, percentiles = c(990, 950, 900, 750)) {
  lapply(percentiles, function(value) {
    data_to_plot_Tropical <- exp(cp$Tropical$matfit[value,])
    
    cat("value Tropical data", value, ":\n")
    print(data_to_plot_Tropical)
    
    plot(data_to_plot_Tropical, type = 'l', main = paste("value Tropical plot", value),
         xlab = "Days of Lag", ylab = "OR")
  })
}

climate_Tropical_plots(cp)

##cumulative tropical plots
cumulative_Tropical_plots <- function(cp, percentiles = c(990, 950, 900, 750)) {
  lapply(percentiles, function(value) {
    data_to_plot_Tropical <- exp(cp$Tropical$cumfit[value,])
    
    cat("value Tropical data", value, ":\n")
    print(data_to_plot_Tropical)
    
    plot(0:14, data_to_plot_Tropical, type = 'l', main = paste("value Tropical plot", value),
         xlab = "Days of Lag", ylab = "OR")
  })
}



cumulative_Tropical_plots(cp)

##Arid Models @ 75th, 90th, 95th, & 99th percentiles 

climate_Arid_plots <- function(cp, percentiles = c(990, 950, 900, 750)) {
  lapply(percentiles, function(value) {
    data_to_plot_Arid <- exp(cp$Arid$matfit[value,])
    
    cat("value Arid data", value, ":\n")
    print(data_to_plot_Arid)
    
    plot(data_to_plot_Arid, type = 'l', main = paste("value Arid plot", value),
         xlab = "Days of Lag", ylab = "OR")
  })
}

climate_Arid_plots(cp)

##cumulative Arid plots
cumulative_Arid_plots <- function(cp, percentiles = c(990, 950, 900, 750)) {
  lapply(percentiles, function(value) {
    data_to_plot_Arid <- exp(cp$Arid$cumfit[value,])
    
    cat("value Arid data", value, ":\n")
    print(data_to_plot_Arid)
    
    plot(0:14, data_to_plot_Arid, type = 'l', main = paste("value Arid plot", value),
         xlab = "Days of Lag", ylab = "OR")
  })
}



cumulative_Arid_plots(cp)


##Continental Models @ 75th, 90th, 95th, & 99th percentiles 

climate_Continental_plots <- function(cp, percentiles = c(990, 950, 900, 750)) {
  lapply(percentiles, function(value) {
    data_to_plot_Continental <- exp(cp$Continental$matfit[value,])
    
    cat("valueContinental data", value, ":\n")
    print(data_to_plot_Continental)
    
    plot(data_to_plot_Continental, type = 'l', main = paste("value Continentalplot", value),
         xlab = "Days of Lag", ylab = "OR")
  })
}

climate_Continental_plots(cp)

##cumulative Continental plots
cumulative_Continental_plots <- function(cp, percentiles = c(990, 950, 900, 750)) {
  lapply(percentiles, function(value) {
    data_to_plot_Continental <- exp(cp$Continental$cumfit[value,])
    
    cat("value Continental data", value, ":\n")
    print(data_to_plot_Continental)
    
    plot(0:14, data_to_plot_Continental, type = 'l', main = paste("value Continentalplot", value),
         xlab = "Days of Lag", ylab = "OR")
  })
}

cumulative_Continental_plots(cp)