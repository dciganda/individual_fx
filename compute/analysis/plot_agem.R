plot_agem <- function(obs_data, estimated_mu, estimated_sd, xlim,
                      pop, n_breaks = 50,
                      margin = 0.05, grid_rows = 11, 
                      n_round = 10, val_col = "turquoise",
                      xlimit, legend_x, legend_y, save = F){
  
  draw_grid <- function(x_coords, y_coords, grid_rows){
    
    suppressWarnings( plot(x_coords, y_coords, type = "n", axes = F, ann = F) )
    
    for(i in 1:grid_rows){
      
      # we draw the i_th vertical line
      segments(x0 = x_coords[i], 
               x1 = x_coords[i],
               y0 = min(y_coords), 
               y1 = max(y_coords), 
               col = rgb(0,0,0,alpha= 0.1))
      
      # we draw the i_th horizontal line
      segments(x0 = min(x_coords), 
               x1 = max(x_coords),
               y0 = y_coords[i], 
               y1 = y_coords[i], 
               col = rgb(0,0,0, alpha = 0.1))
      
      
    }
  }
  
  save_plot <- function(pop){
    save_path <- file.path("..", "write", "plots")
    p <- recordPlot()
    pdf(file.path(save_path, paste0("agem_",pop,".pdf")), width=7, height=7) 
    print(p)
    dev.off()
  }

  
  breaks <- seq(min(obs_data), max(obs_data), length.out = n_breaks)  
  
  bks <- findInterval(obs_data, breaks[1:(n_breaks-1)])
  cts <- as.data.frame(table(bks))
  counts <- merge(data.frame(bks = 1:(n_breaks-1)), cts, by = "bks", all.x = T) 
  counts$Freq [is.na(counts$Freq)] <- 0
  
  aux <- (max(obs_data)-min(obs_data)) * margin
  min <- round(round(min(obs_data), n_round) - aux, n_round)
  max <- round((ceiling(max(obs_data) * 10**n_round) / 10**n_round) + aux,n_round) 
  median_val <- median(obs_data)  
  
  # Histogram is visualized as proportions to fit with density estimation
  # bin_width <- 
  counts <- counts$Freq/sum(counts$Freq)/diff(breaks)
  
  # Draw density of estimated lognormal
  x <- seq(10, 49, length.out = 1000)  # Adjust as needed
  
  meanlog <- log(estimated_mu^2 / sqrt(estimated_sd^2 + estimated_mu^2))
  sdlog <- sqrt(log(1 + (estimated_sd^2 / estimated_mu^2)))
  
  # Calculate density values
  density_values <- dlnorm(x, meanlog = meanlog, sdlog = sdlog)
  
  ymax <- max(max(density_values), max(counts))
  # Define firebrick color with transparency
  firebrick_with_transparency <- rgb(0.93,0.17,0.17, alpha = 0.5)

  
  x_coords <- seq(0,50, length.out = grid_rows)
  y_coords <- seq(0, ymax + 0.01, length.out = grid_rows)

  
  
  # par(new = TRUE)
  draw_grid(x_coords, y_coords, grid_rows)
  # par(new = TRUE)
    
  suppressWarnings(
    lines(x[100:xlimit],density_values[100:xlimit], type = "l", 
       xlim = xlim,
       ylim = c(0, ymax),
       axes = FALSE, 
       col = firebrick_with_transparency,
       ylab= "", xlab = "")
  )
  
  # Add X-axis
  # axis(1, at = round(seq(0, 50, ((50-0) / (length(x_coords) - 1))), 2))
  axis(1, at = pretty(x_coords, n = 10))
  
  
  # Add Y-axis
  axis(2, at = seq(0, max(y_coords), y_coords[2] - y_coords[1])) # Adjusted y-axis and margin added to fit plot
  # axis(2, at = pretty(y_coords, n = 5))

  
  # Use polygon to color the area under the curve
  polygon(c(x, rev(x)), c(density_values, rep(0, length(x))), col = firebrick_with_transparency, border=NA)
  
  
  for (j in 2:n_breaks) {
    rect(breaks[j-1], 
         0, 
         breaks[j], 
         counts[j-1], 
         col = rgb(131/255,75/255,159/255, alpha = 0.8), #firebrick2
         border = NA)
  }
  
  
  # legend
  # Define the colors for the legend
  empirical_color <- rgb(131/255, 75/255, 159/255, alpha = 0.8) # Color used for the histogram
  theoretical_color <- rgb(0.93, 0.17, 0.17, alpha = 0.5) # Color used for the polygon
  

  # Add the legend
  legend(x = legend_x, y = legend_y,
         legend = c("Observed", "Estimated"),
         fill = c(empirical_color, theoretical_color),
         cex = 1.5, # Adjust font size as needed
         border = NA, # No box around the legend
         bty = "n",
         y.intersp = 1.5) # Adjust spacing between lines
  
  
  if(save){
    save_plot(pop)
    
  }
  
  
}

