  plot_posterior <- function(param, name, pop, p_post = F, n_breaks = 20,
                             margin = 0.05, grid_rows = 25, 
                             n_round = 10, val_col = "turquoise",
                             save = F){
    
    
    draw_grid <- function(x_coords, y_coords, grid_rows){
      
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
    
    save_plot <- function(name, pop){
      save_path <- file.path("..", "write", "plots")
      p <- recordPlot()
      pdf(file.path(save_path, paste0(name,"_",pop,".pdf")), width=7, height=7) 
      print(p)
      dev.off()
    }
    
    # browser()
    
    breaks <- seq(min(param), max(param), length.out = n_breaks)  
    
    bks <- findInterval(param, breaks[1:(n_breaks-1)])
    cts <- as.data.frame(table(bks))
    counts <- merge(data.frame(bks = 1:(n_breaks-1)), cts, by = "bks", all.x = T) 
    counts$Freq [is.na(counts$Freq)] <- 0
    
    aux <- (max(param)-min(param)) * margin
    min <- round(round(min(param), n_round) - aux, n_round)
    max <- round((ceiling(max(param) * 10**n_round) / 10**n_round) + aux,n_round) 
    median_val <- median(param)  
    
    # Histogram is visualized as proportions to fit with density estimation
    counts <- counts$Freq/sum(counts$Freq)/diff(breaks)
    
    x_coords <- seq(min, max, length.out = grid_rows)
    y_coords <- seq(0, max(counts) + 0.01, length.out = grid_rows)
    
    plot(0, type = "l", 
         xlim = range(min, max), 
         ylim = c(0, max(counts) + 0.01),
         axes = FALSE, 
         ylab= "", xlab = "")
    
    # Add X-axis
    axis(1, at = round(seq(min, max, ((max-min) / (n_breaks + 4))), 4))
    
    # Add Y-axis
    axis(2, at = round(seq(0, (max(counts) + 0.01 ), length.out = grid_rows), 4)) # Adjusted y-axis and margin added to fit plot
    
    par(new = TRUE)
    draw_grid(x_coords, y_coords, grid_rows)
    par(new = TRUE)
    
    for (j in 2:n_breaks) {
      rect(breaks[j-1], 
           0, 
           breaks[j], 
           counts[j-1], 
           col = rgb(0.93,0.17,0.17, alpha = 0.8), #firebrick2
           border = NA)
    }
    
    segments(x0 = median_val, 
             x1 = median_val,
             y0 = 0, 
             y1 = max(counts)+0.005,
             col = val_col, 
             lwd = 4, 
             lty="dashed") #3  
    
    # Add density estimation
    # Sheater-Jones method to choose bandwidth
    # Epanechnikov kernel (most common)
    kde_param <<- density(x = param, bw = "SJ", kernel = "epanechnikov")
    scale_coeff <- max(counts)/max(kde_param$y)
    
    # Adjust to scale at histogram proportions
    kde_param$y <- kde_param$y*scale_coeff
    
    # Adjust to grid
    kde_param$y[kde_param$x < min] <- NA
    kde_param$y[kde_param$x > max] <- NA
    
    # Draw line of density in plot (its look like this beacause of the bandwidth)
    lines(kde_param$x, kde_param$y, col = "firebrick", lwd = 2, lty = 2)
    
    
    legend(x = median_val + (max(param) - min(param))/6,
           y = max(counts),
           legend = c("Posterior\nmedian"),
           col = val_col,
           lty = "dashed",
           lwd = 4,
           cex = 0.9,
           bty = "n",        #legend box type (bty = "n" removes the border)
           seg.len = 0.8) #the length of lines drawn to illustrate lty and/or lwd
    
    
    if(save){
      print(name)
      print(pop)
      save_plot(name, pop)
      
    }
    
    
  }
  
