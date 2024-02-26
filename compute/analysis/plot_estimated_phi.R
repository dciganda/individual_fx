plot_estimated_phi <- function(param, save = F,
                     pch = 19, cex = 0.2, each_x = 2,
                     each_y = 0.04, alpha = 0.7,
                     margin_y = 0.14, margin_x = 0.18,
                     x_coord = 40, y_coord = 0.45,
                     grid_rows = 15){
  
  add_empty_plot_with_grid <- function(x_data, y_data, x_label_text,
                                       y_label_text, grid_rows = grid_rows,
                                       margin_y = margin_y,
                                       margin_x = margin_x){
    
    draw_grid <-function(x_coords, y_coords){
      
      for(i in 1:length(x_coords)){
        
        # we draw the i_th vertical line
        segments(x0 = x_coords[i], x1 = x_coords[i],
                 y0 = min(y_coords), y1 = max(y_coords), 
                 col=scales::alpha(rgb(0,0,0), 0.1), lwd = 0.5)
      }
      for(i in 1:length(y_coords)){
        # we draw the i_th horizontal line
        segments(x0 = min(x_coords), x1 = max(x_coords),
                 y0 = y_coords[i], y1 = y_coords[i],
                 col=scales::alpha(rgb(0,0,0), 0.1), lwd = 0.5)
      }  
      
    }
    
    xmargin <- (max(x_data, na.rm = T)-min(x_data, na.rm = T))*margin_x
    ymargin <- (max(y_data, na.rm = T)-min(y_data, na.rm = T))*margin_y + 0.025 # Add margin for intervals
    
    new_x <<- round(seq(min(x_data, na.rm = T)-xmargin,
                        max(x_data, na.rm = T)+xmargin, by = each_x),0)
    new_y <<- round(seq(0,
                        max(y_data, na.rm = T)+ymargin, by = each_y),2)
    
    plot(x_data, y_data, xlab=x_label_text, ylab= y_label_text,
         xlim = c(min(new_x),max(new_x)),
         ylim = c(min(new_y),max(new_y)),
         type = 'n', bty = "n", axes = F)
    
    # specify x-axis interval
    axis(side=1, at=round(new_x,0))
    
    ticks <- round(new_y,1)
    axis(side=2, at=ticks)
    
    draw_grid(new_x, new_y)
    
    par(new=TRUE)
  }
  


  add_empty_plot_with_grid(x_data = 10:49,
                           y_data =  seq(0, 0.28, length.out = 40),
                           x_label_text = "Age",
                           y_label_text =  "f(x)",
                           margin_y = margin_y,
                           margin_x = margin_x)

  # phi model
  phi_age <- function(age, phi1, phi2){
    # scale
    t <- (age-10)/(50-10)
    # compute
    phi <- phi1 * (3*t^3-6*t^2+3*t) + phi2 * (-3*t^3+3*t^2)
    return(phi)
  }
  
  # Compute phi for estimated phi1 and phi2
  phi <- lapply(param, function(x) mapply(phi_age,10:50, x[1], x[2]))
  
  mean_vals <- lapply(phi, function(x) apply(x, 2, mean, na.rm = T))
  int_inf <- lapply(phi, function(x) apply(x, 2, quantile, probs = 0.01/2, na.rm = T))
  int_sup <- lapply(phi, function(x) apply(x, 2, quantile, probs = 1-0.01/2, na.rm = T))
  
  # Adding estimated phi to plot
  obs_col <- rgb(131/255,75/255,159/255)
  sim_col <- "firebrick2" #rgb(0.93,0.17,0.17, alpha = 0.8) #rgb(193/255,78/255,78/255)
  
  points(10:50, mean_vals[[1]], col="firebrick2", pch = 1, cex = 1)
  lines(10:50, mean_vals[[1]], col="firebrick2", lwd = 1, lty = 1)
  
  points(10:50, mean_vals[[2]], col="firebrick2", pch = 2, cex = 0.8)
  lines(10:50, mean_vals[[2]], col="firebrick2", lwd = 1, lty = 1)
  
  points(10:50, mean_vals[[3]], col="firebrick2", pch = 5, cex = 0.8)
  lines(10:50, mean_vals[[3]], col="firebrick2", lwd = 1, lty = 1)
  
  polygon(x = c(10:50, rev(10:50)),
          y = c(as.numeric(int_inf[[1]]),rev(as.numeric(int_sup[[1]]))),
          col = adjustcolor("firebrick2", alpha.f = 0.20), border = NA)
  polygon(x = c(10:50, rev(10:50)),
          y = c(as.numeric(int_inf[[2]]),rev(as.numeric(int_sup[[2]]))),
          col = adjustcolor("firebrick2", alpha.f = 0.20), border = NA)
  polygon(x = c(10:50, rev(10:50)),
          y = c(as.numeric(int_inf[[3]]),rev(as.numeric(int_sup[[3]]))),
          col = adjustcolor("firebrick2", alpha.f = 0.20), border = NA)
  
  # Legend
  op <- par(family = "sans")
  
  legend(legend = c("Hutterites", "French Cohorts","French Canadian Cohorts"),
         lwd = c(1,1,1), col = "firebrick2", lty = c(1,1,1),
         pch = c(1, 2, 5),
         cex=1, bty = "n", y.intersp = 1.2,
         x = 37,
         y = 0.25)
  
  
  if(save){
    save_path <- file.path("..", "write", "plots")
    par(mar = c(4, 4, 0.1, 0.1))
    p <- recordPlot()
    pdf(file.path(save_path, paste0("estimated","_","phi",".pdf")), width=7, height=7) 
    print(p)
    dev.off()
  }
}

  



