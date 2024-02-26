plot_fit <- function(pred_post, obs, pop, save = F,
                     pch = 19, cex = 0.2, each_x = 2,
                     each_y = 0.05, alpha = 0.7,
                     margin_y = 0.18, margin_x = 0.18,
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
  
  median_val <- apply(pred_post, 2, quantile, probs = 0.5, na.rm = T)
  int_inf <- apply(pred_post, 2, quantile, probs = 0.01/2, na.rm = T)
  int_sup <- apply(pred_post, 2, quantile, probs = 1-0.01/2, na.rm = T)
  
  add_empty_plot_with_grid(x_data = 10:49,
                           y_data =  int_sup,
                           x_label_text = "Age",
                           y_label_text =  "f(x)",
                           margin_y = margin_y,
                           margin_x = margin_x)

  obs_col <- rgb(131/255,75/255,159/255)
  sim_col <- "firebrick2" #rgb(0.93,0.17,0.17, alpha = 0.8) #rgb(193/255,78/255,78/255)
  
  # Adding observed data as points to plot
  points(obs$age, obs$fx, col=scales::alpha(obs_col,alpha), pch = pch, lwd = 2)
  
  
  # to draw intervals with polygon, there is a special way to specify the parameters:
  #  - the x-axis must be passed 2 times, one normal and the other inverted (with inv).
  #  - the y-axis first the upper edge and then the lower one inverted (with inv) 
  polygon(x = c(10:49, rev(10:49)),
          y = c(as.numeric(int_inf),rev(as.numeric(int_sup))),
          col = adjustcolor("firebrick2", alpha.f = 0.20), border = NA)
  
  
  points(10:49, median_val, pch = 19 , col = 'red4')
  
  # Legend
  op <- par(family = "sans")
  
  
  legend(legend = c("Observed", "Median of sim.","95% interval"),
         lwd = c(1,1,1), col = c(scales::alpha(obs_col, alpha), "red4",
                                 adjustcolor("firebrick2", alpha.f = 0.20)), lty = c(0,0,0),
         pch = c(16, 16, 15),
         cex=0.85, bty = "n", y.intersp = 1.2,
         x = x_coord,
         y = y_coord)
  
  
  if(save){
    save_path <- file.path("..", "write", "plots")
    par(mar = c(4, 4, 0.1, 0.1))
    p <- recordPlot()
    pdf(file.path(save_path, paste0("fit","_",pop,".pdf")), width=7, height=7) 
    print(p)
    dev.off()
  }
}

  



