plot_fecundability <- function(save = F,
                     pch = 19, cex = 0.2, each_x = 2,
                     each_y = 0.05, alpha = 0.7,
                     margin_y = 0, margin_x = 0.1,
                     x_coord = 40, y_coord = 0.45,
                     grid_rows = 100){
  
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
  phi_age <- function(age, phi1, phi2){
    # scale
    t <- (age-10*12)/(50*12-10*12)
    # compute
    phi <- phi1 * (3*t^3-6*t^2+3*t) + phi2 * (-3*t^3+3*t^2)
    return(phi)
  }
  
  # Define the basis functions
  B1 <- function(t) {
    3*t^3 - 6*t^2 + 3*t
  }
  
  B2 <- function(t) {
    -3*t^3 + 3*t^2
  }
  
  par(family="serif")
  add_empty_plot_with_grid(x_data = 10:49,
                           y_data =  seq(0, 0.5, length.out = 40),
                           x_label_text = "Age",
                           y_label_text =  expression(phi(x)),
                           margin_y = margin_y,
                           margin_x = margin_x)

  # Compute the values of the basis functions over the age range
  ages <- seq(10*12, 50*12, by=0.1)
  t_values <- (ages-10*12)/(50*12-10*12)
  B1_values <- B1(t_values)
  B2_values <- B2(t_values)
  
  # Create a vector with age values from 10*12 to 50*12 (since the function is set for months)
  alpha_value <- 0.8  # 50% transparent
  
  # Modify the lines calls to use colors with alpha
  lines(ages/12, B1_values, col=rgb(0, 0, 0, alpha_value), lwd=1.5, lty=2) 
  lines(ages/12, B2_values, col=rgb(0, 0, 0, alpha_value), lwd=1.5, lty=2) 
  
  text(x = 24, y = 0.465, labels = expression(B[1] == 3 * x[s] * (1 - x[s])^2), col="black", cex=0.8)
  text(x = 37, y = 0.465, labels = expression(B[2] == 3 * x[s]^2 * (1 - x[s])), col="black", cex=0.8)
  
  # Calculate phi values and plot lines
  phi_values1 <- phi_age(ages, 0.8, -0.05)
  lines(ages/12, phi_values1, col="firebrick", lwd=1.7, lty=1)
  
  text(x = 22.4, y = 0.39, labels = expression(phi[1] == 0.8), col = "firebrick", cex = 0.7)
  text(x = 23, y = 0.365, labels = expression(phi[2] == -0.05), col = "firebrick", cex = 0.7)
  
  phi_values2 <- phi_age(ages, -0.05, 0.5)
  lines(ages/12, phi_values2, col="firebrick", lwd=1.7, lty=1)
  
  text(x = 41.5, y = 0.25, labels = expression(phi[1] == -0.05), col = "firebrick", cex = 0.7)
  text(x = 41, y = 0.225, labels = expression(phi[2] == 0.5), col = "firebrick", cex = 0.7)
  

  
  if(save){
    save_path <- file.path("..", "write", "plots")
    par(mar = c(4, 4, 0.1, 0.1))
    p <- recordPlot()
    pdf(file.path(save_path, paste0("phi_model.pdf")), width=6, height=6) 
    print(p)
    dev.off()
  }
}

  



