plot_cval <- function(cval_object, i, save = F,
                      pch = 21, cex = 1.2, each_x = 1,
                      each_y = 1, alpha = 0.6,
                      margin_y = 0.2, margin_x = 0.2,
                      x_coord = 20, y_coord = 20, # for legend
                      grid_rows = 20){
  
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
    ymargin <- (max(y_data, na.rm = T)-min(y_data, na.rm = T))*margin_y 
    
    new_x <<- round(seq(min(x_data, na.rm = T)-xmargin,
                        max(x_data, na.rm = T)+xmargin, length.out = grid_rows),4)
    new_y <<- round(seq(min(y_data, na.rm = T)-ymargin,
                        max(y_data, na.rm = T)+ymargin, length.out = grid_rows),4)
    
    plot(x_data, y_data, xlab=x_label_text, ylab= y_label_text,
         xlim = c(min(new_x),max(new_x)),
         ylim = c(min(new_y),max(new_y)),
         type = 'n', bty = "n", axes = F,  
         main = "") # main added to identify parameter
    
    # specify x-axis interval (its the same for y-axis)
    axis_adjust_x <- pretty(new_x) 
    axis_adjust_y <- pretty(new_y) 
    
      # adjustment so that the x-axis do not cross the y-axis
      axis(side=1, at= axis_adjust_x, tick = F) # pretty axis labels
      axis(side=1, at=c(min(new_x),max(new_x)), labels = F) # axis line to fit the grid
      axis(side=1, at= axis_adjust_x[c(-1,-length(axis_adjust_x))]) # avoid limits of pretty axis
      
      
      axis(side=2, at= axis_adjust_y, tick = F) # pretty axis labels
      axis(side=2, at=c(min(new_y),max(new_y)), labels = F) # axis line to fit the grid
      axis(side=2, at= axis_adjust_y[c(-1,-length(axis_adjust_x))]) # avoid limits of pretty axis
      

    draw_grid(new_x, new_y)
    
    par(new=TRUE)
  }
  
  # loop of all parameters of the cval object (the output of the cv4abc)
  for(i in 1:length(cval_object$true)){
    
    add_empty_plot_with_grid(x_data = cval_object$true[[i]],
                             y_data =  cval_object$estim[,i],
                             x_label_text = expression(theta^"*"),
                             y_label_text =  expression(theta),
                             margin_y = margin_y,
                             margin_x = margin_x, 
                             grid_rows = grid_rows)
    
    # Diagonal plot line
    
    lines(seq(min(cval_object$true[[i]]),max(cval_object$true[[i]]),length.out = 50),
          seq(min(cval_object$true[[i]]),max(cval_object$true[[i]]),length.out = 50),
          lwd = 2, col= scales::alpha(rgb(0,0,0), 0.3))
    
    # Scatter plot with true|estimated values 
    
    points(cval_object$true[[i]],cval_object$estim[,i], pch = pch, cex = cex, 
           col = "firebrick4", bg = scales::alpha("firebrick2", alpha = alpha))
    
    
    if(save){
      save_path <- file.path("..","..","..", "write", "plots")
      par(mar = c(4, 4, 1, 1))
      p <- recordPlot()
      pdf(file.path(save_path, paste0("cval_",names(cval_object$true)[i],".pdf")), width=7, height=7) 
      print(p)
      dev.off()
    }
    
  } # end loop for parameters
  
}
