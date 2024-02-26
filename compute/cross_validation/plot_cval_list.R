plot_cval_list <- function(cval_object_list, n=c(), colour=F, save = F,
                      pch = 21, cex = 1.2, each_x = 0.1,
                      each_y = 1, alpha = 0.6,
                      margin_y = 0.2, margin_x = 0.2,
                      x_coord = 20, y_coord = 20, # for legend
                      grid_rows = 21){

  
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
    
    # browser()
    
    xmargin <- (max(x_data, na.rm = T)-min(x_data, na.rm = T))*margin_x
    ymargin <- (max(y_data, na.rm = T)-min(y_data, na.rm = T))*margin_y 
    
    new_x <- round(seq(min(x_data, na.rm = T)-xmargin,
                        max(x_data, na.rm = T)+xmargin, length.out = grid_rows),4)
    new_y <- round(seq(min(y_data, na.rm = T)-ymargin,
                        max(y_data, na.rm = T)+ymargin, length.out = grid_rows),4)
    
    
    # specify x-axis interval (its the same for y-axis)
    axis_adjust_x <<- pretty(new_x, n = grid_rows) 
    axis_adjust_y <<- pretty(new_x, n =grid_rows) 
    
    
    plot(x_data, y_data, xlab=x_label_text, ylab= y_label_text,
         xlim = c(min(axis_adjust_x),max(axis_adjust_x)),
         ylim = c(min(axis_adjust_y),max(axis_adjust_y)),
         type = 'n', bty = "n", axes = F,  
         main = "") # main added to identify parameter
    
      # adjustment so that the x-axis do not cross the y-axis
      axis(side=1, at= axis_adjust_x, tick = T) # pretty axis labels
      axis(side=1, at=c(min(axis_adjust_x),max(axis_adjust_x)), labels = F) # axis line to fit the grid
      # axis(side=1, at= axis_adjust_x[c(-2,-length(axis_adjust_x))], labels = F) # avoid limits of pretty axis

      
      axis(side=2, at= round(axis_adjust_y,2), tick = T, las = 2) # pretty axis labels
      axis(side=2, at=c(min(axis_adjust_y),max(axis_adjust_y)), labels = F) # axis line to fit the grid
      # axis(side=2, at= axis_adjust_x[c(-2,-length(axis_adjust_x))], labels = F) # avoid limits of pretty axis
      

    draw_grid(axis_adjust_x, axis_adjust_y) # because x-axis and y-axis have the same scale
    
    par(new=TRUE)
  }
  
  # Sorting cval_object_list and n by sample size
  n_sorted <- sort(n, index.return = TRUE)
  cval_object_list_sorted <- cval_object_list[n_sorted$ix]
  n_sorted_values <- n_sorted$x
  
  # Main loop for each parameter
  n_param <- length(cval_object_list[[1]]$true)
  for(i in 1:n_param){
    
    # Calculate the overall range for x_data and y_data
    all_x_data <- unlist(lapply(cval_object_list, function(obj) obj$true[[i]]))
    all_y_data <- unlist(lapply(cval_object_list, function(obj) obj[["estim"]][[1]][,i]))
    
    par(mar = c(4, 4, 1, 1))
    add_empty_plot_with_grid( # initializing the empty plot
      x_data = all_x_data, 
      y_data = all_y_data,
      x_label_text = expression(theta^"*"),
      y_label_text = expression(theta),
      margin_y = margin_y,
      margin_x = margin_x, 
      grid_rows = grid_rows
    )
    
    # Diagonal plot line
    lines(seq(min(all_x_data), max(all_x_data), length.out = 50),
          seq(min(all_x_data), max(all_x_data), length.out = 50),
          lwd = 2, col= scales::alpha(rgb(0,0,0), 0.3))
    
    # Define shapes and their respective transparencies
    shapes <- c(17, 15, 19)  # Triangle, Square, Circle
    transparencies <- c(0.4, 0.6, 0.8)  # Varying transparency levels
    
    # Loop through each cval object to plot points
    for(j in 1:length(cval_object_list_sorted)){
      x_data <- cval_object_list_sorted[[j]]$true[[i]]
      y_data <- cval_object_list_sorted[[j]][["estim"]][[1]][,i]
      
      if (colour == T){
        points(x_data, y_data, pch = 19, col = scales::alpha(rainbow(length(cval_object_list_sorted))[j], alpha), cex = cex)
      } else {
        points(x_data, y_data, pch = shapes[j], col = scales::alpha("firebrick2", transparencies[j]), cex = cex)
      }
      
    }
    
    legend_labels <- paste0("Sample size = ", n_sorted_values)
    
    # Position the legend using normalized parent coordinates (npc)
    # Differentiate legend parameters based on the 'colour' parameter
    if (colour == T) {
      legend_colours <- scales::alpha(rainbow(length(cval_object_list_sorted)), alpha)
      legend_shapes <- rep(19, length(cval_object_list_sorted))  # All circles
    } else {
      legend_colours <- rep(scales::alpha("firebrick2", transparencies), each = length(shapes))
      legend_shapes <- rep(shapes, length(cval_object_list_sorted) %/% length(shapes) + 1)[1:length(cval_object_list_sorted)]
    }
    
    legend('topleft', inset=c(0.1,0.1), legend = legend_labels,
           col = legend_colours, pch = legend_shapes,
           cex = 0.8, bty = "n")
    
    if(save){
      save_path <- file.path("..", "write", "plots")
      par(mar = c(4, 4, 1, 1))
      p <- recordPlot()
      pdf(file.path(save_path, paste0("cval_",cval_object_list[[1]]$names$parameter.names[[i]],".pdf")), width=7, height=7) 
      print(p)
      dev.off()
    }
    
  } # end loop for parameters
  
}
