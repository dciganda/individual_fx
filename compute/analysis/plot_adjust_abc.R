plot_adjust_abc <- function(post_object, complete_data,
                            bolfi_output, deep_output, tol, subsample = 1000, save = F,
                            pch = 21, cex = 1, each_x = 1,
                            each_y = 1, alpha = 0.8,
                            margin_y = 0.2, margin_x = 0.2,
                            x_coord = 20, y_coord = 20, # for legend
                            grid_rows = 20,
                            main = F){
  
  # verifivation of tol range between [0,1]
  if (tol <= 0 || tol >= 1) {
    # Crea un objeto de advertencia personalizado
    w <- simpleWarning("Tolerance must be between 0 and 1! Stopping plot_adjust_abc.")
    # Emite la advertencia sin la llamada de la función
    warning(w)
    return(NULL)
  }
  
  # must have a dist column variable to work!
  ordenados <- bolfi_output[order(bolfi_output$dist),]
  bolfi_output <- ordenados[1:(tol*nrow(ordenados)),]
  
  ordenados2 <- deep_output[order(deep_output$dist),]
  deep_output <- ordenados2[1:(tol*nrow(ordenados2)),]
  
  add_empty_plot_with_grid <- function(x_data, y_data, x_label_text,
                                       y_label_text, grid_rows = grid_rows,
                                       margin_y = margin_y,
                                       margin_x = margin_x,
                                       main = ''){
    
    draw_grid <-function(x_coords, y_coords){
      
      # browser()
      
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
                        max(x_data, na.rm = T)+xmargin, length.out = grid_rows),10)
    new_y <<- round(seq(min(y_data, na.rm = T)-ymargin,
                        max(y_data, na.rm = T)+ymargin, length.out = grid_rows),10)
    
    plot(x_data, y_data, xlab=x_label_text, ylab= y_label_text,
         xlim = c(min(new_x),max(new_x)),
         ylim = c(min(new_y),max(new_y)),
         type = 'n', bty = "n", axes = F,  
         main = main) # main added to identify parameter
    
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
  
  # loop of all parameters of the post object 
  for(i in seq_along(colnames(post_object$unadj.values))){
    
    # Prior density
    set.seed(1234)
    mysample <- runif(1000, min(complete_data[,i]), max(complete_data[,i]))
    
    aux_prior <- density(mysample)
    aux_prior <- data.frame(x = aux_prior$x, y = aux_prior$y)
    
    # Unadj. Values
    aux_unadj <- density(post_object$unadj.values[,i])
    aux_unadj <- data.frame(x = aux_unadj$x, y = aux_unadj$y)
    
    # Adj. Values
    aux_adj <- density(post_object$adj.values[,i])
    aux_adj <- data.frame(x = aux_adj$x, y = aux_adj$y)
    
    # Scale is adjusted because as it is more concentrated around a point, 
    # the density is high and affects the plot
    # aux_adj$y <- aux_adj$y/max(aux_adj$y) * max(aux_prior$y, aux_unadj$y)
    aux_bolfi <- density(bolfi_output[,i])
    aux_bolfi <- data.frame(x = aux_bolfi$x, y = aux_bolfi$y)
    
    aux_deep <- density(deep_output[,i])
    aux_deep <- data.frame(x = aux_deep$x, y = aux_deep$y)
    
    par(ask=F) # Remove "Hit <Return> to see next plot"
    
    add_empty_plot_with_grid(x_data = aux_prior$x,
                             y_data =  aux_adj$y,
                             x_label_text = '',
                             y_label_text =  '',
                             margin_y = margin_y,
                             margin_x = margin_x, 
                             grid_rows = grid_rows,
                             main = ifelse(main == T, colnames(post_object$unadj.values)[i], '') )
    
    
    # Adjust data to plot grid
    aux_prior <- aux_prior[aux_prior$x >= min(new_x) & aux_prior$x <= max(new_x), ]
    aux_unadj <- aux_unadj[aux_unadj$x >= min(new_x) & aux_unadj$x <= max(new_x), ]
    aux_adj <- aux_adj[aux_adj$x >= min(new_x) & aux_adj$x <= max(new_x), ]
    
    
    aux_bolfi <- aux_bolfi[aux_bolfi$x >= min(new_x) & aux_bolfi$x <= max(new_x), ]
    
    aux_deep <- aux_deep[aux_deep$x >= min(new_x) & aux_deep$x <= max(new_x), ]
    
    # Prior plot
    lines(x = aux_prior$x, y = aux_prior$y, lwd = 2, lty = "dotted",
          col = "gray50")
    
    # Unadj. Values
    lines(x = aux_unadj$x, y = aux_unadj$y, lwd = 2, lty = "dotted",
          col = "orange")
    
    # Adj. Values
    lines(x = aux_adj$x, y = aux_adj$y, lwd = 2, lty = "solid",
          col = "firebrick2")
    
    # bolfi output
    lines(x = aux_bolfi$x, y = aux_bolfi$y, lwd = 2, lty = "solid",
          col = "navy")
    
    # deep bolfi output
    lines(x = aux_deep$x, y = aux_deep$y, lwd = 2, lty = "solid",
          col = "black")
    
    # Legend
    op <- par(family = "sans")
    
    
    legend("topright", legend = c("prior", "abc rej.", "abc adj.","bolfi", "deep bolfi"),
           lwd = c(2,2,2,2,2), col = alpha(c("gray50","orange","firebrick2","navy","black"), alpha), lty = c(3,3,1,1,1),
           pch = c(1,2,5,5,5), pt.cex = c(0,0,0,0,0),
           cex=1, bty = "n",
           y.intersp = 0.8,
           x.intersp = 0.2, xjust = 1, yjust = 1)
    
    
    if(save){
      save_path <- file.path("..","..","..", "write", "plots")
      par(mar = c(4, 4, 1, 1, 1))
      p <- recordPlot()
      pdf(file.path(save_path, paste0("abc_adjust_",colnames(post_object$unadj.values)[i],".pdf")), width=10, height=7) 
      print(p)
      dev.off()
    }
    
  } # end loop for parameters
  
}
