plot_amenhorrhea <- function(pop, alpha = c(0.5, 0.9), save = F) {
  
  library(scales)
  
  # browser()
  
  # -----------     VALUES DENSITY       ----------- ----
  
  for (i in seq_along(pop)) {
    
    pop[[i]] <- density(pop[[i]], bw = "SJ", kernel = "epanechnikov")
    
    
  }
  
  # -----------     AUXILIAR FUNCTION      ----------- ----
  
  add_empty_plot_with_grid <- function(x_data, y_data, x_label_text,
                                       y_label_text, grid_rows = 25,
                                       margin_y = margin_y,
                                       margin_x = margin_x,
                                       each_x = 2, each_y = 2){
    
    
    
    draw_grid <-function(x_coords, y_coords){
      
      for(i in 1:length(x_coords)){
        
        # we draw the i_th vertical line
        segments(x0 = x_coords[i], x1 = x_coords[i],
                 y0 = min(y_coords), y1 = max(y_coords), col=alpha(rgb(0,0,0), 0.1))
      }
      for(i in 1:length(y_coords)){
        # we draw the i_th horizontal line
        segments(x0 = min(x_coords), x1 = max(x_coords),
                 y0 = y_coords[i], y1 = y_coords[i], col=alpha(rgb(0,0,0), 0.1))
      }  
      
    }
    
    xmargin <- (max(x_data, na.rm = T)-min(x_data, na.rm = T))*margin_x
    ymargin <- (max(y_data, na.rm = T)-min(y_data, na.rm = T))*margin_y 
    
    new_x <- round(seq(min(x_data, na.rm = T)-xmargin,
                       max(x_data, na.rm = T)+xmargin, by = each_x),0)
    
    new_y <- round(seq(0,max(y_data, na.rm = T)+ymargin, by = each_y),0)
    
    plot(x_data, y_data, xlab=x_label_text, ylab= y_label_text,
         xlim = c(min(new_x),max(new_x)),
         ylim = c(min(new_y),max(new_y)),
         type = 'n', bty = "n", axes = F)
    
    # only x-axis interval
    axis(side=1, at=round(new_x,0))
    draw_grid(new_x, new_y)
    
    # ticks <- round(new_y,1)
    # axis(side=2, at=ticks)
    
    par(new=TRUE)
    
  }
  
  # -----------     GRID PLOT              ----------- ----
  
  add_empty_plot_with_grid(x_data = seq(6,21,2),
                           y_data = seq(6,21,2), 
                           x_label_text = "Months",
                           y_label_text = "",
                           margin_y = 0,
                           margin_x = 0)
  
  # -----------     POLYGONS | SEGMENTS    ----------- ----

  
  for (i in seq_along(pop)) {
    dens <- pop[[i]]
    offset <- i * 3.5
    
    # Fill density with polygon
    polygon(x = c(dens$x, rev(dens$x)), 
            y =c(offset + (dens$y / max(dens$y))*3, rep(offset, length(dens$x))), 
            col = alpha("firebrick", alpha = alpha[1]), border = NA)
    
    # Density contour line
    lines(dens$x, offset + (dens$y / max(dens$y))*3, col = alpha("firebrick", alpha = alpha[2]), lwd = 1)
    
    # Text of each density
    text(x = (max(pop[[i]]$x) + 1.2), y = offset + 1,
         labels = names(pop)[i])
    
    # Med. Line
    segments(x0 = dens$x[which(dens$y == max(dens$y))], y0 = (offset - 0.5),
             y1 = (offset + 3 + 0.5), lwd = 3, lty = "dotted",
             col = alpha("firebrick", alpha[2]))
    
    
  }
  
  
  # -----------     SAVE                   ----------- ----
  
  if(save){
    
    save_path <- file.path("..","write", "plots")
    # par(mar = c(4, 4, 0.1, 0.1))
    p <- recordPlot()
    pdf(file.path(save_path, paste0("am_duration.pdf")), width=7, height=7) 
    print(p)
    dev.off()
    
    
    
  }

  
}
