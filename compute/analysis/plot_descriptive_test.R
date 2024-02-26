library(scales)

rm(list=ls())

pop <- list('Hutterites' = c(8,10,12),
            'Historic Quebec' = c(10,12,14),
            'Historic France' = c(12,14,16))




# --------------------- FUNCTIONS --------------------- ----

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
  axis(side=2, at=ticks)
  
  par(new=TRUE)
  
}




# --------------------- EXECUTION --------------------- ----

add_empty_plot_with_grid(x_data = seq(7,18,2),
                         y_data = seq(7,18,2), 
                         x_label_text = "",
                         y_label_text = "",
                         # grid_rows = 40,
                         margin_y = 0.1,
                         margin_x = 0.1)

alpha <- c(0.5,0.9)

  polygon(x = c(min(pop$Hutterites), max(pop$Hutterites), max(pop$Hutterites),min(pop$Hutterites)),                 
          y = c(2, 2, 6, 6),                            
          col = alpha("firebrick", alpha[1]), border = F) 
  
    segments(x0 = pop$Hutterites[[2]], y0 = (2 - 0.2), y1 = (6 + 0.2), lwd = 3, lty = "dotted",
             col = alpha("firebrick", alpha[2]))
    
    text(x = (max(pop$Hutterites) + 0.8), y = ((6 + 0.2) + (2 - 0.2))/2, labels = names(pop)[1])
  
  
  polygon(x = c(min(pop$`Historic Quebec`), max(pop$`Historic Quebec`), max(pop$`Historic Quebec`),min(pop$`Historic Quebec`)),                          
          y = c(7, 7, 11, 11),                            
          col = alpha("firebrick", alpha[1]), border = F)

  
    segments(x0 = pop$`Historic Quebec`[[2]], y0 = (7 - 0.2), y1 = (11 + 0.2), lwd = 3, lty = "dotted",
             col = alpha("firebrick", alpha[2]))
    
    text(x = (max(pop$`Historic Quebec`) + 0.8), y = ((11 + 0.2) + (7 - 0.2))/2, labels = names(pop)[2])
  

  polygon(x = c(min(pop$`Historic France`), max(pop$`Historic France`), max(pop$`Historic France`),min(pop$`Historic France`)),                          
          y = c(12, 12, 16, 16),                            
          col = alpha("firebrick", alpha[1]), border = F)   
  
    segments(x0 = pop$`Historic France`[[2]], y0 = (12 - 0.2), y1 = (16 + 0.2), lwd = 3, lty = "dotted",
             col = alpha("firebrick", alpha[2]))
    
    text(x = (max(pop$`Historic France`) + 0.8), y = ((16 + 0.2) + (12 - 0.2))/2, labels = names(pop)[3])
  






# --------------------- TEST 1 ------------------------ ----


# df <- data.frame(
#   name = c('Hutt','Quebec',"France"),
#   from = c(8,10,12),
#   to = c(12,14,16))
# 
# 
# b <- barplot(as.numeric(df$to), horiz=TRUE, border='transparent', 
#              xlim=range(c(df$from, df$to)), xaxt='n', yaxs='i',
#              space=1)
# barplot(as.numeric(df$from), horiz=TRUE, space=1, add=TRUE, 
#         border='transparent', xaxt='n', names.arg=df$name, 
#         cex.names=0.8)
# 
# box(bty='n')
# axis.Date(1, pretty(df$from), cex.axis=0.8)
# 
# 
# 
# --------------------- TEST 2 ------------------------ ----
# 
# 
# df <- data.frame(
#   Item = c('Hutt','Quebec',"France"),
#   Start = c(8,10,12),
#   End = c(12,14,16),
#   Median = c(10,12,14)
# )
# 
# plot(df$Start, 
#      xaxt = 'n', # Elimina el eje x
#      yaxt = 'n', # Elimina el eje y
#      xlab = "", 
#      ylab = "", 
#      xlim = c(min(df$Start, df$End), max(df$Start, df$End)), 
#      ylim = c(1, length(df$Item)),
#      type = "n") # Crea un plot vacío con los límites especificados
# 
# # Segmentos
# segments(x0 = df$Start, y0 = 1:length(df$Item), 
#          x1 = df$End, y1 = 1:length(df$Item), 
#          col = "red", lwd = 50, alpha = 0.6)
# 
# # Valor Medio
# segments(x0 = df$Median, y0 = 1:length(df$Item), 
#          x1 = df$Median, y1 = 1:length(df$Item), 
#          col = "black", lwd = 5)
# 
# # Ejes
# axis(2, at = 1:length(df$Item), labels = df$Item, las = 2) # Eje y (transformado en eje x en ggplot)
# axis(1, las = 1) # Eje x (transformado en eje y en ggplot)
# title(ylab = "Value", line = 2)
# 
# --------------------- TEST 3------------------------- ----
# 
# plot(20, 20, col = "white", xlab = "X", ylab = "Y")            # Draw empty plot
# #To this plot, we can draw a polygon with the following R code:
#   
#   polygon(x = c(15, 25, 25, 15),                           # X-Coordinates of polygon
#           y = c(15, 15, 20, 20),                             # Y-Coordinates of polygon
#           col = "firebrick")     
# 

    
    
# --------------------- TEST 4 (WORKS) ---------------- ----
    
    plot_polygons <- function(pop, alpha) {
      
      add_empty_plot_with_grid(x_data = seq(7,18,2),
                               y_data = seq(7,18,2), 
                               x_label_text = "",
                               y_label_text = "",
                               margin_y = 0.1,
                               margin_x = 0.1)
      
      num_polygons <- length(pop) 
      y_offset <- 5  # Vertical space
      
      order <- names(pop)  # Orden deseado
      
      for (name in order) {
        i <- match(name, names(pop))  # Encuentra el índice del elemento en 'pop'
        
        x_values <- c(pop[[i]][1], pop[[i]][3], pop[[i]][3], pop[[i]][1])
        y_values <- c(2, 2, 6, 6) + (i - 1) * y_offset  # y coord. adjust
        
        # Polygon
        polygon(x = x_values, y = y_values, col = alpha("firebrick", alpha[1]), border = FALSE)
        
        # Inf. Line
        segments(x0 = pop[[i]][1], y0 = 2 + (i - 1) * y_offset, 
                 y1 = (6) + (i - 1) * y_offset, lwd = 2, 
                 col = alpha("firebrick", alpha[2]))
        
        # Med. Line
        segments(x0 = pop[[i]][2], y0 = (2 - 0.2) + (i - 1) * y_offset, 
                 y1 = (6 + 0.2) + (i - 1) * y_offset, lwd = 3, lty = "dotted", 
                 col = alpha("firebrick", alpha[2]))
        
        # Sup. Line
        segments(x0 = pop[[i]][3], y0 = 2 + (i - 1) * y_offset, 
                 y1 = (6) + (i - 1) * y_offset, lwd = 2, 
                 col = alpha("firebrick", alpha[2]))
        
        # Add label
        text(x = (max(pop[[i]]) + 0.8), y = ((6 + 0.2) + (2 - 0.2)) / 2 + (i - 1) * y_offset, 
             labels = name)
      }
    }
    
    plot_polygons(pop, alpha)
    
    
# --------------------- TEST 5 (DENSITY) -------------- ----
    
    set.seed(1234)
    pop_dens <- list('Hutterites' = density(rnorm(100, mean = 10))$x,
                     'Historic Quebec' = density(rnorm(100, mean = 12))$x,
                     'Historic France' = density(rnorm(100, mean = 14))$x)
    
    
    plot_descriptive <- function(pop_dens, alpha = c(0.4,0.8)) {
      
      for (i in seq_along(pop_dens)) {
      
        pop_dens[[i]] <- density(pop_dens[[i]])

          
      }
      

      
      add_empty_plot_with_grid <- function(x_data, y_data, x_label_text,
                                           y_label_text, grid_rows = 25,
                                           margin_y = margin_y,
                                           margin_x = margin_x,
                                           each_x = 1, each_y = 1){
        
        
        
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
        # axis(side=2,at=round(new_y,0))
        
        par(new=TRUE)
        
      }
      
      
      add_empty_plot_with_grid(x_data = seq(6,22,2),
                               y_data = seq(6,22,2), 
                               x_label_text = "",
                               y_label_text = "",
                               margin_y = 0.1,
                               margin_x = 0.1)
      
      y_offset <- 5  # Vertical space
      
      for (i in seq_along(pop_dens)) {
        dens <- pop_dens[[i]]
        offset <- i * 4
        
        # Fill density with polygon
        polygon(x = c(dens$x, rev(dens$x)), 
                y =c(offset + (dens$y / max(dens$y))*3, rep(offset, length(dens$x))), 
                col = alpha("firebrick", alpha = alpha[1]), border = NA)
        
        # Density contour line
        lines(dens$x, offset + (dens$y / max(dens$y))*3, col = alpha("firebrick", alpha = alpha[2]), lwd = 1)
        
        text(x = (max(pop_dens[[i]]$x) + 2), y = offset,
             labels = names(pop_dens)[i])
        
        # Med. Line
        segments(x0 = mean(pop_dens[[i]]$x), y0 = (offset - 0.5),
                 y1 = (offset + 3 + 0.5), lwd = 3, lty = "dotted",
                 col = alpha("firebrick", alpha[2]))
        
        
      }
        
        

    }
    
    plot_descriptive(pop_dens)
    
    