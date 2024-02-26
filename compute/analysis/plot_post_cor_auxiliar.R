
# --------------- Libraries -------------------- ----

library(MASS) # to use  kde2d
library(rgl) # to use plt3d
library(RColorBrewer) # for distinct palette
        

# --------------- Data generated as example ---- ----

set.seed(123)  

# Range for each column
range_col1 <- c(32, 41)
range_col2 <- c(0.1, 0.85)
range_col3 <- c(0.04, 0.2)

# Create an auxiliary function to generate a column within a specific range
generate_column <- function(n, range) {
  runif(n, min = range[1], max = range[2])
}

# Create a function to generate a matrix with columns in the specified ranges
generate_matrix <- function(nrow, ranges) {
  matrix(
    c(
      generate_column(nrow, ranges[[1]]),
      generate_column(nrow, ranges[[2]]),
      generate_column(nrow, ranges[[3]])
    ),
    ncol = 3
  )
}

# Create a list of example data matrices that meet the requirements
accepted_list <- list(
  data.frame(generate_matrix(100, list(range_col1, range_col2, range_col3))),
  data.frame(generate_matrix(100, list(range_col1, range_col2, range_col3))),
  data.frame(generate_matrix(100, list(range_col1, range_col2, range_col3)))
)

for(i in 1:length(accepted_list)){
  
  names(accepted_list[[i]]) <- c("alpha","lambda","max_phi")
  
}


# --------------- Auxiliar function ------------ ----

draw_grid <-function(x_coords, y_coords, grid_rows){
  
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


# --------------- PLOT_POR_COR ----------------- ----

# some inputs
n_breaks=50
margin=0.1
grid_rows=15
n_round=4

# Function
plot_post_cor <- function(accepted, plot_type = "scatter"){
  

  for (k in 1:length(accepted)) {
    for (i in 1:length(accepted)) {
      for (j in 1:length(accepted)) {
        
        if ((i == 1 && j == 2) || (i == 2 && j == 3) || (i == 3 && j == 1)) {
          
          if(plot_type == "scatter"){
            
            margin <- 0.1
            
            # Auxiliar code to draw grid background
            min_x <- min(accepted[[k]][,i])-min(accepted[[k]][,i])*margin
            max_x <- max(accepted[[k]][,i])+max(accepted[[k]][,i])*margin
            min_y <- min(accepted[[k]][,j])-min(accepted[[k]][,j])*margin
            max_y <- max(accepted[[k]][,j])+max(accepted[[k]][,j])*margin
            
            x_coords_plot <- seq(min_x, max_x, length.out = grid_rows)
            y_coords_plot <- seq(min_y, max_y, length.out = grid_rows)
            
            # Plot grid as background
            plot(0, 
                 type = "l",
                 xlim = c(min_x, max_x),
                 ylim = c(min_y,max_y),
                 xlab = names(accepted[[k]][i]),
                 ylab = names(accepted[[k]][j]), 
                 main = paste(names(accepted[[k]][i]), "vs",names(accepted[[k]][j]),sep=" "),
                 axes = FALSE)
            
            # Add X-axis
            axis(1, at = round(seq(min_x, max_x, length.out = n_breaks),n_round))
            
            # Add Y-axis
            axis(2, at = round(seq(min_y, max_y, length.out = n_breaks),n_round))
            
            par(new = TRUE)
            draw_grid(x_coords_plot, y_coords_plot, grid_rows)
            par(new = TRUE)
            
            # Plot as scatterplot (actual method)
            plot(accepted[[k]][,i],accepted[[k]][,j],
                 xlim=c(min_x,max_x),
                 ylim=c(min_y,max_y),
                 col = rgb(0.93, 0.17, 0.17, alpha = 0.8),
                 pch =16,
                 cex = 1.5,
                 xlab = "",
                 ylab = "",
                 axes = FALSE)
            
          }
          
          if(plot_type == "contour"){
          
            # browser()

            # Generate bivariate distribution for coord z
            dens2d <- kde2d(accepted[[k]][,i], accepted[[k]][,j], n = 50)  
            
            
            # Plot the information as heatmap (contour)
            filled.contour(
                    x = dens2d$x,
                    y = dens2d$y,
                    z = dens2d$z,
                    xlab = names(accepted[[k]][i]),
                    ylab = names(accepted[[k]][j]),
                    main = paste(names(accepted[[k]][i]), "vs",names(accepted[[k]][j]),sep=" ")
                    )

            
          }
          
          if(plot_type == "persp"){
            
            # Plot the information as heatmap
            dens2d <- kde2d(accepted[[k]][,i], accepted[[k]][,j], n = 50)  
            
            # Plot the information with persp
            persp(
              x = dens2d$x,
              y = dens2d$y,
              z = dens2d$z,
              xlab = names(accepted[[k]][i]),
              ylab = names(accepted[[k]][j]),
              main = paste(names(accepted[[k]][i]), "vs",names(accepted[[k]][j]),sep=" "),
              col = rgb(0.93, 0.17, 0.17, alpha = 0.8),
              theta = 30, phi = 30,
              ticktype="detailed"
            )
            
          }
          
          if(plot_type == "heatmap"){
            
            rf <- colorRampPalette(brewer.pal(9,'OrRd'))
            r <- rf(32)
            
            # Plot the information as heatmap
            dens2d <- kde2d(accepted[[k]][,i], accepted[[k]][,j], n = 50)  
            
            # Plot the information with image
            image(dens2d, col = r, 
                  main = paste(names(accepted[[k]][i]), "vs",names(accepted[[k]][j]),sep=" ")
                  )
            
          }
          
            
        }  
      }
    }
  }
  

}



# Run function
# Plot style available:
# - scatter (Default)
# - contour
# - persp
# - heatmap

plot_post_cor(accepted = accepted_list, plot_type = "contour")
