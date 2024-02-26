plot_estimated_agem_dist <- function(param,
                                    obs_data,
                                    n_breaks = 50,
                                    margin = 0.05,
                                    grid_rows = 20,
                                    n_round = 10,
                                    alpha = 0.5,
                                    save = FALSE) {
  
  draw_grid <- function(x_coords, y_coords, grid_rows){
    for(i in 1:grid_rows){
      segments(x0 = x_coords[i], x1 = x_coords[i], y0 = min(y_coords),
               y1 = max(y_coords), col = rgb(0,0,0,alpha= 0.1))
      segments(x0 = min(x_coords), x1 = max(x_coords),
               y0 = y_coords[i], y1 = y_coords[i], col = rgb(0,0,0, alpha = 0.1))
    }
  }
  
  plot_data <- function(data, color, min, max, breaks) {
    bks <- findInterval(data, breaks[1:(n_breaks-1)])
    cts <- as.data.frame(table(bks))
    counts <- merge(data.frame(bks = 1:(n_breaks-1)), cts, by = "bks", all.x = TRUE) 
    counts$Freq[is.na(counts$Freq)] <- 0
    counts$Freq <- counts$Freq / sum(counts$Freq)
    
    col_with_alpha <- rgb(col2rgb(color)[1]/255, col2rgb(color)[2]/255, col2rgb(color)[3]/255, alpha)
    
    for (j in 1:(n_breaks - 1)) {
      rect(breaks[j], 0, breaks[j + 1], counts$Freq[j], col = col_with_alpha, border = NA)
    }
  }
  
  combined_data <- unlist(obs_data)
  aux <- (max(combined_data)-min(combined_data)) * margin
  min_val <- round(round(min(combined_data), n_round) - aux, n_round)
  max_val <- round((ceiling(max(combined_data) * 10**n_round) / 10**n_round) + aux, n_round) 
  breaks <- seq(min_val, max_val, length.out = n_breaks)
  
  x_coords <- seq(min_val, max_val, length.out = grid_rows)
  y_coords <- seq(0, 0.3, length.out = grid_rows) # Adjust as needed
  
  plot(0, type = "n", xlim = c(min_val, max_val), ylim = c(0, 0.3), axes = FALSE, ylab = "", xlab = "") # Adjust ylim as per your data
  axis(1, at = round(seq(min_val, max_val, ((max_val-min_val) / (n_breaks + 4))), n_round))
  axis(2, at = seq(0, 0.3, 0.01)) # Adjust y-axis ticks as needed
  draw_grid(x_coords, y_coords, grid_rows)
  
  plot_data(data = obs_data[[1]], color = "violet",
            min =  min_val, max = max_val, breaks = breaks)
  plot_data(obs_data[[2]], "purple", min_val, max_val, breaks)
  plot_data(obs_data[[3]], "darkorchid", min_val, max_val, breaks)
  
  if(save){
    # Save logic here
  }
}



