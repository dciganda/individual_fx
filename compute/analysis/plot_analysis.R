plot_analysis <- function(post, obs, save = F,
                     pch = 19, cex = 0.2, each_x = 1, each_y = 0.01,
                     alpha = 0.7, fake_obs = F, print_posterior = F, nsim = 2,
                     epsilon = 0.1, margin_y = 0.05, margin_x = 0.05,
                     x_coord = 40, y_coord = 0.4,
                     grid_rows = 25){
  

  # FUNCTIONS ----
  add_empty_plot_with_grid <- function(x_data, y_data, x_label_text,
                                       y_label_text, grid_rows = grid_rows,
                                       margin_y = margin_y,
                                       margin_x = margin_x){
    
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
  
  source(file.path("functions","intervals_function.R"))
  
  plot_obs <- function(obs){
    
    # change background color
    par(bg = "white")
    
    add_empty_plot_with_grid(obs[[1]]$age, obs[[1]]$fx, "Age", "f(x)")
    
    ticks <- seq(min(obs[[1]]$fx), max(obs[[1]]$fx), by = 0.1)
    axis(side=2, at=ticks)
    
    # Add points to plot
    points(obs$age, obs$fx, pch = pch, col = alpha(obs_col, alpha),bty = "n")

    # Legend
    op <- par(family = "sans")
    legend(40, 0.5, legend = c("Observed"),
           lwd = c(1), col = alpha(obs_col, alpha), lty = c(0),
           pch = c(16),
           cex=0.95, bty = "n",
           y.intersp = 0.8,
           x.intersp = 0.5)
    
    ## reset plotting parameters
    par(op)
    
  }
  
  plot_obs_compare <- function(obs){
    
    # change background color
    par(bg = "white")
    
    m <- which.max(sapply(obs, function(x) max(x[[1]]$fx)))
    
    add_empty_plot_with_grid(x_data = obs[[m]][[1]]$age,
                             y_data = obs[[m]][[1]]$fx,
                             x_label_text = "Age",
                             y_label_text = "f(x)",
                             margin_y = margin_y,
                             margin_x = margin_x)
    
    pchs <- c(2,1,5)
    cexs <- c(0.9,1.1,1)
    
    # Add points to plot
    for(i in 1:length(obs)){
      lines(obs[[i]][[1]]$age, obs[[i]][[1]]$fx, lty = 2, col = alpha(obs_col, alpha), lwd = 1)
      points(obs[[i]][[2]]$age, obs[[i]][[2]]$fx, pch = pchs[i], col = alpha(obs_col, alpha),bty = "n", cex = cexs[i])
      
    }
    # Legend
    op <- par(family = "sans")
    legend(7, 0.57, legend = c("Hutterites", "Historic Quebec", "Historic France"),
           lwd = c(1,1,1), col = alpha(obs_col, alpha), lty = c(2,2,2),
           pch = c(1,2,5), pt.cex = c(1.3,1,1),
           cex=1.1, bty = "n",
           y.intersp = 1.2,
           x.intersp = 0.5)
    
    
    ## reset plotting parameters
    par(op)
    
    
  }
  
  plot_sim_obs <- function(x_coord, y_coord){

    obs_path <- file.path("..","..","data","final", pop,"out")
    obs <- read.csv(file.path(obs_path, "asfr.csv"), header = T)
    
    
    # to calculate intervals with interval function
    post_data <- readRDS(file.path(global_path, "post", "posterior.rds"))
    
    intervals_sim <- intervals_function(global_path = global_path,
                                        post_data =  post_data,
                                        element = "asfr",
                                        iniY = iniY,
                                        endY = endY,
                                        nsim = nsim,
                                        eps = epsilon,
                                        age_range = (max(obs$age)+1 - min(obs$age)) ) # range between ages (adjust to include bounds)
    
     
    add_empty_plot_with_grid(x_data = as.numeric(colnames(intervals_sim)),
                             y_data =  as.numeric(intervals_sim[2,]),
                             x_label_text = "Age",
                             y_label_text =  "f(x)",
                             margin_y = margin_y,
                             margin_x = margin_x)
    
    # specify y-axis interval 
    ticks <- seq(min(obs$fx), round(max(as.numeric(intervals_sim[2,]),1)), by = 0.1)
    axis(side=2, at=ticks, labels = ticks)
    
    # Adding observed data as points to plot
    points(obs$age, obs$fx, col=alpha(obs_col,alpha), pch = pch, lwd = 2)
    
    
    # to draw intervals with polygon, there is a special way to specify the parameters:
    #  - the x-axis must be passed 2 times, one normal and the other inverted (with inv).
    #  - the y-axis first the upper edge and then the lower one inverted (with inv) 
    polygon(x = c(colnames(intervals_sim), rev(colnames(intervals_sim))),
            y = c(intervals_sim[3,],rev(intervals_sim[1,])),
            col = adjustcolor("firebrick2", alpha.f = 0.20), border = NA)
    
    
    points(colnames(intervals_sim),intervals_sim[2,], pch = 19 , col = 'red4')
    
    # Legend
    op <- par(family = "sans")
    
    
    legend(legend = c("Observed", "Median of sim.","95% interval"),
           lwd = c(1,1,1), col = c(alpha(obs_col, alpha), "red4",adjustcolor("firebrick2", alpha.f = 0.20)), lty = c(0,0,0),
           pch = c(16, 16, 15),
           cex=0.85, bty = "n", y.intersp = 1.2,
           x = x_coord,
           y = y_coord)
    
  }
  
  save_plot <- function(name, pop){
    save_path <- file.path("..","..","..", "write", "plots")
    par(mar = c(4, 4, 0.1, 0.1))
    p <- recordPlot()
    pdf(file.path(save_path, paste0(name,"_",pop,".pdf")), width=8, height=8) 
    print(p)
    dev.off()
  }
  
  
  obs_col <- rgb(131/255,75/255,159/255)
  sim_col <- "firebrick2" #rgb(0.93,0.17,0.17, alpha = 0.8) #rgb(193/255,78/255,78/255)
  
  # PLOTS ----
  
  if(p_obs){
    obs <- get_obs_data(pop, fake_obs = fake_obs)
    plot_obs(obs)
    save_plot("obs", pop)
  }
  
  if(p_sim_obs){
    plot_sim_obs(x_coord, y_coord)
    save_plot("sim_obs", pop)
  }
  
  if(p_obs_compare){
    obs <- lapply(pop, get_obs_data, fake_obs = fake_obs)
    plot_obs_compare(obs)
    save_plot("obs_compare", pop)
  }
  
  # PRINT POSTERIOR ----
  if(print_posterior){
    post <- readRDS(file.path(global_path, "post", "posterior.rds"))
    print(head(post[order(post$mse),], 50))
    
  }
  
  
  
}



