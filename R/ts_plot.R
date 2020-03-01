#' Plot of forunco object
#'
#' @param forunco forunco object 
#' @param interactive whether or not to return an interactive plot or a static one
#' @param add_data additional data
#' @param ... addtional argument, currently not used
#'
#' @return ggplot/plotly object
#' @export
plot_forunco <- function(forunco, interactive = F, future_vals = NA, ...) {
  require(ggplot2)
  require(lubridate)
  # check if tidy_ts is of class forunco
  if (class(forunco) != 'forunco') 
    stop("Plots only objects of class forunco")
  
  ggd <- rbind(forunco$actuals, forunco$preds)
  
  # check if future values are availabel
  if (class(future_vals) == 'ts') {
    fv <- tidier_ts(y = future_vals)
    ggd <- rbind(ggd, fv)
  }
  
  ggd$date_int <- decimal_date(ggd$date)
  
  p_u <- ggd[which(grepl("^U", ggd$type)), ]
  p_l <- ggd[which(grepl("^L", ggd$type)), ][nrow(p_u):1,,drop=FALSE]
  ggd$c_i <- tolower(substr(ggd$type, 1, 1))
  
  # color palette
  pal <- c(a = '#000000', p = '#808080', u = '#808080', l = '#808080')
  
  # bind rows in p_u und p_l for polygon
  poly <- rbind(p_u, p_l)
  
  p <- ggplot(ggd, aes(x = date_int, y = value, col = c_i)) +
    geom_polygon(data = poly,
                 mapping = aes(x = date_int, y = value),
                 fill = '#808080',
                 colour = NA,
                 alpha = 0.2,
                 stat = 'identity') +
    geom_line() +
    scale_color_manual(values = pal) +
    geom_text(data = ggd[which(ggd$type != 'actual' & ggd$date_int == max(ggd$date_int)),],
              aes(x = (date_int + 0.8), y = value, label = type)) +
    #geom_dl(data = all_ts[which(all_ts$type != 'actual'),], aes(label = type), method = list(dl.trans(x = x + .2), "last.points")) +
    scale_x_continuous(expand = c(0,0.8)) +
    theme_minimal() +
    theme(axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
          axis.ticks.x = element_line(colour = 'black', size= 0.5, linetype = 'solid'),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.margin = unit(c(1,3,1,1), "lines"),
          axis.title = element_blank(),
          text = element_text(size = 14),
          legend.position = 'none')
  
  if (interactive) {
    require(plotly)
    p <- ggplotly(p)
  }
  
  return(p)
}
