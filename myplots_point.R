point_2vars <- function(data, var_x, factor_a, title, x_title, y_title){
  p <- ggplot(data, aes(x=var_x, color=factor_a, fill=factor_a)) +
    geom_point(size = 3, alpha = 0.5) + # geom_hline(yintercept = 1) + geom_hline(yintercept = 3) +
    scale_color_manual(values=c("firebrick", "steelblue")) + scale_fill_manual(values=c("firebrick", "steelblue")) +
    labs(title=title,x=x_title, y = y_title) + theme_bw()
  return (p)
}