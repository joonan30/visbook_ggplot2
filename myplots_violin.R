violin_2vars <- function(data, var_x, factor_a, title, x_title, y_title){
  p <- ggplot(data, aes(factor_a, (var_x))) + geom_violin(aes(fill=factor_a)) + 
  scale_fill_manual(values=c("firebrick", "steelblue")) + labs(title=title,x=x_title, y = y_title) +
  theme_light()
  return (p)
}

violin_2vars_log10 <- function(data, var_x, factor_a, title, x_title, y_title){
  p <- ggplot(data, aes(factor_a, log10(var_x))) + geom_violin(aes(fill=factor_a)) + 
    scale_fill_manual(values=c("firebrick", "steelblue")) + labs(title=title,x=x_title, y = y_title) +
    theme_light()
      theme_bw()
  return (p)
}
