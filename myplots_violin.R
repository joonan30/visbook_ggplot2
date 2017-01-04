violin_2vars <- function(data, var_x, factor_a, title, x_title, y_title){
  p <- ggplot(data, aes(x=var_x, color=factor_a, fill=factor_a)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins=200) + 
  scale_color_manual(values=c("firebrick", "steelblue")) + scale_fill_manual(values=c("firebrick", "steelblue")) +
  labs(title=title,x=x_title, y = y_title) +
  theme_bw()
  return (p)
}

violin_2vars_log10 <- function(data, var_x, factor_a, title, x_title, y_title){
    p <- ggplot(data, aes(x=log10(var_x), color=factor_a, fill=factor_a)) +
      geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins=200) + 
      scale_color_manual(values=c("firebrick", "steelblue")) + scale_fill_manual(values=c("firebrick", "steelblue")) +
      labs(title=title,x=x_title, y = y_title) +
      theme_bw()
  return (p)
}

violin_2vars_density <- function(data, var_x, factor_a, title, x_title, y_title){
  require(plyr)
  mu <- ddply(data, factor_a, summarise, grp.mean=mean(var_x))
  p <- ggplot(data, aes(x=var_x, color=factor_a, fill=factor_a)) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins=200) + 
    geom_density(alpha=0.6) + geom_vline(data=mu, aes(xintercept=grp.mean, color=factor_a), linetype="dashed") +
    scale_color_manual(values=c("firebrick", "steelblue")) + scale_fill_manual(values=c("firebrick", "steelblue")) +
    labs(title=title,x=x_title, y = y_title) +
    theme_bw()
  return (p)
}

violin_2vars_log10_density <- function(data, var_x, factor_a, title, x_title, y_title){
  require(plyr)
  mu <- ddply(data, factor_a, summarise, grp.mean=mean(log10(var_x)))
  p <- ggplot(data, aes(x=log10(var_x), color=factor_a, fill=factor_a)) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins=200) + 
    geom_density(alpha=0.6) + geom_vline(data=mu, aes(xintercept=grp.mean, color=factor_a), linetype="dashed") +
    scale_color_manual(values=c("firebrick", "steelblue")) + scale_fill_manual(values=c("firebrick", "steelblue")) +
    labs(title=title,x=x_title, y = y_title) +
    theme_bw()
  return (p)
}