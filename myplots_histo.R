histo_2vars <- function(data, var_x, factor_a, title, x_title, y_title){
p <- ggplot(data, aes(x=(var_x), color=factor_a, fill=factor_a)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins=200) +  geom_density(alpha=0.6) + 
  scale_color_manual(values=c("dimgray","darkorchid4")) + scale_fill_manual(values=c("dimgray", "darkorchid4")) +
  labs(title="title",x="x_title", y = "y_title") +
  theme_light()
}
