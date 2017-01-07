myCorPlot <- function(df1,df2, main_title, x_title, y_title){
  # Merging two data frames for correlation
  plotDat <- merge(x=df1, y=df2, by="Annot2", all=TRUE)
  plotDat = plotDat[!is.na(plotDat$Total_vars.x) & !is.na(plotDat$Total_vars.y),]
  colnames(plotDat) <- c('Annot2','Annotation_combo.y','Prop1',"Total_vars_1","Point_size_1",
                         'Annotation_combo.x','Prop2',"Total_vars_2","Point_size_2")
  # Option: factor for points
  plotDat$Point_size <- ifelse(plotDat$Total_vars_1>plotDat$Total_vars_2, as.character(plotDat$Point_size_1), as.character(plotDat$Point_size_2)) # max
  plotDat$Point_size <- factor(plotDat$Point_size, levels=c("10-50","51-100","101-1000","1001-10000","Over 10000"))
  nTest = nrow(plotDat)
  # Compute correlation
  corRes = cor.test(plotDat$Prop1, plotDat$Prop2)
  corText = paste(paste('R2=',round(corRes$estimate,2)),'\n',paste('P-value=',prettyNum(corRes$p.value, digits=2)),'\n',
                  paste('Number of test=',nTest), sep='')
  # Color scheme
  cbPalette <- c("10-50"= "#999999", "51-100"= "#009E73", "101-1000"= "#E69F00", "1001-10000"= "#56B4E9", "Over 10000"= "#D55E00", "#CC79A7", "#0072B2")
  p <- ggplot(plotDat, aes(Prop1, Prop2)) + geom_point(alpha=1, aes(colour = factor(Point_size))) + 
    # Add the x,y center line
    geom_hline(yintercept = 0.5, color='gray') + geom_vline(xintercept = 0.5, color='gray') + 
    # Add the title and x,y label
    labs(x= x_title, y= y_title, title= main_title, color=NULL) +
    # Set the theme and color scheme
    theme_light(base_size = 12) + xlim(0,1) + ylim(0,1) + scale_colour_manual(values=cbPalette) + 
    # Adding a regression line
    geom_smooth(method='lm', formula=y~x, color='red') + annotate("text",  x=0.1, y=0.1, label = corText, hjust=-0.1) 
  return(p)
}