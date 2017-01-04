# View multiple plots from the list in one space
view_multiplots <- function(list_plots){
  n <- length(list_plots)
  nCol <- floor(sqrt(n))
  require(grid)
  do.call("grid.arrange", c(list_plots, ncol=nCol))
}

