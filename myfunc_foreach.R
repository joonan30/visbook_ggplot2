# run function in parallel and get the result in a list of data frames
require(foreach)
res = foreach(k=1: nrow(testData)) %do% function_name(testData, k)

# merge data frames from the list into one dataframe
require(plyr)
outMat <- ldply(res, data.frame)
