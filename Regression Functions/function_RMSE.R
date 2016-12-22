RMSE <- function(x, y){
  tmp <- (x-y)*(x-y)
  r <- sum(tmp)/length(x)
  r <- sqrt(r)
  return (r)
}
