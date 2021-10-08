Urn_draws <- function(urn_size=100, ndraws = 1) {
  
  if (ndraws > urn_size) stop(paste('ndraws cannot be >', as.character(urn_size), '!') )
  
  balls <- sample(1:urn_size, size = ndraws, replace = FALSE)
  
  for (i in 1:ndraws) {
    if (balls[i] < 10){
      print(balls[i])
    }
  } 
  # another way of printing
  print(balls[balls<10])
  
  balls <- sort(balls)
  
  mean <- mean(balls)
  
  min <- min(balls)
  
  max <- max(balls)
  
  even_prop <- sum((balls %% 2) == 0) / ndraws
  
  list(balls = balls,
       mean = mean,
       min = min,
       max = max,
       even_prop = even_prop)
}