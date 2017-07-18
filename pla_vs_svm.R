# Perceptron Learning Algorithm Simulation

n_runs <- 1000  # no. of runs
n_pts <- 10   # no. of training points

sum_iter = 0
sum_misc = 0
d = 0

for ( i in 1:n_runs ) {
  # target function
  x1_target <- runif( 2, min = -1, max = 1 )
  x2_target <- runif( 2, min = -1, max = 1 )
  
  w <- c(0,0)    # initializing weights
  
  # generating training points
  x1_train <- runif ( n_pts, min = -1, max = 1 )
  x2_train <- runif ( n_pts, min = -1, max = 1 ) 
  
  # calculate slope of the target function line
  m = (x2_target[2] - x1_target[2])/(x2_target[1] - x1_target[1])
  # calculating and flaggin the training data output using the target function
  y_train <- x2_train - ( x1_target[2] + m*( x1_train - x1_target[1] ))
  y_train <- ifelse ( y_train >= 0, 1, -1 )
  
  train_data <- data.frame( x1_train, x2_train, y_train )
  
  n_iter = 0
  misc_count = 1
  
  while ( n_iter < 100 && misc_count > 0 ) {
    n_iter = n_iter + 1
    # calculating and flagging predicted using weights
    y_pred <- w[1] * train_data$x1_train + w[2] * train_data$x2_train
    train_data$y_pred <- ifelse ( y_pred >= 0, 1, -1 )
    
    misc_count = length( which ( train_data$y_train != train_data$y_pred ))
    if ( misc_count > 0 ) {
      r_misc = sample(1:misc_count, 1)
      r_ind = (which ( train_data$y_train != train_data$y_pred ))[r_misc]
      
      w[1] <- w[1] + train_data$y_train[r_ind] * train_data$x1_train[r_ind]
      w[2] <- w[2] + train_data$y_train[r_ind] * train_data$x2_train[r_ind]
    }
  }
  
  if ( n_iter < 99 ) {
    sum_iter = sum_iter + n_iter    
    sum_misc = sum_misc + misc_count/n_pts
    d = d + 1
  }

}

sum_iter/d
sum_misc/d
