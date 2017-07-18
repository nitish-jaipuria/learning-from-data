# Perceptron Learning Algorithm Simulation

n_runs <- 100
n_pts <- 100   # no. of training points

n_iter = rep ( 0, 1, n_runs )
n_acc = rep ( 0, 1, n_runs )

for( n in 1 : n_runs ) {
  
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
  
  train_data <- data.frame(x1_train, x2_train, y_train)
  lm1 <- lm(y_train ~ (x1_train + x2_train), data=train_data)
  y_pred_train <- predict(lm1, train_data)
  y_pred <- ifelse ( y_pred_train >= 0, 1, -1 )
  error <- abs(y_pred - y_train)/2
  
  x1_train <- runif ( 1000, min = -1, max = 1 )
  x2_train <- runif ( 1000, min = -1, max = 1 ) 
  y_train_out <- x2_train - ( x1_target[2] + m*( x1_train - x1_target[1] ))
  y_train_out <- ifelse ( y_train >= 0, 1, -1 )
  train_data_out <- data.frame(x1_train, x2_train, y_train_out)
  y_pred_train_out <- predict(lm1, train_data_out)
  y_pred_out <- ifelse ( y_pred_train_out >= 0, 1, -1 )
  error_1 <- abs(y_pred_out - y_train_out)/2
  
  n_acc[n] <- mean(error_1)
}

print ( mean ( n_acc ) )
#print ( mean ( n_iter ) )
