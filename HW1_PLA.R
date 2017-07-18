# Perceptron Learning Algorithm Simulation

# target function
x1_target <- runif( 2, min = -1, max = 1 )
x2_target <- runif( 2, min = -1, max = 1 )

w <- c(0,0)    # initializing weights
n_pts <- 10   # no. of training points

# generating training points
x1_train <- runif ( n_pts, min = -1, max = 1 )
x2_train <- runif ( n_pts, min = -1, max = 1 ) 

# calculate slope of the target function line
m = (x2_target[2] - x1_target[2])/(x2_target[1] - x1_target[1])
# calculating and flaggin the training data output using the target function
y_train <- x2_train - ( x1_target[2] + m*( x1_train - x1_target[1] ))
y_train <- ifelse ( y_train >= 0, 1, -1 )

miscl_index <- 1
i_iter <- 0

while( i_iter < 100000 && miscl_index <= length(y_train) && miscl_index > 0 ) {
  # current no. of iterations counter
  i_iter <- i_iter + 1
  # calculating and flagging predicted using weights
  y_pred <- w[1] * x1_train + w[2] * x2_train
  y_pred <- ifelse ( y_pred >= 0, 1, -1 )
  
  miscl_index = 0
  j = 1
  
  # getting the first misclassified point in the training set
  while( j <= length(y_pred) && y_train[j] == y_pred[j] )
    j <- j + 1
  miscl_index <- j
  
  # updating the weights using that misclassfied point
  if ( miscl_index <= length(y_pred) && miscl_index > 0 ) { 
    w[1] <- w[1] + y_train[miscl_index] * x1_train[miscl_index]
    w[2] <- w[2] + y_train[miscl_index] * x2_train[miscl_index]
  }
}

accuracy <- mean( ifelse( y_pred == y_train, 1, 0) )

print(accuracy)
print(i_iter)