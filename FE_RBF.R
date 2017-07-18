library(e1071)

n_pts = 100
n_runs = 0
ein_0_count = 0

kernel_form = 0
reg_form = 0

total_misc_rbf_out = 0
total_misc_rbf_in = 0

rbf <- function( X , Y , K , gamma ) {
  N     <- dim(X)[1] # number of observations
  ncols <- dim(X)[2] # number of variables
  
  repeat {
    km <- kmeans(X, K)  # let's cluster K centers out of the dataset
    if (min(km$size)>0) # only accept if there are no empty clusters
      break
  }
  
  mus <- km$centers # the clusters points
  
  Phi <- matrix(rep(NA,(K+1)*N), ncol=K+1)
  for (lin in 1:N) {
    Phi[lin,1] <- 1    # bias column
    for (col in 1:K) {
      Phi[lin,col+1] <- exp( -gamma * norm(as.matrix(X[lin,]-mus[col,]),"F")^2 )
    }
  }
  
  w <- solve(t(Phi) %*% Phi) %*% t(Phi) %*% Y  # find RBF weights
  
  return ( list(weights=w, centers=mus, gamma=gamma) )
}

rbf_predict <- function ( model , X ) {
  
  gamma   <- model$gamma
  centers <- model$centers
  w       <- model$weights
  N       <- dim(X)[1]    # number of observations
  
  pred <- rep(w[1],N)  # we need to init to a value, so let's start with the bias
  
  for (j in 1:N) {  
    # find prediction for point xj
    for (k in 1:length(centers[,1])) {
      # the weight for center[k] is given by w[k+1] (because w[1] is the bias)
      pred[j] <- pred[j] + w[k+1] * exp( -gamma * norm(as.matrix(X[j,]-centers[k,]),"F")^2 )
    }
  }
  
  pred = ifelse ( pred > 0 , 1 , -1 )
  return ( pred )
}

while ( n_runs < 100 ) {
  
  # Training Data
  x1 <- runif ( n_pts, min = -1, max = 1 )
  x2 <- runif ( n_pts, min = -1, max = 1 )
  y_ <- x2 - x1 + 0.25*sin(pi*x1)
  y <- ifelse ( y_ > 0 , 1 , -1 )
  
  train <- data.frame ( x1 = double(n_pts), x2 = double(n_pts), y = double(n_pts) )
  
  train$x1 = x1
  train$x2 = x2
  train$y = y
  
  # Testing Data
  a1 <- runif ( n_pts, min = -1, max = 1 )
  a2 <- runif ( n_pts, min = -1, max = 1 )
  b_ <- a2 - a1 + 0.25*sin(pi*a1)
  b <- ifelse ( b_ > 0 , 1 , -1 )
  
  test <- data.frame ( x1 = double(n_pts), x2 = double(n_pts), y = double(n_pts) )
  
  test$x1 = a1
  test$x2 = a2
  test$y = b
  
  # RBF Model
  
  n_centres = 9
  gamma_val = 1.5
  w = rbf ( train[, c("x1", "x2")] , train[, c("y")] , n_centres , gamma_val)
  
  train$predict_rbf = rbf_predict ( w , train[, c("x1", "x2")] )
  misc_rbf_in = ifelse ( train$y == train$predict_rbf , 0 , 1)
  
  test$predict_rbf = rbf_predict ( w , test[, c("x1", "x2")] )
  misc_rbf_out = ifelse ( test$y == test$predict_rbf , 0 , 1)
  
  # SVM Model
  
  svm_model = svm ( y ~ x1 + x2 , data = train , scale = FALSE , type = "C-classification" , 
                    kernel = "radial" , gamma = gamma_val , cost = 1000000  )
  
  train$predict_svm = predict( svm_model , train )
  misc_svm_in = ifelse ( train$y == train$predict_svm , 0 , 1)
  
  test$predict_svm = predict( svm_model , test )
  misc_svm_out = ifelse ( test$y == test$predict_svm , 0 , 1)
  
  if ( mean ( misc_svm_out ) < mean ( misc_rbf_out ) ) kernel_form = kernel_form + 1
  else reg_form = reg_form + 1
  
  total_misc_rbf_in = total_misc_rbf_in + mean ( misc_rbf_in )
  total_misc_rbf_out = total_misc_rbf_out + mean ( misc_rbf_out )
  
  if ( mean ( misc_rbf_in ) == 0 ) ein_0_count = ein_0_count + 1
  n_runs = n_runs + 1
}
