library (e1071)

# data input
train_data = read.csv("//home//nitish//Desktop//COURSES//Learning From Data//train_data_svm_rbf.txt", sep = "\t")
test_data = read.csv("//home//nitish//Desktop//COURSES//Learning From Data//test_data_svm_rbf.txt", sep = "\t")
header = c("digit", "intensity", "symmetry")

colnames(train_data) <- header
colnames(test_data) <- header

train_data$dep_var_0 = ifelse ( train_data$digit == 0, 1, 0 )
train_data$dep_var_1 = ifelse ( train_data$digit == 1, 1, 0 )
train_data$dep_var_2 = ifelse ( train_data$digit == 2, 1, 0 )
train_data$dep_var_3 = ifelse ( train_data$digit == 3, 1, 0 )
train_data$dep_var_4 = ifelse ( train_data$digit == 4, 1, 0 )
train_data$dep_var_5 = ifelse ( train_data$digit == 5, 1, 0 )
train_data$dep_var_6 = ifelse ( train_data$digit == 6, 1, 0 )
train_data$dep_var_7 = ifelse ( train_data$digit == 7, 1, 0 )
train_data$dep_var_8 = ifelse ( train_data$digit == 8, 1, 0 )
train_data$dep_var_9 = ifelse ( train_data$digit == 9, 1, 0 )

test_data$dep_var_0 = ifelse ( test_data$digit == 0, 1, 0 )
test_data$dep_var_1 = ifelse ( test_data$digit == 1, 1, 0 )
test_data$dep_var_2 = ifelse ( test_data$digit == 2, 1, 0 )
test_data$dep_var_3 = ifelse ( test_data$digit == 3, 1, 0 )
test_data$dep_var_4 = ifelse ( test_data$digit == 4, 1, 0 )
test_data$dep_var_5 = ifelse ( test_data$digit == 5, 1, 0 )
test_data$dep_var_6 = ifelse ( test_data$digit == 6, 1, 0 )
test_data$dep_var_7 = ifelse ( test_data$digit == 7, 1, 0 )
test_data$dep_var_8 = ifelse ( test_data$digit == 8, 1, 0 )
test_data$dep_var_9 = ifelse ( test_data$digit == 9, 1, 0 )

# problem 2
svm_model_0 = svm ( dep_var_0 ~ intensity + symmetry , data = train_data , 
                    scale = FALSE , type = "C-classification" , 
                    kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                    degree = 2 , cost = 0.01 )

train_data$predict_0 = predict( svm_model_0 , train_data )

svm_model_1 = svm ( dep_var_1 ~ intensity + symmetry , data = train_data , 
                    scale = FALSE , type = "C-classification" , 
                    kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                    degree = 2 , cost = 0.01 )

train_data$predict_1 = predict( svm_model_1 , train_data )

svm_model_2 = svm ( dep_var_2 ~ intensity + symmetry , data = train_data , 
                    scale = FALSE , type = "C-classification" , 
                    kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                    degree = 2 , cost = 0.01 )

train_data$predict_2 = predict( svm_model_2 , train_data )

svm_model_3 = svm ( dep_var_3 ~ intensity + symmetry , data = train_data , 
                    scale = FALSE , type = "C-classification" , 
                    kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                    degree = 2 , cost = 0.01 )

train_data$predict_3 = predict( svm_model_3 , train_data )

svm_model_4 = svm ( dep_var_4 ~ intensity + symmetry , data = train_data , 
                    scale = FALSE , type = "C-classification" , 
                    kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                    degree = 2 , cost = 0.01 )

train_data$predict_4 = predict( svm_model_4 , train_data )

svm_model_5 = svm ( dep_var_5 ~ intensity + symmetry , data = train_data , 
                    scale = FALSE , type = "C-classification" , 
                    kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                    degree = 2 , cost = 0.01 )

train_data$predict_5 = predict( svm_model_5 , train_data )

svm_model_6 = svm ( dep_var_6 ~ intensity + symmetry , data = train_data , 
                    scale = FALSE , type = "C-classification" , 
                    kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                    degree = 2 , cost = 0.01 )

train_data$predict_6 = predict( svm_model_6 , train_data )

svm_model_7 = svm ( dep_var_7 ~ intensity + symmetry , data = train_data , 
                    scale = FALSE , type = "C-classification" , 
                    kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                    degree = 2 , cost = 0.01 )

train_data$predict_7 = predict( svm_model_7 , train_data )

svm_model_8 = svm ( dep_var_8 ~ intensity + symmetry , data = train_data , 
                    scale = FALSE , type = "C-classification" , 
                    kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                    degree = 2 , cost = 0.01 )

train_data$predict_8 = predict( svm_model_8 , train_data )

svm_model_9 = svm ( dep_var_9 ~ intensity + symmetry , data = train_data , 
                    scale = FALSE , type = "C-classification" , 
                    kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                    degree = 2 , cost = 0.01 )

train_data$predict_9 = predict( svm_model_9 , train_data )
  
# misclassification

misc_0 = ifelse ( train_data$dep_var_0 == predict_0 , 0 , 1)
mean(misc_0)

## Answer 5

train_data_1_5 = subset ( train_data , digit == 1 | digit == 5 )
test_data_1_5 = subset ( test_data , digit == 1 | digit == 5 )

train_data_1_5$dep_var_1_5 = ifelse ( train_data_1_5$digit == 1, 1, 0 )
test_data_1_5$dep_var_1_5 = ifelse ( test_data_1_5$digit == 1, 1, 0 )

svm_model_1_5_a = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                    scale = FALSE , type = "C-classification" , 
                    kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                    degree = 2 , cost = 0.001 )

svm_model_1_5_b = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                        scale = FALSE , type = "C-classification" , 
                        kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                        degree = 2 , cost = 0.01 )

svm_model_1_5_c = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                        scale = FALSE , type = "C-classification" , 
                        kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                        degree = 2 , cost = 0.1 )

svm_model_1_5_d = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                        scale = FALSE , type = "C-classification" , 
                        kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                        degree = 2 , cost = 1 )

train_data_1_5$predict_1_5_a = predict( svm_model_1_5_a , train_data_1_5 )
test_data_1_5$predict_1_5_a = predict( svm_model_1_5_a , test_data_1_5 )

misc_1_5_a = ifelse ( train_data_1_5$dep_var_1_5 == train_data_1_5$predict_1_5_a , 0 , 1)
mean ( misc_1_5_a )

## answer 6

svm_model_1_5_a5 = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                        scale = FALSE , type = "C-classification" , 
                        kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                        degree = 5 , cost = 0.001 )

svm_model_1_5_b5 = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                        scale = FALSE , type = "C-classification" , 
                        kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                        degree = 5 , cost = 0.01 )

svm_model_1_5_c5 = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                        scale = FALSE , type = "C-classification" , 
                        kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                        degree = 5 , cost = 0.1 )

svm_model_1_5_d5 = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                        scale = FALSE , type = "C-classification" , 
                        kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                        degree = 5 , cost = 1 )

## Answer 7
for ( n in 1:100 ) {
  svm_model_1_5_a = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                        scale = FALSE , type = "C-classification" , 
                        kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                        degree = 2 , cost = 0.0001  , cross = 10 )
  
  svm_model_1_5_a = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                          scale = FALSE , type = "C-classification" , 
                          kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                          degree = 2 , cost = 0.0001  , cross = 10 )
  
  svm_model_1_5_b = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                          scale = FALSE , type = "C-classification" , 
                          kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                          degree = 2 , cost = 0.001  , cross = 10 )
  
  svm_model_1_5_c = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                          scale = FALSE , type = "C-classification" , 
                          kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                          degree = 2 , cost = 0.01  , cross = 10 )
  
  svm_model_1_5_d = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                          scale = FALSE , type = "C-classification" , 
                          kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                          degree = 2 , cost = 0.1  , cross = 10 )
  
  svm_model_1_5_e = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                          scale = FALSE , type = "C-classification" , 
                          kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                          degree = 2 , cost = 1  , cross = 10 )
}

# answer 8

## Answer 7

c_a = 0
c_b = 0
c_c = 0
c_d = 0
c_e = 0

ecv_best = 0
ecv_best_1 = 0

for ( n in 1:100 ) {
  set.seed(n*5)
  misc_a <- rep(0, times = 10)
  misc_b <- rep(0, times = 10)
  misc_c <- rep(0, times = 10)
  misc_d <- rep(0, times = 10)
  misc_e <- rep(0, times = 10)
  
  for ( i in 1:10 ) {
    perm = sample(nrow(train_data_1_5))
    perm_90pc = perm[1:floor(0.9*nrow(train_data_1_5))]
    train_cv = train_data_1_5[c(perm_90pc),]
    test_cv = train_data_1_5[-c(perm_90pc),]
    
    svm_model_cv_a = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_cv , 
                           scale = FALSE , type = "C-classification" , 
                           kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                           degree = 2 , cost = 0.0001 )
    
    svm_model_cv_b = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_cv , 
                           scale = FALSE , type = "C-classification" , 
                           kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                           degree = 2 , cost = 0.001 )
    
    svm_model_cv_c = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_cv , 
                           scale = FALSE , type = "C-classification" , 
                           kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                           degree = 2 , cost = 0.01 )
    
    svm_model_cv_d = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_cv , 
                           scale = FALSE , type = "C-classification" , 
                           kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                           degree = 2 , cost = 0.1 )
    
    svm_model_cv_e = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_cv , 
                           scale = FALSE , type = "C-classification" , 
                           kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                           degree = 2 , cost = 1 )
    
    test_cv$predict_cv_a = predict( svm_model_cv_a , test_cv )
    misc_a[i] = mean ( ifelse ( test_cv$dep_var_1_5 == test_cv$predict_cv_a , 0 , 1) )
    
    test_cv$predict_cv_b = predict( svm_model_cv_b , test_cv )
    misc_b[i] = mean ( ifelse ( test_cv$dep_var_1_5 == test_cv$predict_cv_b , 0 , 1) )
    
    test_cv$predict_cv_c = predict( svm_model_cv_c , test_cv )
    misc_c[i] = mean ( ifelse ( test_cv$dep_var_1_5 == test_cv$predict_cv_c , 0 , 1) )
    
    test_cv$predict_cv_d = predict( svm_model_cv_d , test_cv )
    misc_d[i] = mean ( ifelse ( test_cv$dep_var_1_5 == test_cv$predict_cv_d , 0 , 1) )
    
    test_cv$predict_cv_e = predict( svm_model_cv_e , test_cv )
    misc_e[i] = mean ( ifelse ( test_cv$dep_var_1_5 == test_cv$predict_cv_e , 0 , 1) )
  }
  
  misc_cv_a = mean ( misc_a )
  misc_cv_b = mean ( misc_b )
  misc_cv_c = mean ( misc_c )
  misc_cv_d = mean ( misc_d )
  misc_cv_e = mean ( misc_e )
  
  min_cv = min ( misc_cv_a, misc_cv_b, misc_cv_c, misc_cv_d, misc_cv_e )
  
  ecv_best = ecv_best + misc_cv_b
  ecv_best_1 = ecv_best_1 + min_cv
  
  if ( min_cv == misc_cv_a ) c_a = c_a + 1
  else if ( min_cv == misc_cv_b ) c_b = c_b + 1
  else if ( min_cv == misc_cv_c ) c_c = c_c + 1
  else if ( min_cv == misc_cv_d ) c_d = c_d + 1
  else if ( min_cv == misc_cv_e ) c_e = c_e + 1
}



## Answer 9 and 10
svm_model_1_5_rb = svm ( dep_var_1_5 ~ intensity + symmetry , data = train_data_1_5 , 
                         scale = FALSE , type = "C-classification" , 
                         kernel = "radial" , gamma = 1 ,
                         cost = 1000000 )

train_data_1_5$predict_1_5_rb = predict( svm_model_1_5_rb , train_data_1_5 )
test_data_1_5$predict_1_5_rb = predict( svm_model_1_5_rb , test_data_1_5 )

misc_1_5_rb_in = ifelse ( train_data_1_5$dep_var_1_5 == train_data_1_5$predict_1_5_rb , 0 , 1)
mean ( misc_1_5_rb_in )
misc_1_5_rb_out = ifelse ( test_data_1_5$dep_var_1_5 == test_data_1_5$predict_1_5_rb , 0 , 1)
mean ( misc_1_5_rb_out )
