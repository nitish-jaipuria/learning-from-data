library(e1071)

x1 = c ( 1 , 0 , 0 , -1 , 0 , 0 , -2 )
x2 = c ( 0 , 1 , -1 , 0 , 2 , -2 , 0 )
y = c ( -1 , -1 , -1 , 1 , 1 , 1 , 1 )

train <- data.frame ( x1 = double(7), x2 = double(7), y = double(7) )

train$x1 = x1
train$x2 = x2
train$y = y

svm_model = svm ( y ~ x1 + x2 , data = train , scale = FALSE , type = "C-classification" , 
                        kernel = "polynomial" , gamma = 1 , coef0 = 1 ,
                        degree = 2 , cost = 1000000 )