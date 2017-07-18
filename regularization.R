setwd("//home//nitish//Desktop//COURSES//Learning From Data")

data_in <- read.csv("in_data.txt", sep="\t", header=FALSE)
data_out <- read.csv("out_data.txt", sep="\t", header=FALSE)
colnames <- c("x1", "x2", "y")

names(data_in) <- colnames
names(data_out) <- colnames

data_in$x1_2 <- (data_in$x1)^2
data_in$x2_2 <- (data_in$x2)^2
data_in$x1_x2 <- (data_in$x1)*(data_in$x2)
data_in$mod_x1_neg_x2 <- abs(data_in$x1 - data_in$x2)
data_in$mod_x1_pos_x2 <- abs(data_in$x1 + data_in$x2)

data_out$x1_2 <- (data_out$x1)^2
data_out$x2_2 <- (data_out$x2)^2
data_out$x1_x2 <- (data_out$x1)*(data_out$x2)
data_out$mod_x1_neg_x2 <- abs(data_out$x1 - data_out$x2)
data_out$mod_x1_pos_x2 <- abs(data_out$x1 + data_out$x2)

# linear regression
lin_model <- lm(formula = y ~ ., data=data_in)
data_in$y_pred_lm <- predict.lm(lin_model, data_in)
data_in$y_pred <- ifelse( data_in$y_pred_lm >= 0, 1, -1)
data_in$mis_ind <- ifelse( data_in$y_pred == data_in$y, 0, 1)

data_out$y_pred_lm <- predict.lm(lin_model, data_out)
data_out$y_pred <- ifelse( data_out$y_pred_lm >= 0, 1, -1)
data_out$mis_ind <- ifelse( data_out$y_pred == data_out$y, 0, 1)

# "x1", "x2", "x1_2", "x2_2", "x1_x2", "mod_x1_neg_x2", "mod_x1_pos_x2", "const"
((ginv((t(z))%*%z))%*%(t(z)))%*%y

reg_coeff = (ginv(((t(z))%*%z) + 0.001*id)%*%(t(z)))%*%y

((ginv((t(z))%*%z) + 0.001*id)%*%(t(z)))%*%y

data_in$y_regl_lm = reg_coeff[1]*data_in$x1 + reg_coeff[2]*data_in$x2 + reg_coeff[3]*data_in$x1_2 + 
  reg_coeff[4]*data_in$x2_2 + reg_coeff[5]*data_in$x1_x2 + reg_coeff[6]*data_in$mod_x1_neg_x2 + 
  reg_coeff[7]*data_in$mod_x1_pos_x2 + reg_coeff[8]*data_in$const

data_in$y_pred_regl <- ifelse( data_in$y_regl_lm >= 0, 1, -1)
data_in$mis_ind_regl <- ifelse( data_in$y_pred_regl == data_in$y, 0, 1)

reg_coeff = (ginv(((t(z))%*%z) + 0.01*id)%*%(t(z)))%*%y
data_out$y_regl_lm = reg_coeff[1]*data_out$x1 + reg_coeff[2]*data_out$x2 + reg_coeff[3]*data_out$x1_2 + 
  reg_coeff[4]*data_out$x2_2 + reg_coeff[5]*data_out$x1_x2 + reg_coeff[6]*data_out$mod_x1_neg_x2 + 
  reg_coeff[7]*data_out$mod_x1_pos_x2 + reg_coeff[8]*data_out$const
data_out$y_pred_regl <- ifelse( data_out$y_regl_lm >= 0, 1, -1)
data_out$mis_ind_regl <- ifelse( data_out$y_pred_regl == data_out$y, 0, 1)
mean(data_out$mis_ind_regl)
