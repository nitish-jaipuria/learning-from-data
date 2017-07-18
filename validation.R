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

data_in_train = data_in[1:25,]
data_in_val = data_in[26:35,]

lin_model <- lm(formula = y ~ ., data=data_in)
data_in$y_pred_lm <- predict.lm(lin_model, data_in)
data_in$y_pred <- ifelse( data_in$y_pred_lm >= 0, 1, -1)
data_in$mis_ind <- ifelse( data_in$y_pred == data_in$y, 0, 1)

vars = c("y", "x1", "x2", "x1_2", "x2_2", "x1_x2", "mod_x1_neg_x2", "mod_x1_pos_x2")
data_t = data_in_train[, vars]
data_v = data_in_val[, vars]
lin_model <- lm(formula = y ~ ., data=data_t)
data_v$y_pred_lm <- predict.lm(lin_model, data_v)
data_v$y_pred <- ifelse( data_v$y_pred_lm >= 0, 1, -1)
data_v$mis_ind <- ifelse( data_v$y_pred == data_v$y, 0, 1)
mean(data_v$mis_ind)

vars = c("y", "x1", "x2", "x1_2")
data_t = data_in_train[, vars]
data_v = data_out[, vars]
lin_model <- lm(formula = y ~ ., data=data_t)
data_v$y_pred_lm <- predict.lm(lin_model, data_v)
data_v$y_pred <- ifelse( data_v$y_pred_lm >= 0, 1, -1)
data_v$mis_ind <- ifelse( data_v$y_pred == data_v$y, 0, 1)
mean(data_v$mis_ind)

data_in_val = data_in[1:25,]
data_in_train = data_in[26:35,]

vars = c("y", "x1", "x2", "x1_2")
data_t = data_in_train[, vars]
data_v = data_in_val[, vars]
lin_model <- lm(formula = y ~ ., data=data_t)
data_v$y_pred_lm <- predict.lm(lin_model, data_v)
data_v$y_pred <- ifelse( data_v$y_pred_lm >= 0, 1, -1)
data_v$mis_ind <- ifelse( data_v$y_pred == data_v$y, 0, 1)
mean(data_v$mis_ind)

vars = c("y", "x1", "x2", "x1_2")
data_t = data_in_train[, vars]
data_v = data_out[, vars]
lin_model <- lm(formula = y ~ ., data=data_t)
data_v$y_pred_lm <- predict.lm(lin_model, data_v)
data_v$y_pred <- ifelse( data_v$y_pred_lm >= 0, 1, -1)
data_v$mis_ind <- ifelse( data_v$y_pred == data_v$y, 0, 1)
mean(data_v$mis_ind)

vars = c("y", "x1", "x2", "x1_2", "x2_2", "x1_x2", "mod_x1_neg_x2")
data_t = data_in_train[, vars]
data_v = data_out[, vars]
lin_model <- lm(formula = y ~ ., data=data_t)
data_v$y_pred_lm <- predict.lm(lin_model, data_v)
data_v$y_pred <- ifelse( data_v$y_pred_lm >= 0, 1, -1)
data_v$mis_ind <- ifelse( data_v$y_pred == data_v$y, 0, 1)
mean(data_v$mis_ind)

e1 = runif( 1000, min = 0, max = 1 )
e2 = runif( 1000, min = 0, max = 1 )
ind = which(e1 < e2)
e = e1
e[ind] = e2[ind]