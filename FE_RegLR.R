library (e1071)

# data input
train_data = read.csv("//home//nitish//Desktop//COURSES//Learning From Data//train_data_svm_rbf.txt", sep = "\t")
test_data = read.csv("//home//nitish//Desktop//COURSES//Learning From Data//test_data_svm_rbf.txt", sep = "\t")
header = c("digit", "intensity", "symmetry")

colnames(train_data) <- header
colnames(test_data) <- header

train_data$dep_var_0 = ifelse ( train_data$digit == 0, 1, -1 )
train_data$dep_var_1 = ifelse ( train_data$digit == 1, 1, -1 )
train_data$dep_var_2 = ifelse ( train_data$digit == 2, 1, -1 )
train_data$dep_var_3 = ifelse ( train_data$digit == 3, 1, -1 )
train_data$dep_var_4 = ifelse ( train_data$digit == 4, 1, -1 )
train_data$dep_var_5 = ifelse ( train_data$digit == 5, 1, -1 )
train_data$dep_var_6 = ifelse ( train_data$digit == 6, 1, -1 )
train_data$dep_var_7 = ifelse ( train_data$digit == 7, 1, -1 )
train_data$dep_var_8 = ifelse ( train_data$digit == 8, 1, -1 )
train_data$dep_var_9 = ifelse ( train_data$digit == 9, 1, -1 )

test_data$dep_var_0 = ifelse ( test_data$digit == 0, 1, -1 )
test_data$dep_var_1 = ifelse ( test_data$digit == 1, 1, -1 )
test_data$dep_var_2 = ifelse ( test_data$digit == 2, 1, -1 )
test_data$dep_var_3 = ifelse ( test_data$digit == 3, 1, -1 )
test_data$dep_var_4 = ifelse ( test_data$digit == 4, 1, -1 )
test_data$dep_var_5 = ifelse ( test_data$digit == 5, 1, -1 )
test_data$dep_var_6 = ifelse ( test_data$digit == 6, 1, -1 )
test_data$dep_var_7 = ifelse ( test_data$digit == 7, 1, -1 )
test_data$dep_var_8 = ifelse ( test_data$digit == 8, 1, -1 )
test_data$dep_var_9 = ifelse ( test_data$digit == 9, 1, -1 )

getWeights <- function (x_df, y_df, param) {
  x_df$constant = ifelse ( 1<2, 1, 0)
  x_mat = as.matrix(x_df)
  y_mat = as.matrix(y_df)
  i_mat = diag(length(x_df))
  
  w_reg = ( solve ( ( t(x_mat) %*% x_mat ) + param*i_mat ) %*% ( t(x_mat) ) %*% y_mat )
  return ( w_reg )
  
}

w_reg_0 = getWeights ( train_data[, c("intensity", "symmetry")],  train_data[, c("dep_var_0")], 1)
w_reg_1 = getWeights ( train_data[, c("intensity", "symmetry")],  train_data[, c("dep_var_1")], 1)
w_reg_2 = getWeights ( train_data[, c("intensity", "symmetry")],  train_data[, c("dep_var_2")], 1)
w_reg_3 = getWeights ( train_data[, c("intensity", "symmetry")],  train_data[, c("dep_var_3")], 1)
w_reg_4 = getWeights ( train_data[, c("intensity", "symmetry")],  train_data[, c("dep_var_4")], 1)
w_reg_5 = getWeights ( train_data[, c("intensity", "symmetry")],  train_data[, c("dep_var_5")], 1)
w_reg_6 = getWeights ( train_data[, c("intensity", "symmetry")],  train_data[, c("dep_var_6")], 1)
w_reg_7 = getWeights ( train_data[, c("intensity", "symmetry")],  train_data[, c("dep_var_7")], 1)
w_reg_8 = getWeights ( train_data[, c("intensity", "symmetry")],  train_data[, c("dep_var_8")], 1)
w_reg_9 = getWeights ( train_data[, c("intensity", "symmetry")],  train_data[, c("dep_var_9")], 1)

train_data$constant = ifelse ( 1<2, 1, 0)

train_data$pred_0 = 
  ifelse ( w_reg_0[c("constant"),]*train_data$constant + w_reg_0[c("intensity"),]*train_data$intensity + w_reg_0[c("symmetry"),]*train_data$symmetry > 0, 1, -1)
train_data$pred_1 = 
  ifelse ( w_reg_1[c("constant"),]*train_data$constant + w_reg_1[c("intensity"),]*train_data$intensity + w_reg_1[c("symmetry"),]*train_data$symmetry > 0, 1, -1)
train_data$pred_2 = 
  ifelse ( w_reg_2[c("constant"),]*train_data$constant + w_reg_2[c("intensity"),]*train_data$intensity + w_reg_2[c("symmetry"),]*train_data$symmetry > 0, 1, -1)
train_data$pred_3 = 
  ifelse ( w_reg_3[c("constant"),]*train_data$constant + w_reg_3[c("intensity"),]*train_data$intensity + w_reg_3[c("symmetry"),]*train_data$symmetry > 0, 1, -1)
train_data$pred_4 = 
  ifelse ( w_reg_4[c("constant"),]*train_data$constant + w_reg_4[c("intensity"),]*train_data$intensity + w_reg_4[c("symmetry"),]*train_data$symmetry > 0, 1, -1)
train_data$pred_5 = 
  ifelse ( w_reg_5[c("constant"),]*train_data$constant + w_reg_5[c("intensity"),]*train_data$intensity + w_reg_5[c("symmetry"),]*train_data$symmetry > 0, 1, -1)
train_data$pred_6 = 
  ifelse ( w_reg_6[c("constant"),]*train_data$constant + w_reg_6[c("intensity"),]*train_data$intensity + w_reg_6[c("symmetry"),]*train_data$symmetry > 0, 1, -1)
train_data$pred_7 = 
  ifelse ( w_reg_7[c("constant"),]*train_data$constant + w_reg_7[c("intensity"),]*train_data$intensity + w_reg_7[c("symmetry"),]*train_data$symmetry > 0, 1, -1)
train_data$pred_8 = 
  ifelse ( w_reg_8[c("constant"),]*train_data$constant + w_reg_8[c("intensity"),]*train_data$intensity + w_reg_8[c("symmetry"),]*train_data$symmetry > 0, 1, -1)
train_data$pred_9 = 
  ifelse ( w_reg_9[c("constant"),]*train_data$constant + w_reg_9[c("intensity"),]*train_data$intensity + w_reg_9[c("symmetry"),]*train_data$symmetry > 0, 1, -1)



train_data$misc_0 = ifelse ( train_data$pred_0 == train_data$dep_var_0 , 0, 1)
train_data$misc_1 = ifelse ( train_data$pred_1 == train_data$dep_var_1 , 0, 1)
train_data$misc_2 = ifelse ( train_data$pred_2 == train_data$dep_var_2 , 0, 1)
train_data$misc_3 = ifelse ( train_data$pred_3 == train_data$dep_var_3 , 0, 1)
train_data$misc_4 = ifelse ( train_data$pred_4 == train_data$dep_var_4 , 0, 1)
train_data$misc_5 = ifelse ( train_data$pred_5 == train_data$dep_var_5 , 0, 1)
train_data$misc_6 = ifelse ( train_data$pred_6 == train_data$dep_var_6 , 0, 1)
train_data$misc_7 = ifelse ( train_data$pred_7 == train_data$dep_var_7 , 0, 1)
train_data$misc_8 = ifelse ( train_data$pred_8 == train_data$dep_var_8 , 0, 1)
train_data$misc_9 = ifelse ( train_data$pred_9 == train_data$dep_var_9 , 0, 1)

test_data$constant = ifelse ( 1<2, 1, 0)

test_data$pred_0 = 
  ifelse ( w_reg_0[c("constant"),]*test_data$constant + w_reg_0[c("intensity"),]*test_data$intensity + w_reg_0[c("symmetry"),]*test_data$symmetry > 0, 1, -1)
test_data$pred_1 = 
  ifelse ( w_reg_1[c("constant"),]*test_data$constant + w_reg_1[c("intensity"),]*test_data$intensity + w_reg_1[c("symmetry"),]*test_data$symmetry > 0, 1, -1)
test_data$pred_2 = 
  ifelse ( w_reg_2[c("constant"),]*test_data$constant + w_reg_2[c("intensity"),]*test_data$intensity + w_reg_2[c("symmetry"),]*test_data$symmetry > 0, 1, -1)
test_data$pred_3 = 
  ifelse ( w_reg_3[c("constant"),]*test_data$constant + w_reg_3[c("intensity"),]*test_data$intensity + w_reg_3[c("symmetry"),]*test_data$symmetry > 0, 1, -1)
test_data$pred_4 = 
  ifelse ( w_reg_4[c("constant"),]*test_data$constant + w_reg_4[c("intensity"),]*test_data$intensity + w_reg_4[c("symmetry"),]*test_data$symmetry > 0, 1, -1)
test_data$pred_5 = 
  ifelse ( w_reg_5[c("constant"),]*test_data$constant + w_reg_5[c("intensity"),]*test_data$intensity + w_reg_5[c("symmetry"),]*test_data$symmetry > 0, 1, -1)
test_data$pred_6 = 
  ifelse ( w_reg_6[c("constant"),]*test_data$constant + w_reg_6[c("intensity"),]*test_data$intensity + w_reg_6[c("symmetry"),]*test_data$symmetry > 0, 1, -1)
test_data$pred_7 = 
  ifelse ( w_reg_7[c("constant"),]*test_data$constant + w_reg_7[c("intensity"),]*test_data$intensity + w_reg_7[c("symmetry"),]*test_data$symmetry > 0, 1, -1)
test_data$pred_8 = 
  ifelse ( w_reg_8[c("constant"),]*test_data$constant + w_reg_8[c("intensity"),]*test_data$intensity + w_reg_8[c("symmetry"),]*test_data$symmetry > 0, 1, -1)
test_data$pred_9 = 
  ifelse ( w_reg_9[c("constant"),]*test_data$constant + w_reg_9[c("intensity"),]*test_data$intensity + w_reg_9[c("symmetry"),]*test_data$symmetry > 0, 1, -1)



test_data$misc_0 = ifelse ( test_data$pred_0 == test_data$dep_var_0 , 0, 1)
test_data$misc_1 = ifelse ( test_data$pred_1 == test_data$dep_var_1 , 0, 1)
test_data$misc_2 = ifelse ( test_data$pred_2 == test_data$dep_var_2 , 0, 1)
test_data$misc_3 = ifelse ( test_data$pred_3 == test_data$dep_var_3 , 0, 1)
test_data$misc_4 = ifelse ( test_data$pred_4 == test_data$dep_var_4 , 0, 1)
test_data$misc_5 = ifelse ( test_data$pred_5 == test_data$dep_var_5 , 0, 1)
test_data$misc_6 = ifelse ( test_data$pred_6 == test_data$dep_var_6 , 0, 1)
test_data$misc_7 = ifelse ( test_data$pred_7 == test_data$dep_var_7 , 0, 1)
test_data$misc_8 = ifelse ( test_data$pred_8 == test_data$dep_var_8 , 0, 1)
test_data$misc_9 = ifelse ( test_data$pred_9 == test_data$dep_var_9 , 0, 1)

#### Transformations ####

train_data$intensity_2 = ( train_data$intensity )^2
train_data$symmetry_2 = ( train_data$intensity )^2
train_data$intensity_symmetry = ( train_data$intensity )*( train_data$symmetry )

w_reg_t_0 = getWeights ( train_data[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data[, c("dep_var_0")], 1)
w_reg_t_1 = getWeights ( train_data[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data[, c("dep_var_1")], 1)
w_reg_t_2 = getWeights ( train_data[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data[, c("dep_var_2")], 1)
w_reg_t_3 = getWeights ( train_data[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data[, c("dep_var_3")], 1)
w_reg_t_4 = getWeights ( train_data[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data[, c("dep_var_4")], 1)
w_reg_t_5 = getWeights ( train_data[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data[, c("dep_var_5")], 1)
w_reg_t_6 = getWeights ( train_data[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data[, c("dep_var_6")], 1)
w_reg_t_7 = getWeights ( train_data[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data[, c("dep_var_7")], 1)
w_reg_t_8 = getWeights ( train_data[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data[, c("dep_var_8")], 1)
w_reg_t_9 = getWeights ( train_data[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data[, c("dep_var_9")], 1)


train_data$pred_t_0 = 
  ifelse ( w_reg_t_0[c("constant"),]*train_data$constant + 
             w_reg_t_0[c("intensity"),]*train_data$intensity + 
             w_reg_t_0[c("symmetry"),]*train_data$symmetry +
             w_reg_t_0[c("intensity_2"),]*train_data$intensity_2 +
             w_reg_t_0[c("symmetry_2"),]*train_data$symmetry_2 +
             w_reg_t_0[c("intensity_symmetry"),]*train_data$intensity_symmetry > 0, 1, -1)

train_data$pred_t_1 = 
  ifelse ( w_reg_t_1[c("constant"),]*train_data$constant + 
             w_reg_t_1[c("intensity"),]*train_data$intensity + 
             w_reg_t_1[c("symmetry"),]*train_data$symmetry +
             w_reg_t_1[c("intensity_2"),]*train_data$intensity_2 +
             w_reg_t_1[c("symmetry_2"),]*train_data$symmetry_2 +
             w_reg_t_1[c("intensity_symmetry"),]*train_data$intensity_symmetry > 0, 1, -1)

train_data$pred_t_2 = 
  ifelse ( w_reg_t_2[c("constant"),]*train_data$constant + 
             w_reg_t_2[c("intensity"),]*train_data$intensity + 
             w_reg_t_2[c("symmetry"),]*train_data$symmetry +
             w_reg_t_2[c("intensity_2"),]*train_data$intensity_2 +
             w_reg_t_2[c("symmetry_2"),]*train_data$symmetry_2 +
             w_reg_t_2[c("intensity_symmetry"),]*train_data$intensity_symmetry > 0, 1, -1)

train_data$pred_t_3 = 
  ifelse ( w_reg_t_3[c("constant"),]*train_data$constant + 
             w_reg_t_3[c("intensity"),]*train_data$intensity + 
             w_reg_t_3[c("symmetry"),]*train_data$symmetry +
             w_reg_t_3[c("intensity_2"),]*train_data$intensity_2 +
             w_reg_t_3[c("symmetry_2"),]*train_data$symmetry_2 +
             w_reg_t_3[c("intensity_symmetry"),]*train_data$intensity_symmetry > 0, 1, -1)

train_data$pred_t_4 = 
  ifelse ( w_reg_t_4[c("constant"),]*train_data$constant + 
             w_reg_t_4[c("intensity"),]*train_data$intensity + 
             w_reg_t_4[c("symmetry"),]*train_data$symmetry +
             w_reg_t_4[c("intensity_2"),]*train_data$intensity_2 +
             w_reg_t_4[c("symmetry_2"),]*train_data$symmetry_2 +
             w_reg_t_4[c("intensity_symmetry"),]*train_data$intensity_symmetry > 0, 1, -1)

train_data$pred_t_5 = 
  ifelse ( w_reg_t_5[c("constant"),]*train_data$constant + 
             w_reg_t_5[c("intensity"),]*train_data$intensity + 
             w_reg_t_5[c("symmetry"),]*train_data$symmetry +
             w_reg_t_5[c("intensity_2"),]*train_data$intensity_2 +
             w_reg_t_5[c("symmetry_2"),]*train_data$symmetry_2 +
             w_reg_t_5[c("intensity_symmetry"),]*train_data$intensity_symmetry > 0, 1, -1)

train_data$pred_t_6 = 
  ifelse ( w_reg_t_6[c("constant"),]*train_data$constant + 
             w_reg_t_6[c("intensity"),]*train_data$intensity + 
             w_reg_t_6[c("symmetry"),]*train_data$symmetry +
             w_reg_t_6[c("intensity_2"),]*train_data$intensity_2 +
             w_reg_t_6[c("symmetry_2"),]*train_data$symmetry_2 +
             w_reg_t_6[c("intensity_symmetry"),]*train_data$intensity_symmetry > 0, 1, -1)

train_data$pred_t_7 = 
  ifelse ( w_reg_t_7[c("constant"),]*train_data$constant + 
             w_reg_t_7[c("intensity"),]*train_data$intensity + 
             w_reg_t_7[c("symmetry"),]*train_data$symmetry +
             w_reg_t_7[c("intensity_2"),]*train_data$intensity_2 +
             w_reg_t_7[c("symmetry_2"),]*train_data$symmetry_2 +
             w_reg_t_7[c("intensity_symmetry"),]*train_data$intensity_symmetry > 0, 1, -1)

train_data$pred_t_8 = 
  ifelse ( w_reg_t_8[c("constant"),]*train_data$constant + 
             w_reg_t_8[c("intensity"),]*train_data$intensity + 
             w_reg_t_8[c("symmetry"),]*train_data$symmetry +
             w_reg_t_8[c("intensity_2"),]*train_data$intensity_2 +
             w_reg_t_8[c("symmetry_2"),]*train_data$symmetry_2 +
             w_reg_t_8[c("intensity_symmetry"),]*train_data$intensity_symmetry > 0, 1, -1)

train_data$pred_t_9 = 
  ifelse ( w_reg_t_9[c("constant"),]*train_data$constant + 
             w_reg_t_9[c("intensity"),]*train_data$intensity + 
             w_reg_t_9[c("symmetry"),]*train_data$symmetry +
             w_reg_t_9[c("intensity_2"),]*train_data$intensity_2 +
             w_reg_t_9[c("symmetry_2"),]*train_data$symmetry_2 +
             w_reg_t_9[c("intensity_symmetry"),]*train_data$intensity_symmetry > 0, 1, -1)

train_data$misc_t_0 = ifelse ( train_data$pred_t_0 == train_data$dep_var_0 , 0, 1)
train_data$misc_t_1 = ifelse ( train_data$pred_t_1 == train_data$dep_var_1 , 0, 1)
train_data$misc_t_2 = ifelse ( train_data$pred_t_2 == train_data$dep_var_2 , 0, 1)
train_data$misc_t_3 = ifelse ( train_data$pred_t_3 == train_data$dep_var_3 , 0, 1)
train_data$misc_t_4 = ifelse ( train_data$pred_t_4 == train_data$dep_var_4 , 0, 1)
train_data$misc_t_5 = ifelse ( train_data$pred_t_5 == train_data$dep_var_5 , 0, 1)
train_data$misc_t_6 = ifelse ( train_data$pred_t_6 == train_data$dep_var_6 , 0, 1)
train_data$misc_t_7 = ifelse ( train_data$pred_t_7 == train_data$dep_var_7 , 0, 1)
train_data$misc_t_8 = ifelse ( train_data$pred_t_8 == train_data$dep_var_8 , 0, 1)
train_data$misc_t_9 = ifelse ( train_data$pred_t_9 == train_data$dep_var_9 , 0, 1)

test_data$intensity_2 = ( test_data$intensity )^2
test_data$symmetry_2 = ( test_data$intensity )^2
test_data$intensity_symmetry = ( test_data$intensity )*( test_data$symmetry )

test_data$pred_t_0 = 
  ifelse ( w_reg_t_0[c("constant"),]*test_data$constant + 
             w_reg_t_0[c("intensity"),]*test_data$intensity + 
             w_reg_t_0[c("symmetry"),]*test_data$symmetry +
             w_reg_t_0[c("intensity_2"),]*test_data$intensity_2 +
             w_reg_t_0[c("symmetry_2"),]*test_data$symmetry_2 +
             w_reg_t_0[c("intensity_symmetry"),]*test_data$intensity_symmetry > 0, 1, -1)

test_data$pred_t_1 = 
  ifelse ( w_reg_t_1[c("constant"),]*test_data$constant + 
             w_reg_t_1[c("intensity"),]*test_data$intensity + 
             w_reg_t_1[c("symmetry"),]*test_data$symmetry +
             w_reg_t_1[c("intensity_2"),]*test_data$intensity_2 +
             w_reg_t_1[c("symmetry_2"),]*test_data$symmetry_2 +
             w_reg_t_1[c("intensity_symmetry"),]*test_data$intensity_symmetry > 0, 1, -1)

test_data$pred_t_2 = 
  ifelse ( w_reg_t_2[c("constant"),]*test_data$constant + 
             w_reg_t_2[c("intensity"),]*test_data$intensity + 
             w_reg_t_2[c("symmetry"),]*test_data$symmetry +
             w_reg_t_2[c("intensity_2"),]*test_data$intensity_2 +
             w_reg_t_2[c("symmetry_2"),]*test_data$symmetry_2 +
             w_reg_t_2[c("intensity_symmetry"),]*test_data$intensity_symmetry > 0, 1, -1)

test_data$pred_t_3 = 
  ifelse ( w_reg_t_3[c("constant"),]*test_data$constant + 
             w_reg_t_3[c("intensity"),]*test_data$intensity + 
             w_reg_t_3[c("symmetry"),]*test_data$symmetry +
             w_reg_t_3[c("intensity_2"),]*test_data$intensity_2 +
             w_reg_t_3[c("symmetry_2"),]*test_data$symmetry_2 +
             w_reg_t_3[c("intensity_symmetry"),]*test_data$intensity_symmetry > 0, 1, -1)

test_data$pred_t_4 = 
  ifelse ( w_reg_t_4[c("constant"),]*test_data$constant + 
             w_reg_t_4[c("intensity"),]*test_data$intensity + 
             w_reg_t_4[c("symmetry"),]*test_data$symmetry +
             w_reg_t_4[c("intensity_2"),]*test_data$intensity_2 +
             w_reg_t_4[c("symmetry_2"),]*test_data$symmetry_2 +
             w_reg_t_4[c("intensity_symmetry"),]*test_data$intensity_symmetry > 0, 1, -1)

test_data$pred_t_5 = 
  ifelse ( w_reg_t_5[c("constant"),]*test_data$constant + 
             w_reg_t_5[c("intensity"),]*test_data$intensity + 
             w_reg_t_5[c("symmetry"),]*test_data$symmetry +
             w_reg_t_5[c("intensity_2"),]*test_data$intensity_2 +
             w_reg_t_5[c("symmetry_2"),]*test_data$symmetry_2 +
             w_reg_t_5[c("intensity_symmetry"),]*test_data$intensity_symmetry > 0, 1, -1)

test_data$pred_t_6 = 
  ifelse ( w_reg_t_6[c("constant"),]*test_data$constant + 
             w_reg_t_6[c("intensity"),]*test_data$intensity + 
             w_reg_t_6[c("symmetry"),]*test_data$symmetry +
             w_reg_t_6[c("intensity_2"),]*test_data$intensity_2 +
             w_reg_t_6[c("symmetry_2"),]*test_data$symmetry_2 +
             w_reg_t_6[c("intensity_symmetry"),]*test_data$intensity_symmetry > 0, 1, -1)

test_data$pred_t_7 = 
  ifelse ( w_reg_t_7[c("constant"),]*test_data$constant + 
             w_reg_t_7[c("intensity"),]*test_data$intensity + 
             w_reg_t_7[c("symmetry"),]*test_data$symmetry +
             w_reg_t_7[c("intensity_2"),]*test_data$intensity_2 +
             w_reg_t_7[c("symmetry_2"),]*test_data$symmetry_2 +
             w_reg_t_7[c("intensity_symmetry"),]*test_data$intensity_symmetry > 0, 1, -1)

test_data$pred_t_8 = 
  ifelse ( w_reg_t_8[c("constant"),]*test_data$constant + 
             w_reg_t_8[c("intensity"),]*test_data$intensity + 
             w_reg_t_8[c("symmetry"),]*test_data$symmetry +
             w_reg_t_8[c("intensity_2"),]*test_data$intensity_2 +
             w_reg_t_8[c("symmetry_2"),]*test_data$symmetry_2 +
             w_reg_t_8[c("intensity_symmetry"),]*test_data$intensity_symmetry > 0, 1, -1)

test_data$pred_t_9 = 
  ifelse ( w_reg_t_9[c("constant"),]*test_data$constant + 
             w_reg_t_9[c("intensity"),]*test_data$intensity + 
             w_reg_t_9[c("symmetry"),]*test_data$symmetry +
             w_reg_t_9[c("intensity_2"),]*test_data$intensity_2 +
             w_reg_t_9[c("symmetry_2"),]*test_data$symmetry_2 +
             w_reg_t_9[c("intensity_symmetry"),]*test_data$intensity_symmetry > 0, 1, -1)

test_data$misc_t_0 = ifelse ( test_data$pred_t_0 == test_data$dep_var_0 , 0, 1)
test_data$misc_t_1 = ifelse ( test_data$pred_t_1 == test_data$dep_var_1 , 0, 1)
test_data$misc_t_2 = ifelse ( test_data$pred_t_2 == test_data$dep_var_2 , 0, 1)
test_data$misc_t_3 = ifelse ( test_data$pred_t_3 == test_data$dep_var_3 , 0, 1)
test_data$misc_t_4 = ifelse ( test_data$pred_t_4 == test_data$dep_var_4 , 0, 1)
test_data$misc_t_5 = ifelse ( test_data$pred_t_5 == test_data$dep_var_5 , 0, 1)
test_data$misc_t_6 = ifelse ( test_data$pred_t_6 == test_data$dep_var_6 , 0, 1)
test_data$misc_t_7 = ifelse ( test_data$pred_t_7 == test_data$dep_var_7 , 0, 1)
test_data$misc_t_8 = ifelse ( test_data$pred_t_8 == test_data$dep_var_8 , 0, 1)
test_data$misc_t_9 = ifelse ( test_data$pred_t_9 == test_data$dep_var_9 , 0, 1)

mean_ein = c(mean(train_data$misc_0), 
             mean(train_data$misc_1), 
             mean(train_data$misc_2), 
             mean(train_data$misc_3), 
             mean(train_data$misc_4), 
             mean(train_data$misc_5), 
             mean(train_data$misc_6), 
             mean(train_data$misc_7), 
             mean(train_data$misc_8), 
             mean(train_data$misc_9))

mean_eout = c(mean(test_data$misc_0), 
             mean(test_data$misc_1), 
             mean(test_data$misc_2), 
             mean(test_data$misc_3), 
             mean(test_data$misc_4), 
             mean(test_data$misc_5), 
             mean(test_data$misc_6), 
             mean(test_data$misc_7), 
             mean(test_data$misc_8), 
             mean(test_data$misc_9))

mean_ein_t = c(mean(train_data$misc_t_0), 
             mean(train_data$misc_t_1), 
             mean(train_data$misc_t_2), 
             mean(train_data$misc_t_3), 
             mean(train_data$misc_t_4), 
             mean(train_data$misc_t_5), 
             mean(train_data$misc_t_6), 
             mean(train_data$misc_t_7), 
             mean(train_data$misc_t_8), 
             mean(train_data$misc_t_9))

mean_eout_t = c(mean(test_data$misc_t_0), 
              mean(test_data$misc_t_1), 
              mean(test_data$misc_t_2), 
              mean(test_data$misc_t_3), 
              mean(test_data$misc_t_4), 
              mean(test_data$misc_t_5), 
              mean(test_data$misc_t_6), 
              mean(test_data$misc_t_7), 
              mean(test_data$misc_t_8), 
              mean(test_data$misc_t_9))

##### 1 vs 5 classifier #####

train_data_1_5 = subset ( train_data , digit == 1 | digit == 5 )
test_data_1_5 = subset ( test_data , digit == 1 | digit == 5 )

train_data_1_5$dep_var_1_5 = ifelse ( train_data_1_5$digit == 1 , 1, -1 )
test_data_1_5$dep_var_1_5 = ifelse ( test_data_1_5$digit == 1 , 1, -1 )

w_1 = getWeights ( train_data_1_5[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data_1_5[, c("dep_var_1_5")], 0.01)
w_2 = getWeights ( train_data_1_5[, c("intensity", "symmetry", "intensity_2", "symmetry_2", "intensity_symmetry")],  train_data_1_5[, c("dep_var_1_5")], 1)

train_data_1_5$pred_1 = 
  ifelse ( w_1[c("constant"),]*train_data_1_5$constant + 
             w_1[c("intensity"),]*train_data_1_5$intensity + 
             w_1[c("symmetry"),]*train_data_1_5$symmetry +
             w_1[c("intensity_2"),]*train_data_1_5$intensity_2 +
             w_1[c("symmetry_2"),]*train_data_1_5$symmetry_2 +
             w_1[c("intensity_symmetry"),]*train_data_1_5$intensity_symmetry > 0, 1, -1)

train_data_1_5$misc_1 = ifelse ( train_data_1_5$pred_1 == train_data_1_5$dep_var_1_5 , 0, 1)

test_data_1_5$pred_1 = 
  ifelse ( w_1[c("constant"),]*test_data_1_5$constant + 
             w_1[c("intensity"),]*test_data_1_5$intensity + 
             w_1[c("symmetry"),]*test_data_1_5$symmetry +
             w_1[c("intensity_2"),]*test_data_1_5$intensity_2 +
             w_1[c("symmetry_2"),]*test_data_1_5$symmetry_2 +
             w_1[c("intensity_symmetry"),]*test_data_1_5$intensity_symmetry > 0, 1, -1)

test_data_1_5$misc_1 = ifelse ( test_data_1_5$pred_1 == test_data_1_5$dep_var_1_5 , 0, 1)


train_data_1_5$pred_2 = 
  ifelse ( w_2[c("constant"),]*train_data_1_5$constant + 
             w_2[c("intensity"),]*train_data_1_5$intensity + 
             w_2[c("symmetry"),]*train_data_1_5$symmetry +
             w_2[c("intensity_2"),]*train_data_1_5$intensity_2 +
             w_2[c("symmetry_2"),]*train_data_1_5$symmetry_2 +
             w_2[c("intensity_symmetry"),]*train_data_1_5$intensity_symmetry > 0, 1, -1)

train_data_1_5$misc_2 = ifelse ( train_data_1_5$pred_2 == train_data_1_5$dep_var_1_5 , 0, 1)

test_data_1_5$pred_2 = 
  ifelse ( w_2[c("constant"),]*test_data_1_5$constant + 
             w_2[c("intensity"),]*test_data_1_5$intensity + 
             w_2[c("symmetry"),]*test_data_1_5$symmetry +
             w_2[c("intensity_2"),]*test_data_1_5$intensity_2 +
             w_2[c("symmetry_2"),]*test_data_1_5$symmetry_2 +
             w_2[c("intensity_symmetry"),]*test_data_1_5$intensity_symmetry > 0, 1, -1)

test_data_1_5$misc_2 = ifelse ( test_data_1_5$pred_2 == test_data_1_5$dep_var_1_5 , 0, 1)

