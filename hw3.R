########
# HW3 
# Instructor: Dr. Thomas Price
# 
# @author: Krishna Gadiraju/kgadira
# Group #5
# Valeri Kozarev
# Nikhil Kaul
# Nick Sherril
#########

# set a seed for reproducibility
set.seed(100)
############################################################################################################
# Helper functions
# TA will use something similar to load data for his own system
# For regression data
load_data <- function(data_folder='./data/', learning_type){
  # this method will read data for regression/classification and return list containing two data frames:
  
  # for regression (specified by learning_type = "regression") 
  # first 19 columns (x1-x19) are attributes, last column (y) is your dependent variable (continuous))
  
  # for classification (specified by learning_type = "classification")
  # first 4 columns (x1-x4) are attributes, last column (class) is your dependent variable (continuous))
  
  # please note, TA WILL use the same training dataset, but a different test set 
  # TA's test set will have the same attributes, but may contain different number of data points
  train_df <- read.csv(paste0(data_folder, learning_type, '-train.csv'), header=T)
  test_df <- read.csv(paste0(data_folder, learning_type, '-test.csv'), header=T)
  
  # make sure dependent variable is of type factor if this is classification
  if(learning_type == 'classification'){
    train_df$class <- as.factor(train_df$class)
    test_df$class <- as.factor(test_df$class)
  }
  return(list(train_df, test_df))
}

# regression data
reg_data <- load_data(data_folder='./data/', learning_type='regression')
reg_train_df <- reg_data[[1]]
reg_test_df <- reg_data[[2]]

# use this one for testing (output on piazza for RMSE)
# preds <- alda_regression(reg_train_df[,1:19], sample_reg_data[,1:19], reg_train_df[,20], "linear")
# calculate_rmse(sample_reg_data[,20], preds)
sample_reg_data <- read.csv("regression_sample_test.csv", header=TRUE)

# Write code for regression here
alda_regression <- function(x_train, x_test, y_train, regression_type){
  # Perform regression (linear/ridge/lasso)
  
  # Inputs:
    # x_train: training data frame(19 variables, x1-x19)
    # x_test: test data frame(19 variables, x1-x19)
    # y_train: dependent variable, training data (vector, continous type)
    # regression_type: specifies type of regression, string variable, can be of type 'linear', 'ridge' or 'lasso'
  
  # General Information:
    # Instructions for specific regression types:
      # linear: no cross validation
      # ridge: use 10-fold cross validation to determine optimal lambda
      # lasso: use 10-fold cross validation to determine optimal lambda
  
  # Output:
    # A list with two elements, first element = model generated, second element = predictions on test data (vector) 
  
  # Function hints: Read the documentation for the functions glmnet, cv.glmnet, predict
  # Ridge and Lasso regression hints: Lambda is the hyperparameter
  if(regression_type == 'linear') { 
    # ~ 2-3 lines of code
    # write code for building a linear regression model using x_train, y_train
    # can you use glmnet to do simple linear regression as well?
    # Explore away!  
    # Hint: Think of what the lambda value means for linear regression without regularization.
    
    myfit <- glmnet(x_train, y_train, lambda=0)
    
    # predict using the model
    myprediction <- predict(myfit, x_test)
    
    # output format
    mylist <- c(myfit, myprediction)
    
    return(mylist)
    
  }else if(regression_type == 'ridge'){
    # ~ 2-3 lines of code
    # write code for ridge regression here
    # 10 fold cross validation, with mse (mean squared error) as the measure
    # the hyperparameter you are tuning here is lambda
    mylambda <- cv.glmnet(x_train, y_train, family = "gaussian", type.measure="mse", nfolds=10, alpha=0)
    myfit <- glmnet(x_train, y_train, family = "gaussian", lambda=mylambda$lambda.min, alpha=0, standardize = TRUE)
    
    # predict on x_test using the model that gives least MSE
    myprediction <- predict(myfit, x_test)
    mylist <- c(myfit, myprediction)
    
    return(mylist)
  
  }else{
    # ~ 2-3 lines of code
    # write code for lasso regression here
    # 10 fold cross validation, with mse (mean squared error) as the measure
    # the hyperparameter you are tuning here is lambda
    mylambda <- cv.glmnet(x_train, y_train, family = "gaussian", type.measure="mse", nfolds=10, alpha=1)
    myfit <- glmnet(x_train, y_train, family = "gaussian", lambda=mylambda$lambda.min, alpha=1, standardize = TRUE)
    
    # predict on x_test using the model that gives least MSE
    myprediction <- predict(mylambda, x_test, s=mylambda$lambda.min)
    mylist <- c(myfit, myprediction)
    
    return(mylist)
  }
  
}

calculate_rmse <- function(y_true, y_pred){
  # DO NOT modify this code. TA has already given you code for this
  # You have already been provided this code to calculate RMSE
  
  # Inputs:
  # y_true: ground truth dependent variable values, of type vector
  # y_pred: prediction outcomes from any regression method, with the same length as y_true
  
  # Outputs:
  # a single value of type double, with the RMSE value
  return(sqrt(sum(y_true - y_pred)^2/length(y_true)))
}

regression_compare_rmse <- function(y_test, linear_regression_prediction, ridge_prediction, lasso_prediction){
  # Calculate the rmse for each of the regression methods: 'linear', 'ridge', 'lasso'
  # Return the best method and its RMSE (i.e., method with least RMSE)
  
  # Inputs:
  # y_test: ground truth dependent variable from test data (vector)
  # linear_regression_prediction: predictions from linear regression (vector)
  # ridge_regression_prediction: predictions from ridge regression (vector)
  # lasso_regression_prediction: predictions from lasso regression (vector)
  
  # Returns:
  # list of three values:
  # First value, of type string, with the name of the best method
  #  'linear' if linear_regression_prediction is best
  #  'ridge' if ridge_prediction is best
  #  'lasso' if lasso_regression is best
  # Second value, of type double, with the corresponding RMSE of the best method (do not round off)
  # third value is a vector of RMSE values, in the following order: c(linear regression's RMSE, ridge regression's RMSE, lasso's RMSE)
  
  # Allowed packages: R-base
  # You are given the implementation for calculate_rmse (see above) 
  
  # find the three RMSE scores
  rmse_linear = calculate_rmse(y_test, linear_regression_prediction)
  rmse_ridge = calculate_rmse(y_test, ridge_prediction)
  rmse_lasso = calculate_rmse(y_test, lasso_prediction)
  
  # get the best one
  scores <- c(rmse_linear, rmse_ridge, rmse_lasso)
  scores <- sort(scores, decreasing=F)
  
  best_rmse = ''
  if (scores[1]==rmse_linear) {
    best_rmse = 'linear'
  } else if (scores[1]==rmse_ridge) {
    best_rmse = 'ridge'
  } else {
    best_rmse = 'lasso'
  }
  
  # output
  mylist = c(best_rmse, scores[1], c(rmse_linear, rmse_ridge, rmse_lasso))
  
  return(mylist)
}


alda_svm <- function(x_train, x_test, y_train, kernel_name){
  # Perform classification using support vector machines (linear/radial/sigmoid)
  
  # Inputs:
    # x_train: training data frame(4 variables, x1-x4)
    # x_test: test data frame(4 variables, x1-x4)
    # y_train: dependent variable, training data (factor)
    # kernel_name: specifies type of SVM kernel, string variable, can be of type 'linear', 'radial' or 'sigmoid' or 'polynomial'

  # General information
    # Both training data and test data have already been scaled - so you don't need to scale it once again.
  
  # Kernel specific information: using 10-fold cross-validation, perform hyperparameter tuning for each kernel as shown below:
    # Linear: 
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10)
    # radial: 
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
    # polynomial:
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
      # 'degree' parameter: for the following values: c(1,2,3)
    # sigmoid:
      # 'cost' parameter: for the following values: c(0.01, 0.1, 1, 10), 
      # 'gamma' parameter: for the following values: c(0.05, 0.5, 1, 2)
  
  # Output:
    # A list with two elements, first element = model generated, second element = predictions on test data (factor) 
  
  # Word of caution:
    # Make sure that you pick the best parameters after tuning
  
  # allowed packages: R-base, e1071
  
  # Hints: See if you can use the 'tune' function in e1071 for cross validation
   if(kernel_name == "radial"){
    # ~1-2 lines 
  
     
     
  }else if(kernel_name == 'polynomial'){
    #~1-2 lines
    
    
    
  }else if(kernel_name == 'sigmoid'){
    #~1-2 lines
    
    
    
  }else{ # default linear kernel
    #~1-2 lines
    
    
    
  }
  
}


classification_compare_accuracy <- function(y_test, linear_kernel_prediction, radial_kernel_prediction, 
                                            polynomial_kernel_prediction, sigmoid_kernel_prediction){
  # ~ 6-10 lines of code
  # Calculate the accuracy for each of the classification methods: 
    # 'svm-linear': linear kernel SVM
    # 'svm-radial': radial kernel SVM
    # 'svm-poly': polynomial kernel SVM
    # 'svm-sigmoid': sigmoid kernel SVM 
  # Return the best method and its accuracy (i.e., method with highest accuracy)
  
  # Inputs:
    # y_test: ground truth dependent variable from test data (factor)
    # linear_kernel_prediction: predictions from linear kernel SVM (factor)
    # radial_kernel_prediction: predictions from radial kernel SVM (factor)
    # polynomial_kernel_prediction: predictions from polynomial kernel SVM (factor)
    # sigmoid_kernel_prediction: predictions from sigmoid kernel SVM (factor)
    
  # Returns:
  # list of three values:
    # First value, of type string, with the name of the best method, sould be:
      # 'svm-linear' if linear_kernel_prediction is best
      # 'svm-radial' if radial_kernel_prediction is best
      # 'svm-poly' if polynomial_kernel_prediction is best
      # 'svm-sigmoid' if sigmoid_kernel_prediction is best
    # Second value, of type double, with the corresponding overall accuracy of the best method (on a scale of 100, do not round off)
    # third value, a vector with the overall accuracies of all methods in this order: c(linear-svm's accuracy, radial-svm's accuracy, poly-svm's accuracy, sigmoid-svm's accuracy)
  # Allowed packages: R-base
  # Note that I asked you to implement accuracy calculation - do not use a library for this
  
  
}

