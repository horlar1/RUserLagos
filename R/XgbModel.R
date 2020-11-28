#' Creative Modelling with Xgboost
#' @description Provides a high level function to train xgboost models
#'
#'
#' @param data a dataframe or matrix denotes the training data
#' @param label target variable.
#' @param parameters list containing the model paramters.
#' @param seed numeric.
#' @param type type of model.Default = regression.
#' @param folds number of folds. default is 5
#' @param newdata a dataframe or matrix denote the test data.
#' @param earlystopping numeric. default = 10
#' @param print_every_n numeric. default = 5
#' @param maximize boolean. default is F. used to maximize the metric of choice.
#'
#'
#'@importFrom xgboost xgb.train
#'@importFrom caret createFolds
#' @export
#'@seealso \code{\link{cor} \link{detectCores}}
#' @examples \dontrun{
#' # don't run this sript
#'
#' }
#'
#'
#'
XgbModel <- function(data,label,newdata,seed,folds,parameter,type ="regression",earlystopping = 10,print_every_n = 5,maximize = F){

  if(type == "regression" | type == "classfication"){
    devresult = rep(0,nrow(data))
    predte = rep(0,nrow(newdata))
    cvscore = c()

    ### iteration
    for (i in 1:length(seed)) {
      cat("model training",i,"\n")

      ### create folds
      set.seed(seed[i])
      folds = createFolds(label, k = folds)

      for (this.round in 1:length(folds)) {
        cat("model training",i," ","fold ",this.round,"\n")
        valid = c(1:length(label))[unlist(folds[this.round])]
        dev = c(1:length(label))[unlist(folds[1:length(folds)!= this.round])]

        dtrain<- xgboost::xgb.DMatrix(data= as.matrix(data[dev,]), label= label[dev])
        dvalid <- xgboost::xgb.DMatrix(data= as.matrix(data[valid,]),label=label[valid])
        #valids <- list(val = dvalid)

        ## set parameters
        parameters <- list(booster = "gbtree",
          objective = param[[1]],
          eval_metric = param[[2]],
          eta = param[[3]],
          colsample_bytree = param[[4]],
          max_depth = param[[5]],
          min_child_weight = param[[6]],
          nthread = parallel::detectCores()-1,
          subsample = param[[7]])

        ## model training
        model <- xgb.train(data = dtrain,
          params = parameters,
          nrounds = param[[8]],
          verbose = T,
          watchlist = list(val=dvalid),
          early_stopping_rounds = earlystopping,
          print_every_n = print_every_n,
          maximize = maximize)

        cvscore <- c(cvscore,min(model[[7]][[2]]))
        #### make predictions
        pred = predict(model,as.matrix(data[valid,]))
        devresult[valid] = pred
        pred_test = predict(model, as.matrix(newdata[,colnames(data)]))
        predte = predte + pred_test


      }
    }
  }
  ## output

  output = list(devresult = devresult,pred = predte,cv_score = cvscore,parameters = parameters)
  return(output)

}
