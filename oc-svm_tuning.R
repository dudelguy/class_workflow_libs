# this function is used to tune nu and return the final classification results of the best oc-svm model

### Input parameter - explanation ###

# refSamples: training data used to build the oc-svm function. This includes the intensity values of measurement points of a single mineral class
# unknown_data: unknown data that is supposed to be classified

#####################################
#####################################

# no labels are necessary, since the training data only includes a single class

# load package e1071 which includes the One-Class SVM algorithm
library(e1071)

ocSVM <- function(refSamples, unknown_data){
  
  # Search for the best nu-value using a loop. 
  # The number of iterations can be changed.
  # Nu can be between 0 and 1, the multiplication value 0.01 might need adjustment according to the value range covered by the data
  
  # Since the OC-SVM function is very conservative while the LIBS intensities show large variations, we aim for
  # the nu-value with a maximum of newly labeled data. 
  # "savedMaxValue" is used to monitor and update the number of newly labelled data and is initially set to 0
  savedMaxValue <- 0
  
  for(j in 1:20){
    
    # create the classification model with the train set
    svm.forBestNu <- svm(as.matrix(refSamples),
                     y=NULL,
                     type='one-classification',
                     nu=0.01*j,
                     kernel="radial", scale=T)
    
    # predict the unknown data
    svm.predict_forBestNu <- predict(svm.forBestNu, unknown_data)
    
    # save the nu value, if the corresponding model classified more data of the test set than every model before
    if(length(which(svm.predict_forBestNu==T)) > savedMaxValue){
      savedMaxValue <- length(which(svm.predict_forBestNu==T))
      nuToUse <- j*0.01
    }
  }
  
  
  # after the loop, the optimal nu-value is used to update the class labels
  if(savedMaxValue == 0){
    # if not a single measurement point was classified, all measurement point of the test set are unknown
    return(print("only unkowns"))
  }else if(savedMaxValue>0){
    # if measurement points are classified, use the optimal value for nu to classify the test data and update the labels
    svm.update <- svm(as.matrix(refSamples),
                  y=NULL,
                  type='one-classification',
                  nu=nuToUse,
                  kernel="radial", scale=T)
    
    svm.predict_update <- predict(svm.update, unknown_data)
    
    # create array with the length of the test data and label all points as '1'
    classResults_update <- rep(1,nrow(unknown_data))
    # change label of all measurement points that were classified to '2'
    classResults_update[which(as.integer(rownames(unknown_data)) %in% as.integer(names(which(svm.predict_update==T))) == T) ] <- 2
    
  }
  # return list that contains the labels after classification
  # 1 = oc-svm function is false -> measurement point remains unknown
  # 2 = oc-svm function is true -> measurement point belongs to class
  return(list(classResults_update))
}
