# Function that includes the classification workflow explained in Müller & Meima 2022 (https://doi.org/10.1016/j.sab.2022.106370)

# The packages MASS und e1071 need to be installed beforehand
# The two functions 'oc svm - tuning' and 'oc svm - multiple minerals' need to be loaded

#################################################################
#################################################################

### Input parameter - explanation ###

# referenceData: LIBS-based intensity lines of the training data. 
#                Every column contains the normalized emission intensity of a specific element line (for more infomration, see Müller & Meima 2022)
#                Every row contains a measurement point

# unknownData: LIBS-based intensity lines of the unknown data that is classified. 
#              Every column contains the normalized emission intensity of a specific element line (for more infomration, see Müller & Meima 2022)
#              Every row contains a measurement point
#              column names must match with the ones from "referenceData"

# label: Label of the training data. Entries must match the measurement points (i.e. rows) of "referenceData".
#        Label need to be numerical (i.e. no strings)

# maxIterations: Number of iterations performed during self-learnig. Default is 0, which is equivalent to no self-learning

# wantedPercentile: Threshold that defines, how much newly labeled points are added to the train set during self-learning. 
#                   Threshold is given in percent of nearest points to the cluster center (measured in euclidean distance).  
#                   In the paper, the default of 1 (i.e. all newly classified points) was used.

#################################################################
#################################################################

### Output parameter - explanation ###

# Output is a list that contains two lists.

# 1. list: Classification result after n iterations of self-learning

# 2. list: Another list, which contains the classification result of the initial classification model without, 
#          and the results after every iteration of self-learning


#################################################################
#################################################################

classification_workflow <- function(referenceData, unknownData, label, maxIterations = 0, wantedPercentile = 1){

  # load the library that includes the LDA algorithm
  library(MASS)
  # perform LDA on the training data (referenceData). The label need to be given as well 
  lda_sprem.model <- lda(referenceData,label)
  
  # apply the discriminant functions of the LDA to training data and the unknown data to be classified
  # the resulting variables include the data after transformation into the LDA-space
  projected_refSamples = as.matrix(referenceData) %*% lda_sprem.model$scaling
  projected_unknownSamples <- as.matrix(unknownData) %*% lda_sprem.model$scaling
  
  # apply the oc-svm classification (see its documentation for more information)
  ocSVM_result <- ocSVM_multipleMinerals(projected_refSamples,projected_unknownSamples,label)
  
  
  # this section of code is needed to initialize the self-learning process described in the paper
  
  # toUdpate is used to indicate, which points should be included in the new train set (1 = not included, 2 = included) 
  toUpdate <- rep(1,nrow(unknownData))
  
  # for self learning, not all newly labeled points need to be added training data
  # this is controlled by using percentiles of the distance to the class centers to which the points belong. 
  # The default is "wantedPercentile = 1", which included all newly labeled data points
  for(w in 1:max(label)){
    selectedLabel <- which(ocSVM_result[[1]][,2] == w)
    distanceSelectedLabel <- ocSVM_result[[2]][selectedLabel,w]
    
    wantedDistance_percent <- quantile(distanceSelectedLabel,wantedPercentile)
    idxWantedPoints <- which(distanceSelectedLabel <= wantedDistance_percent)
    
    toUpdate[selectedLabel][idxWantedPoints] <- 2
  }
  
  # extracting the indices of the unknown measurement points. These points are classified again during self-learning
  newTest <- which(ocSVM_result[[1]][,2] == max(label)+1)
  
  # to be able to reconstruct each classification result of the self-learning process separately, the result of every iteration is stored in a list
  iterationIndices <- list(ocSVM_result[[1]][,2])
  
  ######################################################################################################
  ######################################################################################################
  ######################################################################################################
  
  # if self-learning is wanted (which "maxIterations = 0", the default is no self-learning), the following lines of code are executed
  if(maxIterations > 0){
    # start the outer loop using the given number of iterations
    for(i in 1:maxIterations){
      
      # update the train set using "toUpdate" and update the remaining unknowns with "newTest"
      updatedTrain <- rbind(referenceData,unknownData[which(toUpdate==2),])
      updatedLabel <- c(label,ocSVM_result[[1]][which(toUpdate==2),2])
      updatedUnknown <- unknownData[newTest,]
      
      # apply the LDA to the updated train set
      lda_sprem.model <- lda(updatedTrain,updatedLabel)
      
      # use the new discriminant functions to transform the updated data sets into the LDA-space
      updated_refSamples = as.matrix(updatedTrain) %*% lda_sprem.model$scaling
      updated_unknownSamples <- as.matrix(updatedUnknown) %*% lda_sprem.model$scaling
      
      # calculate the new classification results by using the updated data sets as input
      ocSVM_updated <- ocSVM_multipleMinerals(updated_refSamples,updated_unknownSamples,updatedLabel)
      
      # check, if changes have been made to the remaining unknown data
      if(is.na(ocSVM_updated[[1]][1,1])){
        # if no changes were made, print a statement and indicate the number of iterations
        print("no changes to classification")
        print(paste("finished after ", i-1, " iterations"))
        # exit the loop to avoid unnecessary iterations 
        break
      }else{
        # if changes are made, update the classification results accordingly
        ocSVM_result[[1]][newTest,] <- ocSVM_updated[[1]]
        ocSVM_result[[2]][newTest,] <- ocSVM_updated[[2]]
        
        # update the variable "toUpdate" with the new results. During the next iteration, the new results will be used
        for(k in 1:max(label)){
          selectedLabel <- which(ocSVM_result[[1]][newTest,2] == k)
          if(length(selectedLabel)>0){
            distanceSelectedLabel <- ocSVM_result[[2]][selectedLabel,k]
            
            wantedDistance_percent <- quantile(distanceSelectedLabel,wantedPercentile)
            idxWantedPoints <- which(distanceSelectedLabel <= wantedDistance_percent)
            
            toUpdate[newTest][selectedLabel][idxWantedPoints] <- 2
          }
          # if no changes were made to the classification of a specific class, the loop advances to the next class
          else{
            next
          }
        }
        
        # after updating "toUdpate" by using "newTest", "newTest" is updated itself
        newTest <- which(ocSVM_result[[1]][,2] == max(label)+1)
        
        # the results of the specific iteration is saved in the list at its specific position
        iterationIndices <- c(iterationIndices, list(ocSVM_updated[[1]][,2]))
        
        i <- i+1
      }
    }
  }
  
  # return the final class labels after the selected number of iterations (default is 0 iterations of self-learning)
  # additionally, the classification results for every iteration itself are returned ("iterationIndices")
  return(list(ocSVM_result[[1]][,2],iterationIndices))
}
