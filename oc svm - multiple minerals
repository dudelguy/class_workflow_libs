# function to automatically apply the tuned oc-svm function to every mineral class individually and remove measurement points that are classified multiple times

### Input parameter - explanation ###

# "refSamples_LDAspace" contains a data frame or matrix with element intensity values of the training data, 
# transformed into the LDA-space. Each row of the data frame contains a measurement point, each column the normalized intensity.

# "unknownSamples_LDAspace" contains the intensity values of the data to be classified, transformed into the LDA-space.
# Each row of the data frame contains a measurement point, each column the normalized intensity.

# "label" contains an array with the labels of the training data. 
# Each mineral should have a different numeric value, e.g. mineral 1 = 1, mineral 2 = 2 ...

#####################################
#####################################

ocSVM_multipleMinerals <- function(refSamples_LDAspace, unknownSamples_LDAspace, label){
  
  # loop used to automatically apply the function "ocSVM" for every mineral individually. 
  # Thereby, the labels are used to extract all measurement points correlated to a specific mineral from the train set.
  # The unknownSamples_LDAspace are classified for every mineral
  for(numberMinerals in 1:max(label)){
    
    ocSVM_lda <- ocSVM(refSamples_LDAspace[which(label==numberMinerals),],as.data.frame(unknownSamples_LDAspace))
    
    # create/expand list to save the classification results for every mineral. 
    # Each entry of the list contains the indices of the unknownSamples_LDAspace that were identified as a sepcific mineral of the train set 
    if(numberMinerals==1){
      classesForEveryPoint <- list(which(ocSVM_lda[[1]]==2))
    }else{
      classesForEveryPoint <- c(classesForEveryPoint,list(which(ocSVM_lda[[1]]==2)))
    }
  }
  
  # some measurement points of the unknownSamples_LDAspace might be assigned to multiple minerals
  # remove these duplicates using the smallest euclidean distance to the affiliated class centers
  
  # loop used to calculate the distance of every measurement point of the "unknownSamples_LDAspace" to each class center
  # results are saved as a data frame
  for(i in 1:max(label)){
    # calculate class center for each mineral class of the train set
    class_ldaSpace <- refSamples_LDAspace[which(label==i),]
    class_center_ldaSpace <- apply(class_ldaSpace,2,mean)
    # calculate euclidean distance of each measurement point of the "unknownSamples_LDAspace" to every class center
      for(z in 1:nrow(unknownSamples_LDAspace)){
      # save the distance values in an array  
      if(z == 1){
        distClassifiedPoints <- dist(rbind(unknownSamples_LDAspace[z,],class_center_ldaSpace))
      }else{
        distClassifiedPoints <- c(distClassifiedPoints,dist(rbind(unknownSamples_LDAspace[z,],class_center_ldaSpace)))
      }
    }
    # save results in a data frame, in which every column contains the distance values of each measurement point of 
    # the unknownSamples_LDAspace (row) to the center of a distinct mineral class (column)
    # the data frame contains all classified points of the unknownSamples_LDAspace, even the ones that were classified multiple times
    # these duplicates are removed in the next step.
    if(i==1){
      dist_of_every_class <- as.data.frame(distClassifiedPoints)
    }else{
      dist_of_every_class <- cbind(dist_of_every_class,as.data.frame(distClassifiedPoints))
    }
  }
  
  # create a second data frame with identical structure as the distance data frame.
  # It contains the indices of the classified unknownSamples_LDAspace in column 1 and the affiliated label (i.e. the mineral class) in column 2. 
  # In this data frame, duplicates are possible, since the unknownSamples_LDAspace is classified for every mineral individually.
  
  # Duplicates are removed in the next step by using the distance data frame calculated one step earlier
  # Therefore, the same structure of both data frames is crucial
  for(i in 1:length(classesForEveryPoint)){
    if(length(classesForEveryPoint[[i]])>0){
      if(!exists("classesForEveryPoint_dataframe")){
        classesForEveryPoint_dataframe <- as.data.frame(classesForEveryPoint[[i]])
        classesForEveryPoint_dataframe <- cbind(classesForEveryPoint_dataframe,i)
      }else{
        classToAdd <- as.data.frame(classesForEveryPoint[[i]])
        classToAdd <- cbind(classToAdd,i)
        
        classesForEveryPoint_dataframe <- rbind(classesForEveryPoint_dataframe,classToAdd)
      }  
    }
  }
  
  # compare both data frames and remove duplicated classification results by using the smallest euclidean distance
  if(exists("classesForEveryPoint_dataframe")){
    colnames(classesForEveryPoint_dataframe) <- c("classifiedRows","class")
    
    # identify the measurement points that were classified as multiple minerals (i.e. the duplicates in the data frame)
    # "duplicatedRows" includes all indices that were classified with more than one oc-svm function 
    duplicatedRows <- which(duplicated(classesForEveryPoint_dataframe[,1])==T)
    
    # if there are measurement points that were classified multiple times, their indices are saved
    if(length(duplicatedRows)>0){
      # create a list of lists, in which every entry contains exactly those indices of the large data frame, that are duplicates of the same measurement point
      for(i in 1:length(duplicatedRows)){
        if(i == 1){
          list_with_duplicated_rows <- list(which(classesForEveryPoint_dataframe[,1] == classesForEveryPoint_dataframe[,1][duplicatedRows[i]]))
        }else{
          list_with_duplicated_rows <- c(list_with_duplicated_rows,list(which(classesForEveryPoint_dataframe[,1] == classesForEveryPoint_dataframe[,1][duplicatedRows[i]])))
        }
      }
      
      # create new data frame, from which the duplicates are to be removed
      classificationResults_dataframe  <- classesForEveryPoint_dataframe
      
      # iterate through the list that contains the lists with rows of the duplicated measurement points
      for(i in 1:length(list_with_duplicated_rows)){
        # for every entry, extract the distances of the mineral classes as which the duplicated points were classified as
        distanceForPoint <- dist_of_every_class[classesForEveryPoint_dataframe[,1][list_with_duplicated_rows[[i]][1]], classesForEveryPoint_dataframe[,2][list_with_duplicated_rows[[i]]]]
        # save the rows to be removed from the large data frame (rows with greater euclidean distance) 
        if(i == 1){
          toRemove <- list_with_duplicated_rows[[i]][-which.min(distanceForPoint)]
        }else{
          toRemove <- c(toRemove,list_with_duplicated_rows[[i]][-which.min(distanceForPoint)])
        }
      }
      # remove the identified rows from the large data frame 
      classificationResults_dataframe <- classificationResults_dataframe[-toRemove,]
    }
    # if not a single measurement point was classified multiple times, simply use the initial classification results
    else{
      classificationResults_dataframe  <- classesForEveryPoint_dataframe
    }
    
    # prepare a data frame to be returned by the function
    # create a new data frame with two columns and an identical number of rows as the classified sample 
    classifiedSample_final <- as.data.frame(matrix(NA,nrow(unknownSamples_LDAspace),2))
    # insert the labels of the classified measurement points at their specific index
    classifiedSample_final[classificationResults_dataframe[,1],] <- classificationResults_dataframe
    # Create consecutive index in the first column
    classifiedSample_final[,1] <- c(1:nrow(unknownSamples_LDAspace))
    # create new label for all measurement points that were not classified with any oc-svm function and therefore remain unknown.
    # the label for these points = 1 + the number of minerals to classify
    classifiedSample_final[which(is.na(classifiedSample_final[,2])==T),] <- max(label)+1
    # adapt the names of the column
    colnames(classifiedSample_final) <- colnames(classesForEveryPoint_dataframe)
    
    # return the final data frame, as well as the data frame that includes the distance (can be used to define a threshold for self-learning)
    return(list(classifiedSample_final, dist_of_every_class))
  }else{
    # if not a single measurement point was classified, return a data frame in which all entries are labelled as unknown
    return(list(as.data.frame(matrix(max(label)+1,nrow(unknownSamples_LDAspace),1))))
    # print a statement for clarification
    print("no new classification")
  }
}
