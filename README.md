# class_workflow_libs

This repository contains the code associated with the classification workflow described in 'Müller and Meima 2022, Mineral classification of lithium-bearing pegmatites based on laser-induced breakdown spectroscopy: Application of semi-supervised learning to detect known minerals and unknown material, Spectrochim. Acta B At. Spectrosc., 189, Article 106370' (https://doi.org/10.1016/j.sab.2022.106370).

<br />

[<p align="center"><img width="644" alt="Screenshot 2023-06-13 160742" src="https://github.com/dudelguy/class_workflow_libs/assets/130980491/ee8b2ca3-b660-41c2-88e9-ad7c33352b8b"></p>](https://doi.org/10.1016/j.sab.2022.106370)

<br />

The paper describes a classification workflow for mineral classification in LIBS measurements of geological samples. In addition to mineral classification, the workflow allows the detection of measurement points different from the ones used for training, often associated to new minerals, macro-porosities or mineral borders. Detailed information can be found in the paper. The code was developed at the Federal Institute of Geosciences and Natural Resources of Germany (BGR) as part of the T-REX project funded by EIT RawMaterials (T-REX, Nr. 19122). Additional funding was provided by the German Federal Ministry for Economic Affairs and Energy (Grant Nr. ZF4441001SA7).
After small adaptations, the provided code should be applicable for similar classification problems of other areas in the geoscientific field as well.

The code for classification is provided in three separate functions. The first and second functions are automatically executed during the third function and therefore only need to be loaded. Their input variables are dependent on the input variables of the third function and set automatically. The investigated LIBS spectra (i.e. the input for the functions) should be processed as described in the paper. Thereto, element specific emission lines need to be extracted with peak integration, normalized and stored in a data frame, in which every row contains the information of one measurement point and every column contains the normalized emission intensity of the specific element line. For the train set, mineral labels need to be assigned to every measurement point by hand.
A more detailed information is given in the paper and in the documentation of the individual functions.

### 1. Function: OC-SVM - tuning
This function contains the One-Class Support Vector Machines classification itself. The algorithm used is included in package 'e1071' and automatically tuned to fit the data. 
Afterwards, the classification results of the investigated mineral class are returned.

### 2. Function: OC-SVM - multiple minerals
In this function, OC-SVM - tuning is applied to each investigated mineral class individually. The results are combined and measurement points that are assigned to multiple classes are labelled according to the smallest euclidean distance of the related class centers. 
The function returns a data frame that includes the class labels of every investigated measurement point. Unknown data points are labelled accordingly.

### 3. Function: Classification workflow
This function includes the final classification workflow as described in the paper. First, the training and unknown data are tranformed into the LDA-space using the LDA algorithm implemented in the 'MASS' package, afterwards, the two other functions are called. If necessary, self-learning can be used to iteratively increase the initial train set with the newly labelled data. The euclidean distance of newly labelled point to its associated class center can be used to adapt the number of newly labelled data points included in each self-learning iteration. 
The function returns the final classification result, as well as the result after every iteration of self-learning. This can be used to evaluate the self-learning process and find the optimal number of iterations. 

<br />

If questions remain, feel free to get in touch anytime!
