#### Welcome to the implementation of LSH for duplicate detection in complete-linkage clustering ####

Hereby is a description of the functions which are found in each designated file
The functions are intended to be loaded to replicate the paper.  

I describe the files in order of which they occur when running the code.
To implement it, one needs to load the functions in the "functions" folder, and then run the bootstraps in the "Bootstraps" folder. Within the bootstraps folder is another folder in which all information is assembled. 

Next, I explain what the functions in the functions folder do:

#### File 1: Model_Words ####
The very first step is manipulating the JSON file to make it workable in R. This is done in the file "Extract_Model_Words". A fundamental part of LSH is to come up with binary vector representations. The papers we have studied make use of the title. I use the the feature list and the title to create the model words. Notably, I do not use all features that are available in the dataset. I first analyse which features occur in at least 48% of all the products. This means that I use 30 features from the dataset out of the 351 total features. After identifying the most important features, the values need to be identified. This is done in object feat_value_pair. I further clean these by removing any syntax (?\":.,!) and irrelevant words, such as, "featuresMap.", etc. After obtaining the feat_value_pair, I turn to the titles. The title are cleaned in a similar fashion. Using the features and the titles, the model words are created.

#### File 2: Train_Test_Sampler ####
In this file, the test and the train data is created. The train-test split is 63-27, respectively.

#### File 2: Binary_Signature_Matrix ####
This file contains the procedure of compressing the binary vectors to signature vectors using min-hashing. The figure below shows an example of a small binary matrix. The model words are a few examples of some of the model words used in this paper.

<img width="739" alt="Binary matrix" src="https://user-images.githubusercontent.com/81295233/151661824-12df2eb4-2bc8-42e6-a584-9de71afc18b7.png">


#### File 3: Dissim_Clust ####
In this file the following procedure is conducted. LSH is applied on the signature matrix from function "Binary_Signature_Matrix". Signature matrix is divided into bands and each band contains r rows with the following property: n = r * b  where n is the length of the signature matrix, b is the number of bands, r is the number of rows. "t", also  represents the relation between false positives and false negatives in the following fashion: t = (1/b)^(1/r)

<img width="1061" alt="Correct LSH" src="https://user-images.githubusercontent.com/81295233/151661830-029a6f3e-212a-4dda-b285-dfabe49be4c3.png">


After creating the dissimilarity matrix, it is used as iput in the agglomerative complete-linkage clustering algorithm. The LSH algorithm is meant to reduce the amount of time that the algorithm. Takes to process the input matrix. The "hclust" function is used and height at which to cut the tree is chosen to be 0.8.

#### File 6: Bootstrap #### 
The test and train dataset have been set. Due to some difficulty in the implementation of complete-linkage hierarchical clustering the bootstraps need to be set individually, which is often unwanted, however, unfortunately unavoidable. The bootstraps for all the different values of t are evaluated separately and can be found in the folder "bootstraps". 

I would like to thank Ettina Beiboer for being my programming buddy during the course.
