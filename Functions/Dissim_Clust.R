### LSH - finding dissimilarity matrix

# From paper: The signature vectors are the input vectors for LSH algorithm.
# Property: n = b * r
# Products i and j classified as neighbors by LSH if the signature vectors are equal
# in at least 1 band. If neighbors = 1.
# Products not neighbors with themselves

# Procedure:
# LSH is applied on the signature matrix from function "Binary_Signature_Matrix". 
# Signature matrix is divided into bands
# Each band contains r rows
# n = r * b 
#     n is hte length of the signature matrix
#     b is the number of bands
#     r is the number of rows
# The only parameters determined by LSH are n and t
# "t", which can represent the relation between false positives and false negatives
# t = (1/b)^(1/r)

# Use a hash function to hash the vector a certain bucket
# A bucket defined by:
#   Sequence (string) of the elements (numbers) 
#   in the band
# When two products hash to the same bucket for at least one band = candidate pair.

# Needs to be loaded as well as it is in the Dissimilarity_matrix function
# https://www.r-bloggers.com/2021/11/how-to-calculate-jaccard-similarity-in-r/
jaccard_distance <- function(a, b) { 
  intersection <- length(intersect(a, b))
  union <- length(a) + length(b) - intersection
  return (1 - intersection/union) # 1- for jaccard dissimiarilty 
}

# After obtainin the dissimilarity matrix, clustering is applied 

Dissim_Matrix_Cluster <- function(SigMatrix_Train_Test, r_rows){
  # all required attributes from the signature matrix
  SignatureMatrix_train <- SigMatrix_Train_Test$Signature_Matrix_Train
  SignatureMatrix_test <- SigMatrix_Train_Test$Signature_Matrix_Test
  feat_value_title_train <- SigMatrix_Train_Test$feat_value_title_Train
  feat_value_title_test <- SigMatrix_Train_Test$feat_value_title_Test
  shop_train <- SigMatrix_Train_Test$shop_Train 
  shop_test <- SigMatrix_Train_Test$shop_Test
  Model_Words <- SigMatrix_Train_Test$Model_Words
  brand_of_product_train <- SigMatrix_Train_Test$brand_of_product_Train
  brand_of_product_test <- SigMatrix_Train_Test$brand_of_product_Test
  model_IDS_train <- SigMatrix_Train_Test$model_IDS_Train 
  model_IDS_test <- SigMatrix_Train_Test$model_IDS_Test
  #defining the variables, n, r, b
  r_RowsInBand  <- r_rows # Set by researcher when running LSH
  b_numBands <-  dim(SignatureMatrix_train)[1]/r_RowsInBand
  candidate_pair <- 1
  # print(b_numBands)
  # possibilities:
  # example of 300 size sig. matrix
  # b_numbands = 20, r_RowsInBand = 15
  # b_numbands = 15, r_RowsInBand = 20
  # b_numbands = 12, r_RowsInBand = 25
  # b_numbands = 10, r_RowsInBand = 30
  # Total number of products 
  num_products_train <- dim(SignatureMatrix_train)[2]
  num_products_test <- dim(SignatureMatrix_test)[2]
  # Contains all the bands (number of rows in a band)
  Bands_train <- split(SignatureMatrix_train, 
                       rep(1:b_numBands,
                           each = r_RowsInBand))
  Bands_test <- split(SignatureMatrix_test, 
                      rep(1:b_numBands, 
                          each = r_RowsInBand))
  # The total number of buckets in which we can our products
  EmptyBands_train <- vector(mode="list", 
                             length = b_numBands)  # This stores all the bands 
  EmptyBands_test <- vector(mode="list", 
                            length = b_numBands) 
  # Hashing products to buckets. Need to fix R for LSH.
  for (b in 1:b_numBands){
    # Here, we use a hash function to hash a vector to a certain bucket
    # The band (length predetermined; might change later)
    PerBand_train <- matrix(Bands_train[[b]], nrow = r_RowsInBand)
    # matrix that contains the hash to the corresponding product to a bucket
    SplitSigMatrix_train <- matrix(NA, num_products_train, 1) 
    for (j in 1:num_products_train){
      for (i in 1:r_RowsInBand){
        if (i == 1){
          SplitSigMatrix_train[j,1] <- paste(PerBand_train[i,j]) #splitting signature matrix  
        } 
        else {SplitSigMatrix_train[j,1] <-paste(SplitSigMatrix_train[j,1], PerBand_train[i,j])}
      }
    } 
    unique_dup_buckets_train <- unique(SplitSigMatrix_train) # 
    #need length of unique products
    length_train <- length(unique_dup_buckets_train)
    # total products: 1624
    # with 8 rows: 1325 and sig size = 300
    # with 10 rows: 1483 (finds 141 duplicates; missing 216 duplicates)
    
    # training
    Filling_Buckets_train <- matrix(NA, length_train, num_products_train) 
    In_same_bucket_train <- matrix(NA, length_train, num_products_train)
    for (i in 1:length_train){
      for (j in 1:num_products_train){
        Filling_Buckets_train[i,j] <- ifelse(SplitSigMatrix_train[j,1] == unique_dup_buckets_train[i], 
                                             candidate_pair, # fill 1 if equal
                                             0) # fill 0, else
      }
    }
    #sizefillbuckets_train <- length(which(!Filling_Buckets_train[i,]==1))
    for (i in 1:length_train){
      In_same_bucket_train[i,] <- c(which(Filling_Buckets_train[i,] == candidate_pair), 
                                    rep(NA, length(which(!Filling_Buckets_train[i,] == candidate_pair))))
    }
    # Cleaning the buckets such that only the duplicates remain:
    # Aka remove the products from the buckets that do not have duplicates
    removing_zeroes_train <- colSums(In_same_bucket_train, na.rm=TRUE) # reduces time such that the length of the matrix = length of duplicates
    no_zeroes <- which(removing_zeroes_train != 0)
    Only_Duplicates_train <- length(no_zeroes)
    Candidate_pairs_train <- which(!is.na(In_same_bucket_train[, 2]) )  # the candidate pairs
    
    # only retrieving the possible candidate pairs 
    In_same_bucket_train_clean <- In_same_bucket_train[Candidate_pairs_train, ]
    EmptyBands_train[[b]] <- In_same_bucket_train_clean[, (1:Only_Duplicates_train)]
  }
  # do the same thing for test
  for (b in 1:b_numBands){
    # Here, we use a hash function to hash a vector to a certain bucket
    # The band (length/height predetermined; might change later; do not set k: determines number of clusters)
    PerBand_test <- matrix(Bands_test[[b]], nrow = r_RowsInBand)
    # matrix that contains the hash to the corresponding product to a bucket
    SplitSigMatrix_test <- matrix(NA, num_products_test, 1) 
    for (j in 1:num_products_test){
      for (i in 1:r_RowsInBand){
        if (i == 1){SplitSigMatrix_test[j,1] <- paste(PerBand_test[i,j])
        } else {SplitSigMatrix_test[j,1] <-paste(SplitSigMatrix_test[j,1],PerBand_test[i,j])}
      }
    } # SplitSigMatrix is 1624 by 1; each bucket containing 10 values
    unique_dup_buckets_test <- unique(SplitSigMatrix_test) # 1483 unique bands
    #need length of unique products
    length_test <- length(unique_dup_buckets_test)
    Filling_Buckets_test <- matrix(NA, length_test, num_products_test) 
    In_same_bucket_test <- matrix(NA, length_test, num_products_test)
    for (i in 1:length_test){
      for (j in 1:num_products_test){
        Filling_Buckets_test[i,j] <- ifelse(SplitSigMatrix_test[j,1] == unique_dup_buckets_test[i], 1, 0) 
      }
    }
    #sizefillbuckets_test <- length(which(!Filling_Buckets_test[i,]==1))
    for (i in 1:length_test){
      In_same_bucket_test[i,] <- c(which(Filling_Buckets_test[i,] == candidate_pair), 
                                   rep(NA, length(which(!Filling_Buckets_test[i,]==1))))
    }
    # Cleaning the buckets such that only the duplicates remain:
    # Aka remove the products from the buckets that do not have duplicates
    removing_zeroes_test <- colSums(In_same_bucket_test, na.rm = TRUE)
    Only_Duplicates_test <- length(which(removing_zeroes_test != 0))
    Candidate_pairs_test <- which(!is.na(In_same_bucket_test[,2])) 
    # only retrieving the possible candidate pairs 
    In_same_bucket_test_clean <- In_same_bucket_test[Candidate_pairs_test, ]
    EmptyBands_test[[b]] <- In_same_bucket_test_clean[, (1:Only_Duplicates_test)]
  }
  
  # 15 products hashed to the same bucket 
  ## The next step is to make the dissimilarity matrix ##
  dissimilarity_matrix_train <- matrix(inf, num_products_train, num_products_train) # starting with inf dissimilarity matrix
  dissimilarity_matrix_test <- matrix(inf, num_products_test, num_products_test) # starting with inf dissimilarity matrix 
  # loop over products
  for (p in 1:num_products_train){  # p for products
    # loop over number of bands to split signature matrix
    for (b in 1:b_numBands){  # b for bands
      # loop over the final buckets that contain candidate pairs
      for (i in 1:dim(EmptyBands_train[[b]])[1]){
        # loop through the buckets that do NOT have NA;
        # example: Filled_Buckets[[1]][5,] contains [95 1064   NA   NA   NA], then returns 2
        replace_NA_with_zero_train <- ifelse(!is.na(EmptyBands_train[[b]][i,]),1,0) # counts number of duplicates if there are any
        replace_NA_with_zero_test <- ifelse(!is.na(EmptyBands_test[[b]][i,]),1,0) # counts number of duplicates if there are any
        
        # training loop that counts number duplicates
        for (j in 1:sum(replace_NA_with_zero_train)){
          # need to search for products that are in the buckets and if they 
          # match the products
          if (EmptyBands_train_train[[b]][i,j] == p){ 
            replace_NA_with_zero_train <- ifelse(!is.na(EmptyBands_train[[b]][i,]), candidate_pair, 0) # counts num duplicates
            for (k in 1:sum(replace_NA_with_zero_train)){
              dissimilarity_matrix_train[p, EmptyBands_train[[b]][i,k]] <- candidate_pair # in dissim matrix, if candidate pair found = 1 in (i,j)
            }}}
        
        # testing loop that counts number duplicates
        for (j in 1:sum(replace_NA_with_zero_test)){
          # need to search for products that are in the buckets and if they 
          # match the products
          if (EmptyBands_test[[b]][i,j] == p){ 
            replace_NA_with_zero_test <- ifelse(!is.na(EmptyBands_test[[b]][i,]), candidate_pair, 0) # counts num duplicates
            for (k in 1:sum(replace_NA_with_zero_test)){
              dissimilarity_matrix_train[p, EmptyBands_test[[b]][i,k]] <- candidate_pair # in dissim matrix, if candidate pair found = 1 in (i,j)
            }}}
      }
    }
  }  
  length_train1 <- dim(dissimilarity_matrix_train)[1] # needs to be set for train/test
  length_train2 <- dim(dissimilarity_matrix_train)[2]
  length_test1 <- dim(dissimilarity_matrix_test)[1] # 
  length_test2 <- dim(dissimilarity_matrix_test)[2]
  # to filter out possbilities, set all products with the same website/shop to infinity
  for (i in 1:length_train1){ # for the training set
    for (j in 1:length_train2){
      if (i == j){
        dissimilarity_matrix_train[i,j] <- inf
      } 
      else if (shop_train[i] == shop_train[j]){
        dissimilarity_matrix_train[i,j] <- 1
      }
    }
  }
  # to filter out possibilities, set all products with the same brand to infinity
  possBrand_train <- c(which(!is.na(brand_of_product_train)))
  for (i in possBrand_train){
    for (j in possBrand_train){
      if (brand_of_product_train[i] != brand_of_product_train[j]){
        dissimilarity_matrix_train[i,j] <- inf}}
  }
  possBrand_test <- c(which(!is.na(brand_of_product_test)))
  for (i in possBrand_test){
    for (j in possBrand_test){
      if (brand_of_product_test[i] != brand_of_product_test[j]){
        dissimilarity_matrix_test[i,j] <- inf}}
  }
  for (i in 1:length_test1){ # for the test set
    for (j in 1:length_test2){
      if (i == j){
        dissimilarity_matrix_test[i,j] <- inf
      } 
      else if (shop_test[i] == shop_test[j]){
        dissimilarity_matrix_test[i,j] <- inf
      }
    }
  }
  # rowSums(dissimilarity_matrix_train) check if working
  # colSums(dissimilarity_matrix_train)
  # rowSums(dissimilarity_matrix_test)
  # colSums(dissimilarity_matrix_test)
  # Here compute jaccard distance
  comparing_train <- c()
  comparing_test <- c()
  for (i in 1:length_train1){
    for (j in 1:length_train2){
      if (i < j){ 
        if (dissimilarity_matrix_train[i,j] == 1){ # inf breaks the comp, compare for 1
          comparing_train <- c(comparing_train, 1)
        }
      }
      # this measures how dissimilar two sets are
      else if (dissimilarity_matrix_train[i,j] == 1){
        # need the Jaccard Distance (1 - jaccard(a, b)):
        dissimilarity_matrix_train[i,j] <- jaccard_distance(feat_value_title[[i]], 
                                                            feat_value_title[[j]])
      }
    }
  }
  for (i in 1:length_test1){
    for (j in 1:length_test2){
      if (i < j){ 
        if (dissimilarity_matrix_test[i,j] == 1){ # 10 breaks the comp, compare for 1 to limit computations 
          comparing_test <- c(comparing_test, 1)
        }
      }
      # this measures how dissimilar two sets are
      else if (dissimilarity_matrix_test[i,j] == 1){
        # need the Jaccard Distance (1 - jaccard(a, b)):
        dissimilarity_matrix_test[i,j] <- jaccard_distance(feat_value_title[[i]], 
                                                           feat_value_title[[j]])
      }
    }
  }
  comparisons_made_train <- length(comparing_train)
  comparisons_made_test <- length(comparing_test)
  
  #### Beginning of clustering ####
  h1 <- 0.8
  inf <- 10
  candidate_pair <- 1 # mark as candidate pair!
  length_model_IDS_train <- length(model_IDS_train)
  length_model_IDs_test <- length(model_IDS_test)
  
  model_ID_duplicates_train <- matrix(NA, nrow = length_model_IDS_train, ncol = length_model_IDS_train)
  for (i in 1:length(model_IDS_train)){
    for (j in 1:length(model_IDS_train)){
      if (model_IDS_train[[i]] == model_IDS_train[[j]]){
        if (i<j){ model_ID_duplicates_train[i,j] <- candidate_pair}
      }
    }
  }
  model_ID_duplicates_test <- matrix(NA,nrow = length_model_IDs_test, ncol = length_model_IDS_train)
  for (i in 1:length(model_IDS_test)){
    for (j in 1:length(model_IDS_test)){
      if (model_IDS_test[[i]] == model_IDS_test[[j]]){
        if (i<j){ model_ID_duplicates_test[i,j] <- candidate_pair }
      }
    }
  }
  Num_TotalComparisons_train <- 1023*1022
  TotalComparisons_train <- (Num_TotalComparisons_train/2) # [1023*1022]/2
  
  # The next step is to apply a hierachical clustering method
  # on the dissimilarity matrix. To identify the duplicates obtained
  # through MSPM.
  # "hclust" packages requires a structure from "dist". 
  hclust_obj_train <- as.dist(dissimilarity_matrix_train)
  hclust_obj_test <- as.dist(dissimilarity_matrix_test)
  
  complete_hclust_train <- hclust(hclust_obj_train, method = "complete") # complete linkage
  complete_hclust_test <- hclust(hclust_obj_test, method = "complete")
  #plot(XX) does not work
  # Next step is to cut a tree into groups of data
  cut_complete_hclust_train <- cutree(complete_hclust_train, h = h1) # h = length, not num clusters
  cut_complete_hclust_test <- cutree(complete_hclust_test, h = h1) # h is hyper-parameter
  
  # TRUE / FALSE of duplicates:
  #identify_duplicates_train <- duplicated(cut_complete_hclust_train) 
  #identify_duplicates_test <- duplicated(cut_complete_hclust_test)
  
  #list of duplicates
  duplicates_in_dissimilarity_train <-  cut_complete_hclust_train[duplicated(cut_complete_hclust_train)]
  duplicates_in_dissimilarity_test <-  cut_complete_hclust_test[identify_duplicates_test]
  
  train_diss_length <- length(duplicates_in_dissimilarity_train)
  test_diss_length <- length(duplicates_in_dissimilarity_test)
  
  draft_clusters_train <- vector(mode = "list", length = train_diss_length)
  draft_clusters_test <- vector(mode = "list", length = test_diss_length)
  
  for (i in 1:length(draft_clusters_train)){
    draft_clusters_train[[i]] <- which(cut_complete_hclust_train %in%  duplicates_in_dissimilarity_train[i])}
  for (i in 1:length(draft_clusters_test)){
    draft_clusters_test[[i]] <- which(cut_complete_hclust_test %in%  duplicates_in_dissimilarity_test[i])}
  
  # These are the duplicates found by the clustering method for the training set
  clusters_train <- unique(draft_clusters_train)
  clusters_test <- unique(draft_clusters_test)
  
  # Next, need to evaluate the precision/recall/pair quality/etc.
  True_positives_vector_train <- c()
  True_positives_vector_test <- c()
  False_positives_vector_train <- c()
  False_positives_vector_test <- c()
  # for training 
  for (i in 1:length(clusters_train)){
    if (length(clusters_train[[i]]) == 2){ #need to be at least 2, otherwise cannot compare
      # compare 1 and 2, if equal  --> true positives
      if (model_IDS_train[clusters_train[[i]]][2] == model_IDS_train[clusters_train[[i]]][1]){True_positives_vector_train <- c(True_positives_vector_train, 1)} 
      # add to false positives if not equal
      else if (model_IDS_train[clusters_train[[i]]][1] != model_IDS_train[clusters_train[[i]]][2]){False_positives_vector_train <- c(False_positives_vector_train, 1)}}
    if (length(clusters_train[[i]]) == 3){ 
      #compare 1 and 3, if equal  --> true positives
      if (model_IDS_train[clusters_train[[i]]][1] == model_IDS_train[clusters_train[[i]]][3]){True_positives_vector_train <- c(True_positives_vector_train, 1) } 
      # add to false positives if not equal
      else if (model_IDS_train[clusters_train[[i]]][1] != model_IDS_train[clusters_train[[i]]][3]){False_positives_vector_train <- c(False_positives_vector_train, 1)}
      # compare 2 and 3, if equal  --> true positives
      if (model_IDS_train[clusters_train[[i]]][2] == model_IDS_train[clusters_train[[i]]][3]){True_positives_vector_train <- c(True_positives_vector_train, 1) } 
      # add to false positives if not equal
      else if (model_IDS_train[clusters_train[[i]]][1] != model_IDS_train[clusters_train[[i]]][2]){False_positives_vector_train <- c(False_positives_vector_train, 1)}
      # compare 1 and 2, , if equal  --> true positives
      if (model_IDS_train[clusters_train[[i]]][1] == model_IDS_train[clusters_train[[i]]][2]){True_positives_vector_train <- c(True_positives_vector_train, 1) }
      # add to false positives if not equal
      else if (model_IDS_train[clusters_train[[i]]][2] != model_IDS_train[clusters_train[[i]]][2]){False_positives_vector_train <- c(False_positives_vector_train, 1)}
    }
  }
  # for testing 
  for (i in 1:length(clusters_test)){
    if (length(clusters_test[[i]]) == 2){
      # compare 1 and 2, if equal  --> true positives
      if (model_IDS_test[clusters_test[[i]]][1] == model_IDS_test[clusters_test[[i]]][2]){True_positives_vector_test <- c(True_positives_vector_test, 1)} 
      # add to false positives if not equal
      else if (model_IDS_test[clusters_test[[i]]][1] != model_IDS_test[clusters_test[[i]]][2]){False_positives_vector_test <- c(False_positives_vector_test, 1)}}
    if (length(clusters_test[[i]]) == 3){
      # compare 1 and 3, if equal --> true positives
      if (model_IDS_test[clusters_test[[i]]][1] == model_IDS_test[clusters_test[[i]]][3]){True_positives_vector_test <- c(True_positives_vector_test, 1) } 
      # add to false positives if they are not equal
      else if (model_IDS_test[clusters_test[[i]]][1] != model_IDS_test[clusters_test[[i]]][3]){False_positives_vector_test <- c(False_positives_vector_test, 1)}
      # compare 2 and 3, if equal --> true positives
      if (model_IDS_test[clusters_test[[i]]][2] == model_IDS_test[clusters_test[[i]]][3]){True_positives_vector_test <- c(True_positives_vector_test, 1) } 
      # add to false positives if they are not equal
      else if (model_IDS_test[clusters_test[[i]]][1] != model_IDS_test[clusters_test[[i]]][2]){False_positives_vector_test <- c(False_positives_vector_test, 1)}
      # compare 1 and 3, if equal --> true positives
      if (model_IDS_test[clusters_test[[i]]][1] == model_IDS_test[clusters_test[[i]]][3]){True_positives_vector_test <- c(True_positives_vector_test, 1) }
      # add to false positives if they are not equal
      else if (model_IDS_test[clusters_test[[i]]][2] != model_IDS_test[clusters_test[[i]]][2]){False_positives_vector_test <- c(False_positives_vector_test, 1)}
    }
  }
  true_dups_train_row <- rowSums(model_ID_duplicates_train, na.rm = TRUE)
  true_dups_train <- sum(true_dups_train_row)
  true_dups_test_row <- rowSums(model_ID_duplicates_test, na.rm = TRUE)
  true_dups_test <- sum(true_dups_test_row)
  
  # Calculating the evaluation criteria  for train/test
  #   True/false Positives
  True_positives_train <- length(True_positives_vector_train)
  True_positives_test <- length(True_positives_vector_test)
  False_positives_train <- length(False_positives_vector_train)
  False_positives_test <- length(False_positives_vector_test)
  #   Negatives
  False_negatives_train <- true_dups_train - True_positives_train
  False_negatives_test <- true_dups_test - True_positives_test
  
  #### Precision 
  Precision_train <- (True_positives_train / (True_positives_train + False_positives_train))
  Precision_test <- (True_positives_test / (True_positives_test + False_positives_test))
  
  #### Recall 
  Recall_train <- (True_positives_train / (True_positives_train + False_negatives))
  Recall_test <- (True_positives_test / (True_positives_test + False_negatives))

  #### Pair completeness ####
  Pair_completeness_train <- True_positives_train/true_dups_train
  Pair_completeness_test <- True_positives_test/true_dups_test
  
  #### Pair quality ####
  Pair_quality_train <- True_positives_train/comparisons_made_train
  Pair_quality_test <- True_positives_test/comparisons_made_test
  
  #### F-star ####
  F1_train <- ((2*(Precision_train * Recall_train))/(Precision_train + Recall_train))
  F1_test <- ((2*(Precision_test * Recall_test))/(Precision_test + Recall_test))
  
  #### F1_star ####
  NumF1_star_train <- 2*(Pair_completeness_train*Pair_quality_train)
  F1_star_train <- NumF1_star_train/(Pair_completeness_train+Pair_quality_train )
  NumF1_star_test <- 2*(Pair_completeness_test*Pair_quality_test)
  F1_star_test <- NumF1_star_test/(Pair_completeness_test+Pair_quality_test )
  
  frac_comparisons_train <- comparisons_made/TotalComparisons_train
  frac_comparisons_test <- comparisons_made/TotalComparisons_test
  
  return(list(clusters_train = clusters_train,
              Precision_train = Precision_train,
              Recall_train = Recall_train,
              Pair_quality_train = Pair_quality_train,
              Pair_completeness_train= Pair_completeness_train,
              F1_train = F1_train, 
              F1_star_train = F1_star_train,
              clusters_test = clusters_test,
              Precision_test = Precision_test,
              Recall_test = Recall_test,
              Pair_quality_test = Pair_quality_test,
              Pair_completeness_test= Pair_completeness_test,
              F1_test = F1_test, 
              F1_star_test = F1_star_test))
}






