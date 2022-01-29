Binary_Signature_Matrix <- function(Size_Row_Sig = , Test_Train_Sample_Split_Output){
  # Extract_Model_Words has the following outputs for output ModelWords:
  feat_value_title_Train <- Test_Train_Sample_Split_Output$Train_value_title
  feat_value_title_test <- Test_Train_Sample_Split_Output$Test_value_title
  shop_Train <- Test_Train_Sample_Split_Output$Train_shop              
  shop_test <- Test_Train_Sample_Split_Output$Test_shop              
  brand_of_product_Train <- Test_Train_Sample_Split_Output$Train_brand_of_product 
  brand_of_product_test <- Test_Train_Sample_Split_Output$Test_brand_of_product 
  model_IDS_Train <- Test_Train_Sample_Split_Output$Train_Model_IDS  
  model_IDS_test <- Test_Train_Sample_Split_Output$Test_Model_IDS  
  feat_value_title_test <- Test_Train_Sample_Split_Output$Test_value_title
  Model_Words <- Test_Train_Sample_Split_Output$Model_Words   # No train/test set bc need all for binary/signature matrix    
  n_size <- Size_Row_Sig
  
  BinaryVectors_Train <- matrix(NA,length(feat_value_title_Train),length(Model_Words))
  for (i in 1:length(feat_value_title_Train)) {
    for (j in 1:length(Model_Words))
      BinaryVectors_Train[i,j] <- ifelse(sum(feat_value_title_Train[[i]]==Model_Words[j]) > 0,1,0)
  }
  BinaryVectors_test <- matrix(NA,length(feat_value_title_test),length(Model_Words))
  for (i in 1:length(feat_value_title_test)) {
    for (j in 1:length(Model_Words))
      BinaryVectors_test[i,j] <- ifelse(sum(feat_value_title_test[[i]] == Model_Words[j]) > 0,1,0)
  }
  Binary_Matrix_Train <- t(BinaryVectors_Train) # need products at the top 
  Binary_Matrix_test <- t(BinaryVectors_test)
  # Compressing the binary vectors to signature vectors using min-hashing
  # Size of signatures can change
  # Size_Row_Sig <- 300 # Row signature is related to fraction of comparisons
  
  Signature_Matrix_Train <- matrix(NA, nrow = n_size, ncol = dim(Binary_Matrix_Train)[2])
  PerRow_Train <- c() 
  for (i in 1:n_size){
    PerRow_Train <- Binary_Matrix_Train[sample(1:nrow(Binary_Matrix_Train)),]
    for (j in 1:dim(PerRow_Train)[2])
      Signature_Matrix_Train[i,j] <- match(1, 
                                           PerRow_Train[,j]) # 1 is the value for min-hashing
  }
  Signature_Matrix_test <- matrix(NA, nrow = n_size, ncol = dim(Binary_Matrix_test)[2])
  PerRow_test <- c() 
  for (i in 1:n_size){
    PerRow_test <- Binary_Matrix_test[sample(1:nrow(Binary_Matrix_test)),]
    for (j in 1:dim(PerRow_test)[2])
      Signature_Matrix_test[i,j] <- match(1, 
                                          PerRow_test[,j]) # 1 needs to be the value for minhashing 
  }
  
  return(list(Signature_Matrix_Train = Signature_Matrix_Train, 
              shop_Train = shop_Train, 
              brand_of_product_Train = brand_of_product_Train, 
              feat_value_title_Train = feat_value_title_Train, 
              model_IDS_Train = model_IDS_Train,
              Signature_Matrix_Test = Signature_Matrix_test, 
              shop_Test = shop_test, 
              brand_of_product_Test = brand_of_product_test, 
              feat_value_title_Test = feat_value_title_test, 
              model_IDS_Test = model_IDS_test,
              Model_Words = Model_Words))
}
