Test_Train_Sampler <- function(Model_Words_output, seed = seed){
  set.seed(seed) # for replicable results
  # Extracting output from Model_Words
  feat_value_title <- Extract_Model_Words_output$feat_value_title
  shop <- Extract_Model_Words_output$shop
  Model_Words <- Extract_Model_Words_output$Model_Words
  Model_IDS <- Extract_Model_Words_output$Model_IDS
  brand_of_product <- Extract_Model_Words_output$brand_of_product
  
  # Identifying which products are in train/test
  train_sample        <- 0.63
  Products            <- 1:length(feat_value_title)
  Products_for_train  <- sample(length(feat_value_title)*train_sample)
  Products_for_test   <- Products[-Products_for_train]
  
  #Training data
  Train_value_title       <- feat_value_title[Products_for_train]
  Train_shop              <- shop[Products_for_train] #shop that is in Products_for_train
  Train_brand_of_product  <- brand_of_product[Products_for_train]
  Train_Model_IDS         <- Model_IDS[Products_for_train]
  
  # Test data
  Test_value_title      <- feat_value_title[Products_for_test]
  Test_shop             <- shop[Products_for_test]
  Test_brand_of_product <- brand_of_product[Products_for_test]
  Test_Model_IDS        <- Model_IDS[Products_for_test]
  
  # Model words does not need to be trained/tested split
  
  return(list(Train_value_title = Train_value_title, 
              Train_brand_of_product = Train_brand_of_product, 
              Train_shop = Train_shop, 
              Train_Model_IDS = Train_Model_IDS, 
              Test_value_title = Test_value_title, 
              Test_brand_of_product = Test_brand_of_product,
              Test_shop = Test_shop, 
              Test_Model_IDS = Test_Model_IDS,
              Model_Words = Model_Words))
}
