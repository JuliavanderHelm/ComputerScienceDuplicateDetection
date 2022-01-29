setwd("")
library(jsonlite)
library(dplyr)
JSON <- fromJSON("TVs-all-merged.json", flatten = TRUE)

# num_all_products <- 1624
# unique products: 1262

#### Extract_Model_Words Function ####
#### This function extracts the model words based on the titles
#### and relevant features in features list (need to occur at least 48% amongst products)
# Returns: feat_value_title, shop, Model_Words, brand_of_product, Model_IDS 
Extract_Model_Words <- function(JSON){
  # unzip JSON
  apart <- c()
  # sizeJSON <- length(JSON)
  for (i in 1:sizeJSON) {
    apart <- c(apart, JSON[i]) # turn to list 
  }
  
  # Need to return Model_IDs for later methods 
  title <- c()
  shop  <- c()
  Model_IDS <- c()
  for(i in 1:length(JSON)){
    if (dim(apart[[i]])[1] == 1) {
      title <- c(title, 
                 apart[[i]]$title[1])
      shop <- c(shop, 
                apart[[i]]$shop[1])
      Model_IDS <- c(Model_IDS, 
                     apart[[i]]$modelID[1])
    }
    else if (dim(apart[[i]])[1] == 2) {
      for (j in 1:2){
        title <- c(title, 
                   apart[[i]]$title[j])
        shop <- c(shop, 
                  apart[[i]]$shop[j])
        Model_IDS <- c(Model_IDS, 
                       apart[[i]]$modelID[j])
      }
    }
    else if (dim(apart[[i]])[1] == 3) {
      for (j in 1:3){
        title <- c(title, 
                   apart[[i]]$title[j])
        shop <- c(shop, 
                  apart[[i]]$shop[j])
        Model_IDS <- c(Model_IDS, 
                       apart[[i]]$modelID[j])
      }
    }
    else if (dim(apart[[i]])[1] == 4) {
      for (j in 1:4){
        title <- c(title, 
                   apart[[i]]$title[j])
        shop <- c(shop, 
                  apart[[i]]$shop[j])
        Model_IDS <- c(Model_IDS, 
                       apart[[i]]$modelID[j])
      }
    }
  }
  
  FeaturesMapLocation <- 5
  #### Extracting all possible features present in the JSON file ####
  # They run from 1 to 4 because the maximum number of duplicates we have is 4
   All_Feat <- c()
  for (i in 1:length(JSON)) {
    if (dim(apart[[i]])[1] == 1) { # products with no duplicates
      All_Feat <- c(All_Feat,
                    names(apart[[i]][FeaturesMapLocation:length(apart[[i]])]))  #feature map starts at 5 until length of apart
    } else if (dim(apart[[i]])[1] == 2) {
      for (a in 1:2){ # 2 duplicates
        All_Feat <- c(All_Feat,
                      names(apart[[i]][FeaturesMapLocation:length(apart[[i]])][a,]))
      }
    } else if (dim(apart[[i]])[1] == 3) {
      for (a in 1:3){ # 3 duplicates
        All_Feat <- c(All_Feat, 
                      names(apart[[i]][FeaturesMapLocation:length(apart[[i]])][a,]))
      }
    }else if (dim(apart[[i]])[1] == 4) {
      for (a in 1:4){ # 4 duplicates
        All_Feat <- c(All_Feat, 
                      names(apart[[i]][FeaturesMapLocation:length(apart[[i]])][a,]))
      }
    }
  }
  ##### Discovering most frequently used features to decide what feat to include
  
  Ordered <- table(All_Feat) %>%  as.data.frame() %>% arrange(desc(Freq)) # 1624 is total number of products
  Ordered$All_Feat[1:30] 
  # feature 30, DVI inputs is shared among 795 product descriptions
  # 795/1624 is thereby shared between 48.9% of the the products
  # summary of what is contained in Ordered--> featuresMap...
  # Maximum Resolution, Brand, UPC, Aspect Ratio, Screen Size (Measured Diagonally),
  # USB Port, TV Type, Vertical Resolution, Screen Size Class, Component Video Inputs,
  # V-Chip, HDMI Inputs, Warranty Terms - Parts, Product Depth (without stand), Screen Size,
  # Product Depth (with stand), Product Height (without stand), USB Input, Composite Inputs,
  # Product Height (with stand), PC Inputs, Screen Refresh Rate, Ethernet Port (919).... 
  # only want features that at least 900 products have in common
  MostCommonFeatures <- as.matrix(Ordered[1:30,1], 
                                  nrow = 30, 
                                  ncol = 1) 
  FeatPerProduct      <- c() # splitting apart features-values
  values_of_products  <- c()
  for(i in 1:length(JSON)){
    if (dim(apart[[i]])[1]==1) {
      FeatPerProduct <- c(FeatPerProduct, 
                          paste(names(apart[[i]][FeaturesMapLocation:length(apart[[i]])]), 
                                sep="", 
                                collapse = "~"))
      values_of_products <- c(values_of_products, 
                              paste(apart[[i]][FeaturesMapLocation:length(apart[[i]])], 
                                    sep="", 
                                    collapse = "~"))
    } else if (dim(apart[[i]])[1]==2) {
      for (j in 1:2){
        FeatPerProduct <- c(FeatPerProduct, 
                            paste(names(apart[[i]][FeaturesMapLocation:length(apart[[i]])][j]), 
                                  sep="", 
                                  collapse = "~"))
        values_of_products <- c(values_of_products, 
                                paste(apart[[i]][FeaturesMapLocation:length(apart[[i]])][j],
                                      sep="", 
                                      collapse = "~"))
      }
    } else if (dim(apart[[i]])[1]==3) {
      for (j in 1:3){
        FeatPerProduct <- c(FeatPerProduct, 
                            paste(names(apart[[i]][FeaturesMapLocation:length(apart[[i]])][j]), 
                                  sep="", 
                                  collapse = "~"))
        values_of_products <- c(values_of_products, 
                                paste(apart[[i]][FeaturesMapLocation:length(apart[[i]])][j], 
                                      sep="", 
                                      collapse = "~"))
      }
    } else if (dim(apart[[i]])[1]==4) {
      for (j in 1:4){
        FeatPerProduct <- c(FeatPerProduct, 
                            paste(names(apart[[i]][FeaturesMapLocation:length(apart[[i]])][j]), 
                                  sep="", 
                                  collapse = "~"))
        values_of_products <- c(values_of_products, 
                                paste(apart[[i]][FeaturesMapLocation:length(apart[[i]])][j], 
                                      sep="", 
                                      collapse = "~"))
      }
      values_of_products <- c(values_of_products,
                              paste(apart[[i]][FeaturesMapLocation:length(apart[[i]])][j],
                                    sep="",
                                    collapse = "~"))
    }
  }
  
  # How to extract only the most common features for each product:
  # # PerProduct to be the feature list per product represented as a list
  # # PerProdCommonFeatNA is the list with common features and NA's
  # # PerProdCommonFeat is the list with only common feautres.
  PerProductFeat  <- c()
  PerProductValue <-  c()
  for (i in 1:length(FeatPerProduct)){
    PerProductFeat <- c(PerProductFeat,(strsplit(FeatPerProduct[i], "~")))
    PerProductValue <- c(PerProductValue,(strsplit(values_of_products[i], "~")))
  }
  # for PerProductFeat[[1]] has length 39 -->  contains the feature
  # for PerProductValue[[1]] has length 39 --> contains the value
  
  PerProdCommonFeatNA <- vector(mode="list", length=length(PerProductFeat))
  length_products <- length(PerProductFeat)
  length_features <- length(MostCommonFeatures)
  for (a in 1:length_products){
  for (j in 1:length_features){
  for (k in 1:length(PerProductFeat[[i]])){
    if (PerProductFeat[[i]][k] == MostCommonFeatures[j]){ #this indicates if the most common feature appears in the list of features of a product
    PerProdCommonFeatNA[[a]][k] <- PerProductFeat[[a]][k]
      }
    }
    }
  }
  
  PerProdCommonFeat <- vector(mode="list", length = length(PerProdCommonFeatNA))
  for (i in 1:length(PerProdCommonFeatNA)){
    PerProdCommonFeat[[i]] <- PerProdCommonFeatNA[[i]][-which(is.na(PerProdCommonFeatNA[[i]]))] # This removes all remaining NAs
  }
  
  values <- c()
  splitter <- strsplit(values_of_products[i], "~")
  for (i in 1:length(values_of_products)){
    values <- c(values,splitter)
  }
  
  valuesNA <- c()
  for (i in 1:length(PerProdCommonFeatNA)){
    valuesNA[[i]] <- values[[i]][which(!is.na(PerProdCommonFeatNA[[i]]))]
  }
  
  feat_value_pair <- vector(mode = "list", length = 1624)
  for (i in 1:length(valuesNA)){
    for (k in 1:length(valuesNA[[i]])){
      feat_value_pair[[i]][k] <- paste(PerProdCommonFeat[[i]][k],
                                       valuesNA[[i]][k], 
                                       sep=" ")
    }
  }
  # I now have a featuresMap. Key (in this case feature)  + Value
  
  #### Cleaning the feat_value_pair object
  clean1      <- c()
  clean2      <- c()
  clean3      <- c()
  clean4      <- c()
  clean5      <- c()
  clean6      <- c()
  feat_value  <- c()
  length_feat_value_pair <- length(feat_value_pair)
  for (i in 1:length_feat_value_pair){
    clean1[[i]] <- gsub("featuresMap.", "", as.character(feat_value_pair[[i]]))
    clean2[[i]] <- unique(tolower(clean1[[i]]))
    clean3[[i]] <- gsub("[()]", "", as.character(clean2[[i]]))
    clean4[[i]] <- clean3[[i]][!clean3[[i]] %in% c('\" ', ":","–", "\\\\", '-', '/', 'x', "na")]
    clean5[[i]] <- gsub(pattern = ('\"'), replacement="", x = as.character(clean4[[i]]))
    clean6[[i]] <- gsub(" na", "", as.character(clean5[[i]]))
    feat_value[[i]] <- gsub(pattern = ('/"'), replacement="", x = as.character(clean5[[i]]))
  }
  
  # The brands in the datafile; after inspection 
  ALLbrands <- unique(c("Vizio", "Sharp", "Sony", "Philips", "Samsung", "LG", "Toshiba", "Panasonic"))
  ALLbrands <- tolower(ALLbrands) # so that they can be indicted
  
  shop <- tolower(shop)
  title <- tolower(title)
  title_split <- c()
  for (i in 1:length(title)){
    title_split <- c(title_split, unlist(strsplit(title[i], " +")))
  }
  
  #### cleaning the titles  ####
  cleanp1             <- c()
  cleanp2             <- c()
  cleanp3             <- c()
  cleanp4             <- c()
  cleanp5             <- c()
  cleanp6             <- c()
  unique_model_words  <- c()
  cleanp1 <- title_split[!title_split %in% c('\" ', ":","–", "\\\\", '-', '"/', ".", "+")]
  cleanp2 <- gsub(pattern = ('\"'), replacement="", x= as.character(cleanp1))
  cleanp3 <- gsub("[()]", "", as.character(cleanp2))
  cleanp4 <- tolower(cleanp3)
  cleanp5 <- gsub(pattern = ('inch'), replacement="inches", x= as.character(cleanp4))
  cleanp6 <- gsub(pattern = ( " '"), replacement="inches", x= as.character(cleanp4))
  cleanp7 <- gsub(pattern = ('diagonal'), replacement="diag.", x= as.character(cleanp4))
  cleanp8 <- cleanp7[!cleanp7 %in% c("|", "b")]
  unique_model_words <- unique(tolower(cleanp6))
  
  # Goal: be able to look through the title to find the brand
  #splittingtitles -> find_title
  find_title  <- c()
  title1      <- c()
  lower_title <- c()
  title_length <- length(title)
  for (i in 1:title_length){
    title1 <- c(title1, (strsplit(title[i], " +"))) 
  }
  for (i in 1:title_length){
   lower_title <-  tolower(title1[[i]])
    find_title[[i]] <- unique(lower_title[[i]])
  }
  
  # RETURN: brand_of_product
  brand_of_product <- c()
  title_length <- length(find_title)
  length_brands <- length(ALLbrands)
  for (i in 1:title_length){
  for (j in 1:length_brands){
  for (a in 1:length(find_title[[i]])) { # the brand is in the title
    if (find_title[[i]][a]==ALLbrands[j]){
         brand_of_product[i] <- ALLbrands[j]
      }
    }
    }
  }
  # 1624 brands; the brand is in every title
  
  # RETURN this value: feat_value_title
  # feat_value is the feature and value from the feature list
  # unique_model_words is extracted from the titles
  feat_value_title <- vector(mode="list", 
                             length = length(unique_model_words))
  for (i in 1:length(find_title)){
    feat_value_title[[i]] <- c(paste(c(feat_value[[i]], find_title[[i]]), sep=""))
  }
  length(feat_value_title)

  onlyFeatures <- c()
  for (i in 1:length(find_title)){
    onlyFeatures <- c(onlyFeatures, feat_value[[i]])
  }
  
  # RETURN: Model_Words ---
  temp_model_words <- c(onlyFeatures, unique_model_words)
  # Do not repeated model words, therefore only use unique
  Model_Words <- unique(temp_model_words)
  # length(Model_Words) # length: 3983
  
  return(list(feat_value_title = feat_value_title, 
              shop = shop, 
              Model_Words = Model_Words, 
              brand_of_product = brand_of_product, 
              Model_IDS = Model_IDS))
}











