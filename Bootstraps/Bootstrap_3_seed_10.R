### BOOTSTRAP 3 #### 
# the following seeds are used for the bootstraps: 1, 5, 10, 15, 20, 25.
# Each file is the same, except for the seed.

Extract_05 <- Extract_Model_Words(JSON)
Test_Train_05 <- Test_Train_Sample_Split(Extract_05, seed = 10)
SigMatrix_05 <- Binary_Signature_Matrix(Size_Row_Sig = 764, Test_Train_05)
Boot3_data_05 <- Dissim_Clust(SigMatrix_05, r_rows = 2)

Extract_1 <- Extract_Model_Words(JSON)
Test_Train_1 <- Test_Train_Sample_Split(Extract_1, seed = 10)
SigMatrix_1 <- Binary_Signature_Matrix(Size_Row_Sig = 202, Test_Train_1)
Boot3_data_1 <- Dissim_Clust(SigMatrix_1, r_rows = 2)

Extract_15 <- Extract_Model_Words(JSON)
Test_Train_15 <- Test_Train_Sample_Split(Extract_15, seed = 10)
SigMatrix_15 <- Binary_Signature_Matrix(Size_Row_Sig = 798, Test_Train_15)
Boot3_data_15 <- Dissim_Clust(SigMatrix_15, r_rows = 3)

Extract_2 <- Extract_Model_Words(JSON)
Test_Train_2 <- Test_Train_Sample_Split(Extract_2, seed = 10)
SigMatrix_2 <- Binary_Signature_Matrix(Size_Row_Sig = 348, Test_Train_2)
Boot3_data_2 <- Dissim_Clust(SigMatrix_2, r_rows = 3)

Extract_25 <- Extract_Model_Words(JSON)
Test_Train_25 <- Test_Train_Sample_Split(Extract_25, seed = 10)
SigMatrix_25 <- Binary_Signature_Matrix(Size_Row_Sig = 204, Test_Train_25)
Boot3_data_25 <- Dissim_Clust(SigMatrix_25, r_rows = 3)

Extract_3 <- Extract_Model_Words(JSON)
Test_Train_3 <- Test_Train_Sample_Split(Extract_3, seed = 10)
SigMatrix_3 <- Binary_Signature_Matrix(Size_Row_Sig = 444, Test_Train_3)
Boot3_data_3 <- Dissim_Clust(SigMatrix_3, r_rows = 4)

Extract_35 <- Extract_Model_Words(JSON)
Test_Train_35 <- Test_Train_Sample_Split(Extract_35, seed = 10)
SigMatrix_35 <- Binary_Signature_Matrix(Size_Row_Sig = 228, Test_Train_35)
Boot3_data_35 <- Dissim_Clust(SigMatrix_35, r_rows = 4)

Extract_4 <- Extract_Model_Words(JSON)
Test_Train_4 <- Test_Train_Sample_Split(Extract_4, seed = 10)
SigMatrix_4 <- Binary_Signature_Matrix(Size_Row_Sig = 435, Test_Train_4)
Boot3_data_4 <- Dissim_Clust(SigMatrix_4, r_rows = 5)

Extract_45 <- Extract_Model_Words(JSON)
Test_Train_45 <- Test_Train_Sample_Split(Extract_45, seed = 10)
SigMatrix_45 <- Binary_Signature_Matrix(Size_Row_Sig = 240, Test_Train_45)
Boot3_data_45 <- Dissim_Clust(SigMatrix_45, r_rows = 5)


Extract_5 <- Extract_Model_Words(JSON)
Test_Train_5 <- Test_Train_Sample_Split(Extract_5, seed = 10)
SigMatrix_5 <- Binary_Signature_Matrix(Size_Row_Sig = 336, Test_Train_5)
Boot3_data_5 <- Dissim_Clust(SigMatrix_5, r_rows = 6)

Extract_55 <- Extract_Model_Words(JSON)
Test_Train_55 <- Test_Train_Sample_Split(Extract_55, seed = 10)
SigMatrix_55 <- Binary_Signature_Matrix(Size_Row_Sig = 198, Test_Train_55)
Boot3_data_55 <- Dissim_Clust(SigMatrix_55, r_rows = 6)

Extract_6 <- Extract_Model_Words(JSON)
Test_Train_6 <- Test_Train_Sample_Split(Extract_6, seed = 10)
SigMatrix_6 <- Binary_Signature_Matrix(Size_Row_Sig = 210, Test_Train_6)
Boot3_data_6 <- Dissim_Clust(SigMatrix_6, r_rows = 7)

Extract_65 <- Extract_Model_Words(JSON)
Test_Train_65 <- Test_Train_Sample_Split(Extract_65, seed = 10)
SigMatrix_65 <- Binary_Signature_Matrix(Size_Row_Sig = 208, Test_Train_65)
Boot3_data_65 <- Dissim_Clust(SigMatrix_65, r_rows = 8)

Extract_7 <- Extract_Model_Words(JSON)
Test_Train_7 <- Test_Train_Sample_Split(Extract_7, seed = 10)
SigMatrix_7 <- Binary_Signature_Matrix(Size_Row_Sig = 207, Test_Train_7)
Boot3_data_7 <- Dissim_Clust(SigMatrix_7, r_rows = 9)

Extract_75 <- Extract_Model_Words(JSON)
Test_Train_75 <- Test_Train_Sample_Split(Extract_75, seed = 10)
SigMatrix_75 <- Binary_Signature_Matrix(Size_Row_Sig = 198, Test_Train_75)
Boot3_data_75 <- Dissim_Clust(SigMatrix_75, r_rows = 11)

Extract_8 <- Extract_Model_Words(JSON)
Test_Train_8 <- Test_Train_Sample_Split(Extract_8, seed = 10)
SigMatrix_8 <- Binary_Signature_Matrix(Size_Row_Sig = 221, Test_Train_8)
Boot3_data_8 <- Dissim_Clust(SigMatrix_8, r_rows = 13)
