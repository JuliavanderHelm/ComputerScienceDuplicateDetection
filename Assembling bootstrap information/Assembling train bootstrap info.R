#|-----------------------Assembling the information - train ----------------------------------
# x_axis <- c(0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80)
# Pair quality 
# Pair completeness 
# F1 measure
# F1*

# Pair quality - train - replace with train to get train data. Can command-find and replace all train with "train" and run results again

Pair_quality_values_boot1 <- c(Boot1_data_05$Pair_quality_train, Boot1_data_1$Pair_quality_train, Boot1_data_15$Pair_quality_train, Boot1_data_2$Pair_quality_train, Boot1_data_25$Pair_quality_train, Boot1_data_3$Pair_quality_train, Boot1_data_35$Pair_quality_train, Boot1_data_4$Pair_quality_train, Boot1_data_45$Pair_quality_train, Boot1_data_5$Pair_quality_train, Boot1_data_55$Pair_quality_train, Boot1_data_6$Pair_quality_train, Boot1_data_65$Pair_quality_train, Boot1_data_7$Pair_quality_train, Boot1_data_75$Pair_quality_train, Boot1_data_8$Pair_quality_train)
Pair_quality_values_boot2 <- c(Boot2_data_05$Pair_quality_train, Boot2_data_1$Pair_quality_train, Boot2_data_15$Pair_quality_train, Boot2_data_2$Pair_quality_train, Boot2_data_25$Pair_quality_train, Boot2_data_3$Pair_quality_train, Boot2_data_35$Pair_quality_train, Boot2_data_4$Pair_quality_train, Boot2_data_45$Pair_quality_train, Boot2_data_5$Pair_quality_train, Boot2_data_55$Pair_quality_train, Boot2_data_6$Pair_quality_train, Boot2_data_65$Pair_quality_train, Boot2_data_7$Pair_quality_train, Boot2_data_75$Pair_quality_train, Boot2_data_8$Pair_quality_train)
Pair_quality_values_boot3 <- c(Boot3_data_05$Pair_quality_train, Boot3_data_1$Pair_quality_train, Boot3_data_15$Pair_quality_train, Boot3_data_2$Pair_quality_train, Boot3_data_25$Pair_quality_train, Boot3_data_3$Pair_quality_train, Boot3_data_35$Pair_quality_train, Boot3_data_4$Pair_quality_train, Boot3_data_45$Pair_quality_train, Boot3_data_5$Pair_quality_train, Boot3_data_55$Pair_quality_train, Boot3_data_6$Pair_quality_train, Boot3_data_65$Pair_quality_train, Boot3_data_7$Pair_quality_train, Boot3_data_75$Pair_quality_train, Boot3_data_8$Pair_quality_train) 
Pair_quality_values_boot4 <- c(Boot4_data_05$Pair_quality_train, Boot4_data_1$Pair_quality_train, Boot4_data_15$Pair_quality_train, Boot4_data_2$Pair_quality_train, Boot4_data_25$Pair_quality_train, Boot4_data_3$Pair_quality_train, Boot4_data_35$Pair_quality_train, Boot4_data_4$Pair_quality_train, Boot4_data_45$Pair_quality_train, Boot4_data_5$Pair_quality_train, Boot4_data_55$Pair_quality_train, Boot4_data_6$Pair_quality_train, Boot4_data_65$Pair_quality_train, Boot4_data_7$Pair_quality_train, Boot4_data_75$Pair_quality_train, Boot4_data_8$Pair_quality_train)
Pair_quality_values_boot5 <- c(Boot5_data_05$Pair_quality_train, Boot5_data_1$Pair_quality_train, Boot5_data_15$Pair_quality_train, Boot5_data_2$Pair_quality_train, Boot5_data_25$Pair_quality_train, Boot5_data_3$Pair_quality_train, Boot5_data_35$Pair_quality_train, Boot5_data_4$Pair_quality_train, Boot5_data_45$Pair_quality_train, Boot5_data_5$Pair_quality_train, Boot5_data_55$Pair_quality_train, Boot5_data_6$Pair_quality_train, Boot5_data_65$Pair_quality_train, Boot5_data_7$Pair_quality_train, Boot5_data_75$Pair_quality_train, Boot5_data_8$Pair_quality_train)
Pair_quality_values_boot6 <- c(Boot6_data_05$Pair_quality_train, Boot6_data_1$Pair_quality_train, Boot6_data_15$Pair_quality_train, Boot6_data_2$Pair_quality_train, Boot6_data_25$Pair_quality_train, Boot6_data_3$Pair_quality_train, Boot6_data_35$Pair_quality_train, Boot6_data_4$Pair_quality_train, Boot6_data_45$Pair_quality_train, Boot6_data_5$Pair_quality_train, Boot6_data_55$Pair_quality_train, Boot6_data_6$Pair_quality_train, Boot6_data_65$Pair_quality_train, Boot6_data_7$Pair_quality_train, Boot6_data_75$Pair_quality_train, Boot6_data_8$Pair_quality_train)

matrixPairQuality <- matrix(nrow = 6, ncol = 16)
matrixPairQuality[1,] <- Pair_quality_values_boot1
matrixPairQuality[2,] <- Pair_quality_values_boot2
matrixPairQuality[3,] <- Pair_quality_values_boot3
matrixPairQuality[4,] <- Pair_quality_values_boot4
matrixPairQuality[5,] <- Pair_quality_values_boot5
matrixPairQuality[6,] <- Pair_quality_values_boot6

plot_matrixPairQuality<- colMeans(matrixPairQuality)

plot(x_axis, 
     plot_matrixPairQuality,
     bty = "L",
     type = "l",
     xlab  = "Fraction of comparisons",
     ylab = "Pair quality")

# | --------------------------------------------------------------------------
# Pair completeness - train

Pair_completeness_values_boot1 <- c(Boot1_data_05$Pair_completeness_train, Boot1_data_1$Pair_completeness_train, Boot1_data_15$Pair_completeness_train, Boot1_data_2$Pair_completeness_train, Boot1_data_25$Pair_completeness_train, Boot1_data_3$Pair_completeness_train, Boot1_data_35$Pair_completeness_train, Boot1_data_4$Pair_completeness_train, Boot1_data_45$Pair_completeness_train, Boot1_data_5$Pair_completeness_train, Boot1_data_55$Pair_completeness_train, Boot1_data_6$Pair_completeness_train, Boot1_data_65$Pair_completeness_train, Boot1_data_7$Pair_completeness_train, Boot1_data_75$Pair_completeness_train, Boot1_data_8$Pair_completeness_train)
Pair_completeness_values_boot2 <- c(Boot2_data_05$Pair_completeness_train, Boot2_data_1$Pair_completeness_train, Boot2_data_15$Pair_completeness_train, Boot2_data_2$Pair_completeness_train, Boot2_data_25$Pair_completeness_train, Boot2_data_3$Pair_completeness_train, Boot2_data_35$Pair_completeness_train, Boot2_data_4$Pair_completeness_train, Boot2_data_45$Pair_completeness_train, Boot2_data_5$Pair_completeness_train, Boot2_data_55$Pair_completeness_train, Boot2_data_6$Pair_completeness_train, Boot2_data_65$Pair_completeness_train, Boot2_data_7$Pair_completeness_train, Boot2_data_75$Pair_completeness_train, Boot2_data_8$Pair_completeness_train)
Pair_completeness_values_boot3 <- c(Boot3_data_05$Pair_completeness_train, Boot3_data_1$Pair_completeness_train, Boot3_data_15$Pair_completeness_train, Boot3_data_2$Pair_completeness_train, Boot3_data_25$Pair_completeness_train, Boot3_data_3$Pair_completeness_train, Boot3_data_35$Pair_completeness_train, Boot3_data_4$Pair_completeness_train, Boot3_data_45$Pair_completeness_train, Boot3_data_5$Pair_completeness_train, Boot3_data_55$Pair_completeness_train, Boot3_data_6$Pair_completeness_train, Boot3_data_65$Pair_completeness_train, Boot3_data_7$Pair_completeness_train, Boot3_data_75$Pair_completeness_train, Boot3_data_8$Pair_completeness_train) 
Pair_completeness_values_boot4 <- c(Boot4_data_05$Pair_completeness_train, Boot4_data_1$Pair_completeness_train, Boot4_data_15$Pair_completeness_train, Boot4_data_2$Pair_completeness_train, Boot4_data_25$Pair_completeness_train, Boot4_data_3$Pair_completeness_train, Boot4_data_35$Pair_completeness_train, Boot4_data_4$Pair_completeness_train, Boot4_data_45$Pair_completeness_train, Boot4_data_5$Pair_completeness_train, Boot4_data_55$Pair_completeness_train, Boot4_data_6$Pair_completeness_train, Boot4_data_65$Pair_completeness_train, Boot4_data_7$Pair_completeness_train, Boot4_data_75$Pair_completeness_train, Boot4_data_8$Pair_completeness_train)
Pair_completeness_values_boot5 <- c(Boot5_data_05$Pair_completeness_train, Boot5_data_1$Pair_completeness_train, Boot5_data_15$Pair_completeness_train, Boot5_data_2$Pair_completeness_train, Boot5_data_25$Pair_completeness_train, Boot5_data_3$Pair_completeness_train, Boot5_data_35$Pair_completeness_train, Boot5_data_4$Pair_completeness_train, Boot5_data_45$Pair_completeness_train, Boot5_data_5$Pair_completeness_train, Boot5_data_55$Pair_completeness_train, Boot5_data_6$Pair_completeness_train, Boot5_data_65$Pair_completeness_train, Boot5_data_7$Pair_completeness_train, Boot5_data_75$Pair_completeness_train, Boot5_data_8$Pair_completeness_train)
Pair_completeness_values_boot6 <- c(Boot6_data_05$Pair_completeness_train, Boot6_data_1$Pair_completeness_train, Boot6_data_15$Pair_completeness_train, Boot6_data_2$Pair_completeness_train, Boot6_data_25$Pair_completeness_train, Boot6_data_3$Pair_completeness_train, Boot6_data_35$Pair_completeness_train, Boot6_data_4$Pair_completeness_train, Boot6_data_45$Pair_completeness_train, Boot6_data_5$Pair_completeness_train, Boot6_data_55$Pair_completeness_train, Boot6_data_6$Pair_completeness_train, Boot6_data_65$Pair_completeness_train, Boot6_data_7$Pair_completeness_train, Boot6_data_75$Pair_completeness_train, Boot6_data_8$Pair_completeness_train)

matrixPair_completeness<- matrix(nrow = 6, ncol = 16)
matrixPair_completeness[1,] <- Pair_completeness_values_boot1
matrixPair_completeness[2,] <- Pair_completeness_values_boot2
matrixPair_completeness[3,] <- Pair_completeness_values_boot3
matrixPair_completeness[4,] <- Pair_completeness_values_boot4
matrixPair_completeness[5,] <- Pair_completeness_values_boot5
matrixPair_completeness[6,] <- Pair_completeness_values_boot6

plot_matrixPair_completeness<- colMeans(matrixPair_completeness)

plot(x_axis,
     plot_matrixPair_completeness,
     bty = "L",
     type = "l",
     xlab  = "Fraction of comparisons",
     ylab = "Pair completeness",
     ylim= c(0, 0.8))

# | --------------------------------------------------------------------------
# F1 - train

F1_train_values_boot1 <- c(Boot1_data_05$F1_train, Boot1_data_1$F1_train, Boot1_data_15$F1_train, Boot1_data_2$F1_train, Boot1_data_25$F1_train, Boot1_data_3$F1_train, Boot1_data_35$F1_train, Boot1_data_4$F1_train, Boot1_data_45$F1_train, Boot1_data_5$F1_train, Boot1_data_55$F1_train, Boot1_data_6$F1_train, Boot1_data_65$F1_train, Boot1_data_7$F1_train, Boot1_data_75$F1_train, Boot1_data_8$F1_train)
F1_train_values_boot2 <- c(Boot2_data_05$F1_train, Boot2_data_1$F1_train, Boot2_data_15$F1_train, Boot2_data_2$F1_train, Boot2_data_25$F1_train, Boot2_data_3$F1_train, Boot2_data_35$F1_train, Boot2_data_4$F1_train, Boot2_data_45$F1_train, Boot2_data_5$F1_train, Boot2_data_55$F1_train, Boot2_data_6$F1_train, Boot2_data_65$F1_train, Boot2_data_7$F1_train, Boot2_data_75$F1_train, Boot2_data_8$F1_train)
F1_train_values_boot3 <- c(Boot3_data_05$F1_train, Boot3_data_1$F1_train, Boot3_data_15$F1_train, Boot3_data_2$F1_train, Boot3_data_25$F1_train, Boot3_data_3$F1_train, Boot3_data_35$F1_train, Boot3_data_4$F1_train, Boot3_data_45$F1_train, Boot3_data_5$F1_train, Boot3_data_55$F1_train, Boot3_data_6$F1_train, Boot3_data_65$F1_train, Boot3_data_7$F1_train, Boot3_data_75$F1_train, Boot3_data_8$F1_train)
F1_train_values_boot4 <- c(Boot4_data_05$F1_train, Boot4_data_1$F1_train, Boot4_data_15$F1_train, Boot4_data_2$F1_train, Boot4_data_25$F1_train, Boot4_data_3$F1_train, Boot4_data_35$F1_train, Boot4_data_4$F1_train, Boot4_data_45$F1_train, Boot4_data_5$F1_train, Boot4_data_55$F1_train, Boot4_data_6$F1_train, Boot4_data_65$F1_train, Boot4_data_7$F1_train, Boot4_data_75$F1_train, Boot4_data_8$F1_train)
F1_train_values_boot5 <- c(Boot5_data_05$F1_train, Boot5_data_1$F1_train, Boot5_data_15$F1_train, Boot5_data_2$F1_train, Boot5_data_25$F1_train, Boot5_data_3$F1_train, Boot5_data_35$F1_train, Boot5_data_4$F1_train, Boot5_data_45$F1_train, Boot5_data_5$F1_train, Boot5_data_55$F1_train, Boot5_data_6$F1_train, Boot5_data_65$F1_train, Boot5_data_7$F1_train, Boot5_data_75$F1_train, Boot5_data_8$F1_train)
F1_train_values_boot6 <- c(Boot6_data_05$F1_train, Boot6_data_1$F1_train, Boot6_data_15$F1_train, Boot6_data_2$F1_train, Boot6_data_25$F1_train, Boot6_data_3$F1_train, Boot6_data_35$F1_train, Boot6_data_4$F1_train, Boot6_data_45$F1_train, Boot6_data_5$F1_train, Boot6_data_55$F1_train, Boot6_data_6$F1_train, Boot6_data_65$F1_train, Boot6_data_7$F1_train, Boot6_data_75$F1_train, Boot6_data_8$F1_train)

matrixF1_train<- matrix(nrow = 6, ncol = 16)
matrixF1_train[1,] <- F1_train_values_boot1
matrixF1_train[2,] <- F1_train_values_boot2
matrixF1_train[3,] <- F1_train_values_boot3
matrixF1_train[4,] <- F1_train_values_boot4
matrixF1_train[5,] <- F1_train_values_boot5
matrixF1_train[6,] <- F1_train_values_boot6

plot_matrixF1_train<- colMeans(matrixF1_train)

plot(x_axis, 
     plot_matrixF1_train,
     bty = "L",
     type = "l",
     xlab  = "Fraction of comparisons",
     ylab = "F1-measure",
     ylim=c(0.02,0.2))

# F1 star - train

F1_star_train_values_boot1 <- c(Boot1_data_05$F1_star_train, Boot1_data_1$F1_star_train, Boot1_data_15$F1_star_train, Boot1_data_2$F1_star_train, Boot1_data_25$F1_star_train, Boot1_data_3$F1_star_train, Boot1_data_35$F1_star_train, Boot1_data_4$F1_star_train, Boot1_data_45$F1_star_train, Boot1_data_5$F1_star_train, Boot1_data_55$F1_star_train, Boot1_data_6$F1_star_train, Boot1_data_65$F1_star_train, Boot1_data_7$F1_star_train, Boot1_data_75$F1_star_train, Boot1_data_8$F1_star_train)
F1_star_train_values_boot2 <- c(Boot2_data_05$F1_star_train, Boot2_data_1$F1_star_train, Boot2_data_15$F1_star_train, Boot2_data_2$F1_star_train, Boot2_data_25$F1_star_train, Boot2_data_3$F1_star_train, Boot2_data_35$F1_star_train, Boot2_data_4$F1_star_train, Boot2_data_45$F1_star_train, Boot2_data_5$F1_star_train, Boot2_data_55$F1_star_train, Boot2_data_6$F1_star_train, Boot2_data_65$F1_star_train, Boot2_data_7$F1_star_train, Boot2_data_75$F1_star_train, Boot2_data_8$F1_star_train)
F1_star_train_values_boot3 <- c(Boot3_data_05$F1_star_train, Boot3_data_1$F1_star_train, Boot3_data_15$F1_star_train, Boot3_data_2$F1_star_train, Boot3_data_25$F1_star_train, Boot3_data_3$F1_star_train, Boot3_data_35$F1_star_train, Boot3_data_4$F1_star_train, Boot3_data_45$F1_star_train, Boot3_data_5$F1_star_train, Boot3_data_55$F1_star_train, Boot3_data_6$F1_star_train, Boot3_data_65$F1_star_train, Boot3_data_7$F1_star_train, Boot3_data_75$F1_star_train, Boot3_data_8$F1_star_train) 
F1_star_train_values_boot4 <- c(Boot4_data_05$F1_star_train, Boot4_data_1$F1_star_train, Boot4_data_15$F1_star_train, Boot4_data_2$F1_star_train, Boot4_data_25$F1_star_train, Boot4_data_3$F1_star_train, Boot4_data_35$F1_star_train, Boot4_data_4$F1_star_train, Boot4_data_45$F1_star_train, Boot4_data_5$F1_star_train, Boot4_data_55$F1_star_train, Boot4_data_6$F1_star_train, Boot4_data_65$F1_star_train, Boot4_data_7$F1_star_train, Boot4_data_75$F1_star_train, Boot4_data_8$F1_star_train)
F1_star_train_values_boot5 <- c(Boot5_data_05$F1_star_train, Boot5_data_1$F1_star_train, Boot5_data_15$F1_star_train, Boot5_data_2$F1_star_train, Boot5_data_25$F1_star_train, Boot5_data_3$F1_star_train, Boot5_data_35$F1_star_train, Boot5_data_4$F1_star_train, Boot5_data_45$F1_star_train, Boot5_data_5$F1_star_train, Boot5_data_55$F1_star_train, Boot5_data_6$F1_star_train, Boot5_data_65$F1_star_train, Boot5_data_7$F1_star_train, Boot5_data_75$F1_star_train, Boot5_data_8$F1_star_train)
F1_star_train_values_boot6 <- c(Boot6_data_05$F1_star_train, Boot6_data_1$F1_star_train, Boot6_data_15$F1_star_train, Boot6_data_2$F1_star_train, Boot6_data_25$F1_star_train, Boot6_data_3$F1_star_train, Boot6_data_35$F1_star_train, Boot6_data_4$F1_star_train, Boot6_data_45$F1_star_train, Boot6_data_5$F1_star_train, Boot6_data_55$F1_star_train, Boot6_data_6$F1_star_train, Boot6_data_65$F1_star_train, Boot6_data_7$F1_star_train, Boot6_data_75$F1_star_train, Boot6_data_8$F1_star_train)

matrixF1_star_train<- matrix(nrow = 6, ncol = 16)
matrixF1_star_train[1,] <- F1_star_train_values_boot1
matrixF1_star_train[2,] <- F1_star_train_values_boot2
matrixF1_star_train[3,] <- F1_star_train_values_boot3
matrixF1_star_train[4,] <- F1_star_train_values_boot4
matrixF1_star_train[5,] <- F1_star_train_values_boot5
matrixF1_star_train[6,] <- F1_star_train_values_boot6

plot_matrixF1_star_train<- colMeans(matrixF1_star_train)
plot(x_axis, 
     F1_star_smooth,
     bty = "L",
     type = "l",
     xlab  = "Fraction of comparisons",
     ylab = "F1*-measure",
     ylim=c(0,0.15))

# |--------------------Final output---------------------------

Matrix_Outputs <- matrix(ncol  = 16, nrow = 4)
colnames(Matrix_Outputs) <- x_axis

rownames(Matrix_Outputs) <- c("F1-Measure", "F1*-measure", "Pair quality", "Pair completeness")
Matrix_Outputs[1,] <- plot_matrixF1_train
Matrix_Outputs[2,] <- F1_star_smooth
Matrix_Outputs[3,] <- plot_matrixPairQuality_10
Matrix_Outputs[4,] <- smooth

save(Matrix_Outputs, file = "Final_output_for_4_measures.RData")

write.csv(Matrix_Outputs, "Outputs.csv")


