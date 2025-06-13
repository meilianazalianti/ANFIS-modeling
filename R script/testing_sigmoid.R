##ANFIS##

ANFIS <- function(data){
  #Layer 1
  ##Fungsi Generalized Bell##
  sigmoid <- function(x, a, c) {
    1 / (1 + exp(-a * (x - c)))
  }
  step=1
  
  # Untuk 5 variabel V1 hingga V5
  A1 <- sigmoid(data$x1,step,-98300000000000000000)
  A2 <- sigmoid(data$x1,step,30600000000000000000000000000000000000000)
  B1 <- sigmoid(data$x2,step,-1730000000000000000000)
  B2 <- sigmoid(data$x2,step,238000000000000000000000000000000000000000000000000)
  C1 <- sigmoid(data$x3,step,-60600000000000000)
  C2 <- sigmoid(data$x3,step,256000000000000000000000000000000000000000000000)
  D1 <- sigmoid(data$x4,step,-14500000000000000000)
  D2 <- sigmoid(data$x4,step,30600000000000000000000000000000000000000000000000)
  E1 <- sigmoid(data$x5,step,-16900000000000000000)
  E2 <- sigmoid(data$x5,step,7920000000000000000000000000000000000000000000000000)
  F1 <- sigmoid(data$x6,step,-21200000000000000000)
  F2 <- sigmoid(data$x6,step,4260000000000000000000000000000000000000000000000000)
  
  
  layer1 <- data.frame(A1, A2, B1, B2, C1, C2, D1, D2, E1, E2, F1, F2)
  
  #Layer 2
  O_21 <- layer1$A1 * layer1$B1 * layer1$C1 * layer1$D1*layer1$E1 * layer1$F1
  O_22 <- layer1$A2 * layer1$B2 * layer1$C2 * layer1$D2*layer1$E2 * layer1$F2
  
  layer2 <- data.frame(O_21, O_22)
  
  #Layer 3
  O_31 <- (layer2$O_21 / (layer2$O_21 + layer2$O_22))
  O_32 <- (layer2$O_22 / (layer2$O_21 + layer2$O_22))
  
  layer3 <- data.frame(O_31, O_32)
  
  #Layer 4
  ParameterKonsekuen <- c(8.7216952877, -0.0496600620, 0.1362014441, 0.1616250177, 16.8516500519, 
                          0.0516386952, 5.1430463634, -0.0244071752, 0.0834931813, -0.1568582051, 
                          0.0922446466, 1.2620489370, 0.1803402228, 0.0005450145)
  PK <- data.frame(ParameterKonsekuen)
  PK
  
  #Output Layer 4
  f <- function(ci1, ci2, ci3, ci4, ci5,ci6, ci0, x1, x2, x3, x4,x5,x6){ci1 * x1 + ci2 * x2 + ci3 * x3 + ci4 * x4+ci5 * x5+ci6 * x6 + ci0}
  f1 <- f(PK[1,], PK[2,], PK[3,], PK[4,], PK[5,], PK[6,], PK[7,], data$x1, data$x2, data$x3, data$x4,data$x5,data$x6)
  
  f2 <- f(PK[8,], PK[9,], PK[10,], PK[11,], PK[12,],PK[13,], PK[14,],data$x1, data$x2, data$x3, data$x4,data$x5,data$x6)
  tabelf <- data.frame(f1,f2)
  
  O_41 <- O_31*f1
  O_42 <- O_32*f2
  
  layer4 <- data.frame(O_41,O_42)
  
  #Layer 5
  O_5 <- O_41+O_42
  
  hasil <- data.frame(data[,7],O_5)
  
  print(hasil)
}

# Import Data
data_test <- readxl::read_excel("C:/Users/A c e r/OneDrive/Documents/skripsi/energy+efficiency/test_data80.xlsx")
hasil3 <- ANFIS(data_test)

plot(data_test$Y, type = "l", col = "blue", lwd = 2, ylab = "Nilai", xlab = "Indeks",
     main = "Actual and Predicted Comparison")
lines(hasil3$O_5, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Prediction"), col = c("blue", "red"), lty = 1, cex = 0.8)


error <- hasil3$Y - hasil3$O_5
e <- error^2
rmse <- ((sum(e) / 154)^(0.5))
rmse


data_test <- readxl::read_excel("C:/Users/A c e r/OneDrive/Documents/skripsi/energy+efficiency/test_set90.xlsx")
hasil <- readxl::read_excel("C:/Users/A c e r/OneDrive/Documents/MATLAB/resultssigmo.xlsx")

plot(data_test$Y, type = "l", col = "blue", lwd = 2, ylab = "Nilai", xlab = "Indeks",
     main = "Perbandingan Aktual dan Prediksi")
lines(hasil, col = "red", lwd = 2)
legend("topleft", legend = c("Aktual", "Prediksi"), col = c("blue", "red"), lty = 1, cex = 0.8)


error <- data_test$Y - hasil
e <- error^2
rmse <- ((sum(e) / 308)^(0.5))
rmse
