##ANFIS##
data <- data.frame(x1 = 0.98, x2 = 514, x3 = 294, x4 = 2, x5 = 0, x6 = 0)
ANFIS <- function(data){
  #Layer 1
  ##Fungsi Generalized Bell##
  gauss <- function(x, a, c) {
    exp(-((x - c)^2) / (2 * a^2))
  }
  
  A1 <- gauss(data$x1, 0.0727, 0.852)
  A2 <- gauss(data$x1, 0.0409, 0.677)
  B1 <- gauss(data$x2, 48.1, 596.)
  B2 <- gauss(data$x2, 41.6, 747.)
  C1 <- gauss(data$x3, 42.0, 330.)
  C2 <- gauss(data$x3, 41.6, 306.)
  D1 <- gauss(data$x4, 1.11, 3.51)
  D2 <- gauss(data$x4, 1.11, 3.51)
  E1 <- gauss(data$x5, 0.134, 0.233)
  E2 <- gauss(data$x5, 0.131, 0.227)
  F1 <- gauss(data$x6, 1.56, 2.82)
  F2 <- gauss(data$x6, 1.59, 2.85)
  
  
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
  ParameterKonsekuen <- c(643.4663, 1.027569, 0.1217273, 0.1276172, 16.61801, 0.167859, -1169.373, -452.9659, 
                          4.054853, -4.443182, -0.01028171, 10.43824, -0.04166007, -1349.98)
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
  
  hasil <- data.frame(O_5)
  
  print(hasil)
}
ANFIS(data)

# Import Data
data_test <- readxl::read_excel("C:/Users/A c e r/OneDrive/Documents/skripsi/energy+efficiency/test_data80.xlsx")
hasil3 <- ANFIS(data_test)

plot(data_test$Y, type = "l", col = "blue", lwd = 2, ylab = "Nilai", xlab = "Indeks",
     main = "Actual and Predicted Comparison Gaussian")
lines(hasil3$O_5, col = "red", lwd = 2)
legend("topleft", legend = c("Actual", "Prediction"), col = c("blue", "red"), lty = 1, cex = 0.8)


error <- hasil3$Y - hasil3$O_5
e <- error^2
rmse <- ((sum(e) / 154)^(0.5))
rmse

#benerann

data_test <- readxl::read_excel("C:/Users/A c e r/OneDrive/Documents/skripsi/energy+efficiency/test_set60.xlsx")
hasil <- readxl::read_excel("C:/Users/A c e r/OneDrive/Documents/MATLAB/resultsgauss.xlsx")

plot(data_test$Y, type = "l", col = "blue", lwd = 2, ylab = "Nilai", xlab = "Indeks",
     main = "Perbandingan Aktual dan Prediksi")
lines(hasil, col = "red", lwd = 2)
legend("topleft", legend = c("Aktual", "Prediksi"), col = c("blue", "red"), lty = 1, cex = 0.8)


error <- data_test$Y - hasil
e <- error^2
rmse <- ((sum(e) / 308)^(0.5))
rmse
