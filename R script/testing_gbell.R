##ANFIS##
data <- data.frame(x1 = 0.98, x2 = 514, x3 = 294, x4 = 2, x5 = 0, x6 = 0)
ANFIS <- function(data){
  #Layer 1
  ##Fungsi Generalized Bell##
  gbell <- function(x,a,b,c){1/(1+(abs((x-c)/a))^(2*b))}
  
  A1 <- gbell(data$x1, 0.0727,2, 0.852)
  A2 <- gbell(data$x1, 0.0409,2, 0.677)
  B1 <- gbell(data$x2, 48.1,2, 596.)
  B2 <- gbell(data$x2, 41.6,2, 747.)
  C1 <- gbell(data$x3, 42.0,2, 330.)
  C2 <- gbell(data$x3, 41.6,2, 306.)
  D1 <- gbell(data$x4, 1.11,2, 3.51)
  D2 <- gbell(data$x4, 1.11,2, 3.51)
  E1 <- gbell(data$x5, 0.134,2, 0.233)
  E2 <- gbell(data$x5, 0.131,2, 0.227)
  F1 <- gbell(data$x6, 1.56,2, 2.82)
  F2 <- gbell(data$x6, 1.59,2, 2.85)
  
  
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
  #ParameterKonsekuen <- c(-0.001676, -0.6837, 0.0009517, 101.1, 245.8, -37.31, -0.001751,
                          #-0.004406, -0.6284, 0.4074, -64.78, 49.91, -28.95, -0.003728)
  ParameterKonsekuen <- c(336.54115817,
                          0.50871686,0.20257983,0.14905181,17.07209243,0.14417837,-624.08375030,-595.73821669,
                          -0.08229729,-0.47611261,-0.03613106,9.84829036,-0.02847582,622.23833911)
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
data_test <- readxl::read_excel("C:/Users/A c e r/OneDrive/Documents/skripsi/energy+efficiency/test_set60.xlsx")
hasil <- readxl::read_excel("C:/Users/A c e r/OneDrive/Documents/MATLAB/resultsgbell.xlsx")

plot(data_test$Y, type = "l", col = "blue", lwd = 2, ylab = "Nilai", xlab = "Indeks",
     main = "Perbandingan Aktual dan Prediksi")
lines(hasil, col = "red", lwd = 2)
legend("topleft", legend = c("Aktual", "Prediksi"), col = c("blue", "red"), lty = 1, cex = 0.8)


error <- data_test$Y - hasil
e <- error^2
rmse <- ((sum(e) / 308)^(0.5))
rmse
