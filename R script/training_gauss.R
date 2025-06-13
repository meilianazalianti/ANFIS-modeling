rm(list = ls())
data <- readxl::read_excel("C:/Users/A c e r/OneDrive/Documents/skripsi/energy+efficiency/train_data80.xlsx")
data
# Normalisasi data
normalisasi <- function(x, xmax, xmin){(x - xmin) / (xmax - xmin)}

xmax1 <- max(data[, 1])
xmax2 <- max(data[, 2])
xmax3 <- max(data[, 3])
xmax4 <- max(data[, 4])
xmax5 <- max(data[, 5])
xmax6 <- max(data[, 6])
xmax7 <- max(data[, 7])
xmin1 <- min(data[, 1])
xmin2 <- min(data[, 2])
xmin3 <- min(data[, 3])
xmin4 <- min(data[, 4])
xmin5 <- min(data[, 5])
xmin6 <- min(data[, 6])
xmin7 <- min(data[, 7])

data[, 1] <- normalisasi(data[, 1], xmax1, xmin1)
data[, 2] <- normalisasi(data[, 2], xmax2, xmin2)
data[, 3] <- normalisasi(data[, 3], xmax3, xmin3)
data[, 4] <- normalisasi(data[, 4], xmax4, xmin4)
data[, 5] <- normalisasi(data[, 5], xmax5, xmin5)
data[, 6] <- normalisasi(data[, 6], xmax6, xmin6)
data[, 7] <- normalisasi(data[, 7], xmax7, xmin7)
data

# Memuat library yang diperlukan
library(tidyverse)  # Untuk manipulasi data
library(factoextra) # Untuk visualisasi clustering

# Clustering K-Means
kmeans.result <- kmeans(data[, 1:6], centers = 2, nstart = 25)  # Menggunakan K-Means dengan 2 cluster

# Menampilkan hasil clustering
print(kmeans.result)


# Fungsi untuk denormalisasi data
denormalized <- function(x, xmax, xmin){(x * (xmax - xmin) + xmin)}

# Menerapkan fungsi denormalisasi pada beberapa kolom
data[, 1] <- denormalized(data[, 1], xmax1, xmin1)
data[, 2] <- denormalized(data[, 2], xmax2, xmin2)
data[, 3] <- denormalized(data[, 3], xmax3, xmin3)
data[, 4] <- denormalized(data[, 4], xmax4, xmin4)
data[, 5] <- denormalized(data[, 5], xmax5, xmin5)
data[, 6] <- denormalized(data[, 6], xmax6, xmin6)
data[, 7] <- denormalized(data[, 7], xmax7, xmin7)
data

# Membuat data frame hasil clustering
df.cluster <- data.frame(data, kmeans.result$cluster)
df.cluster

# Visualisasi cluster
fviz_cluster(kmeans.result, data = data[, 1:6],
             geom = "point", stand = FALSE, 
             ellipse.type = "convex", 
             ggtheme = theme_minimal()) +
  labs(title = "Clustering K-Means")

# Menghitung rata-rata untuk setiap cluster
Mean <- data[, 1:6] %>%
  mutate(Cluster = kmeans.result$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
Mean

# Menghitung standar deviasi untuk setiap cluster
std <- data[, 1:6] %>%
  mutate(Cluster = kmeans.result$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("sd")
std

## TRAINING MODEL ANFIS ##
train_ANFIS <- function(data,epoch,learning_rate){
  rest_list <- list()
  for (iter in seq_len(epoch))
    rest_list[[iter]] <- {
      #Layer 1
      ##Fungsi Generalized Bell##
      gauss <- function(x, a, c) {
        exp(-((x - c)^2) / (2 * a^2))
      }
      
      # Untuk 5 variabel V1 hingga V5
      A1 <- gauss(data$x1, as.numeric(std[1, 2]),as.numeric(Mean[1, 2]))
      A2 <- gauss(data$x1, as.numeric(std[2, 2]),as.numeric(Mean[2, 2]))
      B1 <- gauss(data$x2, as.numeric(std[1, 3]),as.numeric(Mean[1, 3]))
      B2 <- gauss(data$x2, as.numeric(std[2, 3]),as.numeric(Mean[2, 3]))
      C1 <- gauss(data$x3, as.numeric(std[1, 4]),as.numeric(Mean[1, 4]))
      C2 <- gauss(data$x3, as.numeric(std[2, 4]),as.numeric(Mean[2, 4]))
      D1 <- gauss(data$x4, as.numeric(std[1, 5]),as.numeric(Mean[1, 5]))
      D2 <- gauss(data$x4, as.numeric(std[2, 5]),as.numeric(Mean[2, 5]))
      E1 <- gauss(data$x5, as.numeric(std[1, 6]),as.numeric(Mean[1, 6]))
      E2 <- gauss(data$x5, as.numeric(std[2, 6]),as.numeric(Mean[2, 6]))
      F1 <- gauss(data$x6, as.numeric(std[1, 7]),as.numeric(Mean[1, 7]))
      F2 <- gauss(data$x6, as.numeric(std[2, 7]),as.numeric(Mean[2, 7]))
      
      layer1 <- data.frame(A1, A2, B1, B2, C1, C2, D1, D2, E1, E2, F1, F2)
      layer1
      
      
      #Layer 2
      O_21 <- layer1$A1 * layer1$B1 * layer1$C1 * layer1$D1*layer1$E1 * layer1$F1
      O_22 <- layer1$A2 * layer1$B2 * layer1$C2 * layer1$D2*layer1$E2 * layer1$F2
      
      layer2 <- data.frame(O_21,O_22)
      layer2
      
      #Layer 3
      O_31 <- (layer2$O_21/(layer2$O_21+layer2$O_22))
      O_32 <- (layer2$O_22/(layer2$O_21+layer2$O_22))
      layer3 <- data.frame(O_31,O_32)
      layer3
      
      #Layer 4
      
      #####Perhitungan Parameter Konsekuen dengan Least Square Estimator#####
      c11 <- O_31 * data$x1
      c12 <- O_31 * data$x2
      c13 <- O_31 * data$x3
      c14 <- O_31 * data$x4
      c15 <- O_31 * data$x5
      c16 <- O_31 * data$x6
      c10 <- O_31  # Bias
      
      c21 <- O_32 * data$x1
      c22 <- O_32 * data$x2
      c23 <- O_32 * data$x3
      c24 <- O_32 * data$x4
      c25 <- O_32 * data$x5
      c26 <- O_32 * data$x6
      c20 <- O_32  # Bias
      
      
      # Matriks Desain A
      A <- matrix(c(c11, c12, c13, c14, c15, c16, c10, c21, c22, c23, c24, c25,c26, c20), 691, 14)
      A
      #Transpose Matriks A
      A_T <- t(A)
      #Perkalian A_T dengan A
      B <- A_T%*%A
      #Invers A_T*A
      BI <- solve(B)
      BI
      Target <- matrix(data$Y)
      Target
      y <- A_T%*%Target
      #Parameter Konsekuen
      Parameter_Konsekuen <- BI%*%y
      PK <- data.frame(Parameter_Konsekuen)
      PK
      
      ##Output Layer 4
      f <- function(ci1, ci2, ci3, ci4, ci5,ci6, ci0, x1, x2, x3, x4,x5,x6){ci1 * x1 + ci2 * x2 + ci3 * x3 + ci4 * x4+ci5 * x5+ci6 * x6 + ci0}
      f1 <- f(PK[1,], PK[2,], PK[3,], PK[4,], PK[5,], PK[6,], PK[7,], data$x1, data$x2, data$x3, data$x4,data$x5,data$x6)
      
      f2 <- f(PK[8,], PK[9,], PK[10,], PK[11,], PK[12,],PK[13,], PK[14,],data$x1, data$x2, data$x3, data$x4,data$x5,data$x6)
      tabelf <- data.frame(f1,f2)
      tabelf
      
      O_41 <- O_31*f1
      O_42 <- O_32*f2
      
      layer4 <- data.frame(O_41,O_42)
      layer4
      
      ##Layer 5
      O_5 <- O_41+O_42
      O_5
      
      hasil <- data.frame(data[,7],O_5)
      hasil
      
      error <- data$Y-O_5
      e <- error^2
      rmse <- ((sum(e)/691)^(0.5))
      rmse
      
      #Perbandingan Output Jaringan dan Output Aktual
      Tabel <- data.frame(hasil,error)
      Tabel
      
      #Layer 5
      e5 <- (-2)*(Tabel$Y - Tabel$O_5)
      e5
      ratarata <- mean(e5)
      ratarata
      
      #Layer 4
      e4a <- ratarata
      e4b <- ratarata
      e4 <- c(e4a,e4b)
      e4
      
      #Layer 3
      e3a <- e4a*f1
      e3b <- e4b*f2
      e3 <- data.frame(e3a,e3b)
      e3
      meane3a <- mean(e3a)
      meane3b <- mean(e3b)
      
      #Layer 2
      e2a <- (layer2$O_22/(layer2$O_21+layer2$O_22)^2)*(meane3a-meane3b)
      
      e2b <- (layer2$O_21/(layer2$O_21+layer2$O_22)^2)*(meane3b-meane3a)  
      e2 <- data.frame(e2a,e2b)  
      e2  
      meane2 <- c(mean(e2a),mean(e2b))  
      meane2  
      
      ##Layer 1  
      e1f <- mean(e2a)*A1*B1*C1*D1*E1
      e2f <- mean(e2b)*A2*B2*C2*D2*E2
      e1e <- mean(e2a)*A1*B1*C1*D1*F1
      e2e <- mean(e2b)*A2*B2*C2*D2*F2
      e1d <- mean(e2a)*A1*B1*C1*E1*F1
      e2d <- mean(e2b)*A2*B2*C2*E2*F2
      e1c <- mean(e2a)*A1*B1*D1*E1*F1
      e2c <- mean(e2b)*A2*B2*D2*E2*F2
      e1b <- mean(e2a)*A1*C1*D1*E1*F1
      e2b <- mean(e2b)*A2*C2*D2*E2*F2
      e1a <- mean(e2a)*B1*C1*D1*E1*F1
      e2a <- mean(e2b)*B2*C2*D2*E2*F2
      e1 <- data.frame(e1f,e2f,e1e,e2e,e1d,e2d,e1c,e2c,e1b,e2b,e1a,e2a)
      rataratae1 <- c(mean(e1f),mean(e2f),mean(e1e),mean(e2e),mean(e1d),mean(e2d),mean(e1c),mean(e2c),mean(e1b),mean(e2b),mean(e1a),mean(e2a))
      rataratae1
      
      ##Rata-rata variabel input  
      avrx1 <- mean(data$x1)
      avrx2 <- mean(data$x2)
      avrx3 <- mean(data$x3)
      avrx4 <- mean(data$x4)
      avrx5 <- mean(data$x5)
      avrx6 <- mean(data$x6)
      avrx <- c(avrx1, avrx2,avrx3,avrx4,avrx5,avrx6)
      avrx 
      
      ##Menghitung nilai error pada parameter mean dan standar deviasi  
      ec <- function(x,a,c,e){((2*(x-c)^2)/(a^3*(1+((x-c)/a)^2)^2))*e}
      ea <- function(x,a,c,e){((2*(x-c)^2)/(a^2*(1+((x-c)/a)^2)^2))*e}
      
      # Parameter mean
      x11 <- ec(avrx1,std[1,2],Mean[1,2],mean(e1a))
      x12 <- ec(avrx2,std[2,2],Mean[2,2],mean(e2a))
      x21 <- ec(avrx2,std[1,3],Mean[1,3],mean(e1b))
      x22 <- ec(avrx2,std[2,3],Mean[2,3],mean(e2b))
      x31 <- ec(avrx3,std[1,2],Mean[1,2],mean(e1c))
      x32 <- ec(avrx3,std[2,2],Mean[2,2],mean(e2c))
      x41 <- ec(avrx4,std[1,2],Mean[1,2],mean(e1d))
      x42 <- ec(avrx4,std[2,2],Mean[2,2],mean(e2d))
      x51 <- ec(avrx5,std[1,2],Mean[1,2],mean(e1e))
      x52 <- ec(avrx5,std[2,2],Mean[2,2],mean(e2e))
      x61 <- ec(avrx6,std[1,2],Mean[1,2],mean(e1f))
      x62 <- ec(avrx6,std[2,2],Mean[2,2],mean(e2f))
      
      Error_Mean <- data.frame(matrix(c(x11,x12,x21,x22,x31,x32,x41,x42,x51,x52,x61,x62),2,6))
      Error_Mean  
      
      #Parameter sd  
      x11a <- ea(avrx1,std[1,2],Mean[1,2],mean(e1a))
      x12a <- ea(avrx2,std[2,2],Mean[2,2],mean(e2a))
      x21a <- ea(avrx2,std[1,3],Mean[1,3],mean(e1b))
      x22a <- ea(avrx2,std[2,3],Mean[2,3],mean(e2b))
      x31a <- ea(avrx3,std[1,2],Mean[1,2],mean(e1c))
      x32a <- ea(avrx3,std[2,2],Mean[2,2],mean(e2c))
      x41a <- ea(avrx4,std[1,2],Mean[1,2],mean(e1d))
      x42a <- ea(avrx4,std[2,2],Mean[2,2],mean(e2d))
      x51a <- ea(avrx5,std[1,2],Mean[1,2],mean(e1e))
      x52a <- ea(avrx5,std[2,2],Mean[2,2],mean(e2e))
      x61a <- ea(avrx6,std[1,2],Mean[1,2],mean(e1f))
      x62a <- ea(avrx6,std[2,2],Mean[2,2],mean(e2f))
      
      Error_Std <<- data.frame(matrix(c(x11a,x12a,x21a,x22a,x31a,x32a,x41a,x42a,x51a,x52a,x61a,x62a),2,6))
      Error_Std  
      
      #nilai learning rate = 0.1 learning rate n  
      #Perubahan parameter mean 
      delta_c <- function(n,e,c){n*e*c}
      x1c <- delta_c (learning_rate,x11, avrx1)
      x1c2 <- delta_c (learning_rate,x12,avrx1)
      x2c <- delta_c (learning_rate,x21, avrx2)
      x2c2 <- delta_c (learning_rate,x22,avrx2)
      x3c <- delta_c (learning_rate,x31, avrx3)
      x3c2 <- delta_c (learning_rate,x32,avrx3)
      x4c <- delta_c (learning_rate,x41, avrx4)
      x4c2 <- delta_c (learning_rate,x42,avrx4)
      x5c <- delta_c (learning_rate,x51, avrx5)
      x5c2 <- delta_c (learning_rate,x52,avrx5)
      x6c <- delta_c (learning_rate,x61,avrx6)
      x6c2 <- delta_c (learning_rate,x62,avrx6)
      
      xc <- data.frame(matrix(c(x1c,x1c2,x2c,x2c2,x3c,x3c2,x4c,x4c2,x5c,x5c2,x6c,x6c2),2,6))
      xc
      
      #Perubahan parameter sd  
      delta_a <- function(n,e) (n*e)
      x1a <- delta_a (learning_rate,x11a)
      x1a2 <- delta_a (learning_rate,x12a)
      x2a <- delta_a (learning_rate,x21a)
      x2a2 <- delta_a (learning_rate,x22a)
      x3a <- delta_a (learning_rate,x31a)
      x3a2 <- delta_a (learning_rate,x32a)
      x4a <- delta_a (learning_rate,x41a)
      x4a2 <- delta_a (learning_rate,x42a)
      x5a <- delta_a (learning_rate,x51a)
      x5a2 <- delta_a (learning_rate,x52a)
      x6a <- delta_a (learning_rate,x61a)
      x6a2 <- delta_a (learning_rate,x62a)
      
      xa <- data.frame(matrix(c(x1a,x1a2,x2a,x2a2,x3a,x3a2,x4a,x4a2,x5a,x5a2,x6a,x6a2),2,6))
      xa  
      
      #Mean Baru  
      Mean[1,2] <- Mean[1,2]+x1c
      Mean[2,2] <- Mean[2,2]+x1c2
      Mean[1,3] <- Mean[1,3]+x2c
      Mean[2,3] <- Mean[2,3]+x2c2
      Mean[1,4] <- Mean[1,4]+x3c
      Mean[2,4] <- Mean[2,4]+x3c2
      Mean[1,5] <- Mean[1,5]+x4c
      Mean[2,5] <- Mean[2,5]+x4c2
      Mean[1,6] <- Mean[1,6]+x5c
      Mean[2,6] <- Mean[2,6]+x5c2
      Mean[1,7] <- Mean[1,7]+x6c
      Mean[2,7] <- Mean[2,7]+x6c2
      Mean
      
      #Sd baru  
      std[1,2] <- std[1,2]+x1a
      std[2,2] <- std[2,2]+x1a2
      std[1,3] <- std[1,3]+x2a
      std[2,3] <- std[2,3]+x2a2
      std[1,4] <- std[1,4]+x3a
      std[2,4] <- std[2,4]+x3a2
      std[1,5] <- std[1,5]+x4a
      std[2,5] <- std[2,5]+x4a2
      std[1,6] <- std[1,6]+x5a
      std[2,6] <- std[2,6]+x5a2
      std[1,7] <- std[1,7]+x6a
      std[2,7] <- std[2,7]+x6a2
      std  
      
      
      cat("RMSE","\n")  
      print(rmse)  
      
      
    }
  return(rest_list)
}


train_ANFIS(data,500,0.9)

