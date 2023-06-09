file_path <- "D:\\R experiments\\MCMC 3d\\NEONDSSampleLiDARPointCloud.las"
lidar_data <- readLAS(file_path)
n <- length(lidar_data$X)

# 随便抽点点，不然点太密集了，运算量过大
inx <- runif(n / 1000, min = 0, max = n)  # 抽0.1%的点
X <- lidar_data$X[inx]
Y <- lidar_data$Y[inx]
Z <- lidar_data$Z[inx]

pc <- as.matrix(cbind(X, Y, Z))  # 储存点云，每一行都是一个点云的坐标

# 把点都打出来
plot3d(X, Y, Z, size = 2, col = "green", xlim = range(X) + c(-1, 1) * 1000, ylim = range(Y) + c(-1, 1) * 1000)

# 风向随便定义一个单位向量，就说是 sqrt(2)*(1, 1)
wind_dir <- (1 / sqrt(2)) * c(1, 1)

# 把我们要投影的平面画出来
bias <- -wind_dir %*% c(max(X), max(Y))  # 平面表达式为x^T(wd1, wd2, 0)+bias = 0
planes3d(wind_dir[1], wind_dir[2], 0, bias, col = "grey")

# 把点都投到平面上去
#   随便在平面上取个点
x0 <- c(-sqrt(2) * bias - 4.112e+06, 4.112e+06, 440)

#   把x0这个点标红
plot3d(x0[1], x0[2], x0[3], col = "red", xlim = range(X) + c(-1, 1) * 1000, ylim = range(Y) + c(-1, 1) * 1000)
planes3d(wind_dir[1], wind_dir[2], 0, bias, col = "grey")

#   计算向量(xi - x0)到v的投影
XIXO <- X - x0[1]
YIYO <- Y - x0[2]
ZIZO <- Z - x0[3]

pc_mat <- as.matrix(cbind(XIXO, YIYO, ZIZO))  # 这个矩阵每一行都是一个(xi - x0)向量的坐标

sv <- pc_mat %*% c(wind_dir, 0)  # 把support vector半成品求出来，是个向量

n <- length(sv)

mapped_X <- c()
mapped_Y <- c()
mapped_Z <- c()

for (i in 1: n) {

  mapped_pc <- as.vector(pc[i, ] - sv[i] * c(wind_dir, 0))

  
  mapped_X <- c(mapped_X, mapped_pc[1])
  mapped_Y <- c(mapped_Y, mapped_pc[2])
  mapped_Z <- c(mapped_Z, mapped_pc[3])
  
}



# 把投影完成的点打到平面上，显示为紫色
plot3d(mapped_X, mapped_Y, mapped_Z, col = "purple", xlim = range(X) + c(-1, 1) * 1000, ylim = range(Y) + c(-1, 1) * 1000)
planes3d(wind_dir[1], wind_dir[2], 0, bias, col = "grey")
