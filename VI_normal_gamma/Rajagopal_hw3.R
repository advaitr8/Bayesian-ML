x_1 <- read.csv("X_set1.csv", header=F)
x_2 <- read.csv("X_set2.csv", header=F)
x_3 <- read.csv("X_set3.csv", header=F)
#
y_1 <- read.csv("y_set1.csv", header=F)
y_2 <- read.csv("y_set2.csv", header=F)
y_3 <- read.csv("y_set3.csv", header=F)
#
z_1 <- read.csv("z_set1.csv", header=F)
z_2 <- read.csv("z_set2.csv", header=F)
z_3 <- read.csv("z_set3.csv", header=F)

##
vi_function <- function(a_0,b_0,e_0,f_0,x,y,iter){
  d = dim(x)[2]
  N = dim(x)[1]
  #preparing data
  xixi = matrix(0, nrow=d, ncol=d)
  yixi = matrix(0, nrow=d, ncol=1)
  
  for(i in 1:N){
    xit  = as.numeric(matrix(x[i,], nrow=d, ncol=1))
    yit  = y[i,1]
    
    xixi = xixi + xit %*% t(xit)
    yixi = yixi + yit*xit
    
  }
  #initialize starting values
  alpha	= diag(a_0/b_0, d)
  lambda	= e_0/f_0
  
  mu_w	= matrix(0, nrow=d, ncol=1)
  sigma_w	= matrix(0, nrow=d, ncol=d)
  
  e_1	= e_0 + 0.5*N
  f_1	= f_0
  
  a_1	= a_0 + 0.5
  b_1	= rep(b_0, d)
  
  obj_L	= c()
  h_l	= c()
  
  for(j in 1:iter){
    sigma_w = solve(drop(lambda)*xixi + alpha)
    mu_w = solve(drop(lambda)*xixi + alpha,drop(lambda)*yixi)

    df = 0
    for(i in 1:N){
      df = df + (y[i,1] - (t(as.numeric(matrix(x[i,], nrow=d, ncol=1))) %*% mu_w))^2 +  t(as.numeric(matrix(x[i,], nrow=d, ncol=1))) %*% sigma_w %*% as.numeric(matrix(x[i,], nrow=d, ncol=1))
    }
    f_1 = f_0 + 0.5*df	
    
    for(i in 1:d){
      b_1[i]		= b_0 + 0.5*(mu_w[i,1]*mu_w[i,1] + sigma_w[i,i])
      alpha[i,i] 	= a_1/b_1[i]
    }
    lambda	= e_1/f_1
    H	= drop(lambda)*xixi + alpha 
    ob	= determinant(H, logarithm=TRUE)
    obv	= ob$modulus*ob$sign
    obj_L	= c(obj_L, 0.5*obv + 1)
    h_l	= c(h_l, det(H))
    print(j)
  }
  return(list(sw=sigma_w, mw=mu_w, a1=a_1, b1=b_1, e1=e_1, f1=f_1, obj=obj_L, HL=h_l))
}


test1 <- vi_function(1e-16, 1e-16, 1, 1, x_1, y_1, 500)
plot(test1$obj,type="l")

test2 <- vi_function(1e-16, 1e-16, 1, 1, x_2, y_2, 500)
plot(test2$obj,type="l")

test3 <- vi_function(1e-16, 1e-16, 1, 1, x_3, y_3, 500)
plot(test3$obj,type="l")

######
#plots
######
rn1 <- test1
rn2 <- test2
rn3 <- test3
## part A
par(mfcol = c(3,1))
plot(rn1$obj[3:500],
     type = 'l',
     xlab = "iterations",
     ylab = "VI objective function",
     main = "dataset 1 [N = 100]",
     bty = 'n',
     yaxt = 'n',
     xaxt = 'n',
     cex.main = 0.8,
     cex.lab = 0.8)
axis (2, at = c(10,110,210),
      lwd = 0.5 ,
      cex.axis = 0.8)
axis (1, at = c(0,100,200,300,400,500),
      lwd = 0.5 ,
      cex.axis = 0.8)
##
plot(rn2$obj[6:500],
     type = 'l',
     xlab = "iterations",
     ylab = "VI objective function",
     main = "dataset 2 [N = 250]",
     bty = 'n',
     yaxt = 'n',
     xaxt = 'n',
     cex.main = 0.8,
     cex.lab = 0.8)
axis (2, at = c(100,300,500),
      lwd = 0.5 ,
      cex.axis = 0.8)
axis (1, at = c(0,100,200,300,400,500),
      lwd = 0.5 ,
      cex.axis = 0.8)
##
plot(rn3$obj[1:500],
     type = 'l',
     xlab = "iterations",
     ylab = "VI objective function",
     main = "dataset 3 [N = 500]",
     bty = 'n',
     yaxt = 'n',
     xaxt = 'n',
     cex.main = 0.8,
     cex.lab = 0.8)
axis (2, at = c(100,300,500,700,900),
      lwd = 0.5 ,
      cex.axis = 0.8)
axis (1, at = c(0,100,200,300,400,500),
      lwd = 0.5 ,
      cex.axis = 0.8)


#part B
par(mfcol = c(3,1))
eq_alpha_k1 <- rn1$b1/rn1$a1
plot(eq_alpha_k1,
     pch = 16,
     cex = 0.7,
     xlab = "k from 1:101",
     ylab = "b_prime/a_prime",
     main = "dataset 1 [N = 100]",
     bty = 'n',
     yaxt = 'n',
     xaxt = 'n',
     cex.main = 0.8,
     cex.lab = 0.8)
lines(eq_alpha_k1,
      col = "gray",
      lwd = 0.7)
points(eq_alpha_k1,
       pch = 16,
       cex = 0.7)
axis (2, at = c(0,50,100,150),
      lwd = 0.5 ,
      cex.axis = 0.8)
axis (1, at = seq(0,100, length.out = 11),
      lwd = 0.5 ,
      cex.axis = 0.8)
#
eq_alpha_k2 <- rn2$b1/rn2$a1
plot(eq_alpha_k2,
     pch = 16,
     cex = 0.7,
     xlab = "k from 1:101",
     ylab = "b_prime/a_prime",
     main = "dataset 2 [N = 250]",
     bty = 'n',
     yaxt = 'n',
     xaxt = 'n',
     cex.main = 0.8,
     cex.lab = 0.8)
lines(eq_alpha_k2,
      col = "gray",
      lwd = 0.7)
points(eq_alpha_k2,
       pch = 16,
       cex = 0.7)
axis (1, at = seq(0,250, length.out = 11),
      lwd = 0.5 ,
      cex.axis = 0.8)
axis (2, at = c(0,50,100,150),
      lwd = 0.5 ,
      cex.axis = 0.8)
#
eq_alpha_k3 <- rn3$b1/rn2$a1
plot(eq_alpha_k3,
     pch = 16,
     cex = 0.7,
     xlab = "k from 1:101",
     ylab = "b_prime/a_prime",
     main = "dataset 3 [N = 500]",
     bty = 'n',
     yaxt = 'n',
     xaxt = 'n',
     cex.main = 0.8,
     cex.lab = 0.8)
lines(eq_alpha_k3,
      col = "gray",
      lwd = 0.7)
points(eq_alpha_k3,
       pch = 16,
       cex = 0.7)
axis (1, at = seq(0,500, length.out = 11),
      lwd = 0.5 ,
      cex.axis = 0.8)
axis (2, at = c(0,50,100),
      lwd = 0.5 ,
      cex.axis = 0.8)


#part C
rn1$f1/rn1$e1
rn2$f1/rn2$e1
rn3$f1/rn3$e1


#part D
#plot y_hat vs z
# par(mfcol = c(1,3))
y_i_hat_1 <- rep(0,100)
for(i in 1:100){
  y_i_hat_1[i] <- as.numeric(matrix(x_1[i,], nrow = 1, ncol = 101))%*%rn1$mw
}
plot(z_1$V1,unlist(y_i_hat_1),
     type = 'l',
     xlab = "z values",
     ylab = "y_hat/y_i",
     main = "dataset 1 [N = 100]",
     xaxt = 'n',
     yaxt = 'n',
     cex.lab = 0.7,
     cex.main = 0.8,
     col = "red",
     bty = 'n',
     lwd = 1.5)
axis (1, at = seq(-5,5),
      lwd = 0.5 ,
      cex.axis = 0.8)
axis (2, at = seq(-2,10, by = 2),
      lwd = 0.5 ,
      cex.axis = 0.8)
points(z_1$V1,y_1$V1,
       pch = 16,
       cex = 0.7)
sinc_stuff1 <- rdetools::sinc(z_1$V1)
lines(z_1$V1, 10*sinc_stuff1,
      lwd = 1.5)
legend('topright',
       legend = c("y_hat", "data", "ground truth"),
       pch = c(NA,16,NA),
       lty = c(1,NA,1),
       col = c("red","black","black"),
       bty = 'n',
       cex = 0.7)
###
y_i_hat_2 <- rep(0,250)
for(i in 1:250){
  y_i_hat_2[i] <- as.numeric(matrix(x_2[i,], nrow = 1, ncol = 251))%*%rn2$mw
}
plot(z_2$V1,unlist(y_i_hat_2),
     type = 'l',
     xlab = "z values",
     ylab = "y_hat/y_i",
     main = "dataset 2 [N = 250]",
     xaxt = 'n',
     yaxt = 'n',
     cex.lab = 0.7,
     cex.main = 0.8,
     col = "red",
     bty = 'n',
     lwd = 1.5)
axis (1, at = seq(-5,5),
      lwd = 0.5 ,
      cex.axis = 0.8)
axis (2, at = seq(-2,10, by = 2),
      lwd = 0.5 ,
      cex.axis = 0.8)
points(z_2$V1,y_2$V1,
       pch = 16,
       cex = 0.7)
sinc_stuff2 <- rdetools::sinc(z_2$V1)
lines(z_2$V1, 10*sinc_stuff2,
      lwd = 1.5)
legend('topright',
       legend = c("y_hat", "data", "ground truth"),
       pch = c(NA,16,NA),
       lty = c(1,NA,1),
       col = c("red","black","black"),
       bty = 'n',
       cex = 0.7)
####
y_i_hat_3 <- rep(0,500)
for(i in 1:500){
  y_i_hat_3[i] <- as.numeric(matrix(x_3[i,], nrow = 1, ncol = 501))%*%rn3$mw
}
plot(z_3$V1,unlist(y_i_hat_3),
     type = 'l',
     xlab = "z values",
     ylab = "y_hat/y_i",
     main = "dataset 3 [N = 500]",
     xaxt = 'n',
     yaxt = 'n',
     cex.lab = 0.7,
     cex.main = 0.8,
     col = "red",
     bty = 'n',
     lwd = 1.5)
axis (1, at = seq(-5,5),
      lwd = 0.5 ,
      cex.axis = 0.8)
axis (2, at = seq(-2,10, by = 2),
      lwd = 0.5 ,
      cex.axis = 0.8)
points(z_3$V1,y_3$V1,
       pch = 16,
       cex = 0.7)
sinc_stuff3 <- rdetools::sinc(z_3$V1)
lines(z_3$V1, 10*sinc_stuff3,
      lwd = 1.5)
legend('topright',
       legend = c("y_hat", "data", "ground truth"),
       pch = c(NA,16,NA),
       lty = c(1,NA,1),
       col = c("red","black","black"),
       bty = 'n',
       cex = 0.7)


