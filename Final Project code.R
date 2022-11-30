library(DescTools)
cont.table <- function(x1, x2) {
  ak <- length(which(x1==1))
  bk <- length(which(x2==1))
  ck <- length(which(x1==0))
  dk <- length(which(x2==0))
  sigma.k.sq <- 1/ak + 1/bk + 1/ck + 1/dk
  OR <- (ak*dk)/(bk*ck)
  
  return(list(
    ak=ak,
    bk=bk,
    ck=ck,
    dk=dk,
    log.or=log(OR),
    sigma.k.sq=sigma.k.sq,
    tau.k=sigma.k.sq^(-2)
  ))
}



# K=3
K = 3
alpha = -0.5
beta = 2.5
N = 70
n1 = 35
n2 = 35
x <- rep(1, 35)
x.beta <- beta*x

## K1, small N
set.seed(111)
error <- rnorm(n1, 0, 0.9)
p1 <- 1/(1+exp(-(alpha + x.beta + error)))
p2 <- 1/(1+exp(-(alpha + error)))
y1 <- rbinom(n = n1, size = 1, prob = p1) 
y2 <- rbinom(n = n2, size = 1, prob = p2)
table1 <- cont.table(y1, y2)

## K2, small N
set.seed(222)
error <- rnorm(n1, 0, 0.9)
p1 <- 1/(1+exp(-(alpha + x.beta + error)))
p2 <- 1/(1+exp(-(alpha + error)))
y1 <- rbinom(n = n1, size = 1, prob = p1)
y2 <- rbinom(n = n2, size = 1, prob = p2)
table2 <- cont.table(y1, y2)

## K3, small N
set.seed(333)
error <- rnorm(n1, 0, 0.9)
p1 <- 1/(1+exp(-(alpha + x.beta + error)))
p2 <- 1/(1+exp(-(alpha + error)))
y1 <- rbinom(n = n1, size = 1, prob = p1)
y2 <- rbinom(n = n2, size = 1, prob = p2)
table3 <- cont.table(y1, y2)

#### Contrast Test
theta <- matrix(c(table1$log.or, table2$log.or, table3$log.or), nrow = 3, ncol = 1)
Sigma <- diag(c(table1$sigma.k.sq, table2$sigma.k.sq, table3$sigma.k.sq))
C.T <- matrix(c(1, 0, -1, 1, 0, -1), nrow = 2, ncol = 3)
contrast.chisq <- t(C.T %*% theta) %*% solve(C.T %*% Sigma %*% t(C.T)) %*% C.T %*% theta
contrast.chisq
pchisq(contrast.chisq, df = 2, lower.tail = FALSE)

#### Cochran's Test
theta.C <- (table1$tau.k*table1$log.or+table2$tau.k*table2$log.or+table3$tau.k*table3$log.or)/(table1$tau.k+table2$tau.k+table3$tau.k)
cochran.chisq <- table1$tau.k*((table1$log.or - theta.C)^2) + table2$tau.k*((table2$log.or - theta.C)^2) + table3$tau.k*((table3$log.or - theta.C)^2)
cochran.chisq
pchisq(cochran.chisq, df = 2, lower.tail = FALSE)

#### Breslow Test
## MH Common OR
MH.OR = ((table1$ak * table1$dk)/N + (table2$ak * table2$dk)/N + (table3$ak * table3$dk)/N)/((table1$bk * table1$ck)/N + (table2$bk * table2$ck)/N + (table3$bk * table3$ck)/N)
df <- array(
  c(table1$ak, table1$bk, table1$ck, table1$dk, table2$ak, table2$bk, table2$ck, table2$dk, table3$ak, table3$bk, table3$ck, table3$dk), 
  dim=c(2,2,3)
)
breslow.chisq <- BreslowDayTest(df, OR = MH.OR)
breslow.chisq









####### 100 times iter
library(foreach)
library(doParallel)
ncores <- detectCores() - 1L
registerDoParallel(ncores)

K = 3
alpha = -1
beta = 2.5
N = 70
n1 = 35
n2 = 35
x <- rep(1, 35)
x.beta <- beta*x
set.seed(123)
error <- rnorm(n1, 0, 0)

contrast.results <- foreach (i = 1:100, 
         .combine = 'c') %dopar% {
  #p <- 1/(1+exp(-x.beta))
  y1 <- rbinom(n = n1, size = 1, prob = plogis(alpha + x.beta + error)) 
  y2 <- rbinom(n = n2, size = 1, prob = plogis(alpha + error))
  table1 <- cont.table(y1, y2)
  
  ## K2, small N, 0 var random
  y1 <- rbinom(n = n1, size = 1, prob = plogis(alpha + x.beta + error)) 
  y2 <- rbinom(n = n2, size = 1, prob = plogis(alpha + error)) 
  table2 <- cont.table(y1, y2)
  
  ## K3, small N, 0 var random
  y1 <- rbinom(n = n1, size = 1, prob = plogis(alpha + x.beta + error)) 
  y2 <- rbinom(n = n2, size = 1, prob = plogis(alpha + error)) 
  table3 <- cont.table(y1, y2)
  
  #### Contrast Test
  theta <- matrix(c(table1$log.or, table2$log.or, table3$log.or), nrow = 3, ncol = 1)
  Sigma <- diag(c(table1$sigma.k.sq, table2$sigma.k.sq, table3$sigma.k.sq))
  C.T <- matrix(c(1, 0, -1, 1, 0, -1), nrow = 2, ncol = 3)
  contrast.chisq <- t(C.T %*% theta) %*% solve(C.T %*% Sigma %*% t(C.T)) %*% C.T %*% theta
  contrast.chisq <- as.numeric(contrast.chisq)
}

mean(contrast.results)









