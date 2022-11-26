cont.table <- function(x1, x2) {
  ak <- length(which(x1==1))
  bk <- length(which(x2==1))
  ck <- length(which(x1==0))
  dk <- length(which(x2==0))
  sigma.k.sq <- 1/ak + 1/bk + 1/ck + 1/dk
  
  return(list(
    ak=ak,
    bk=bk,
    ck=ck,
    dk=dk,
    sigma.k.sq=sigma.k.sq
  ))
}

# K=3
## K1, small N
set.seed(123)
K3.data.1.tr <- rbinom(35, 1, 0.6)
K3.data.1.ctr <- rbinom(35, 1, 0.25)
cont.table(K3.data.1.tr, K3.data.1.ctr) # 0.26

# treatment <- sample(c(0, 1), size = 70, replace = TRUE)
# xbeta <- 3*treatment
# p <- 1/(1+exp(-xbeta))
# y <- rbinom(n = 70, size = 1, prob = p)
# model1 <- glm(y ~ treatment, family = "binomial")
# summary(model1)

# error <- rnorm(70, 0, 0.25)
# model2 <- glm(y ~ treatment + error, family = "binomial")
# summary(model2)
## K2, small N
set.seed(456)
K3.data.2.tr <- rbinom(35, 1, 0.9)
K3.data.2.ctr <- rbinom(35, 1, 0.25)
cont.table(K3.data.2.tr, K3.data.2.ctr) # 0.43

## K3, small N
set.seed(789)
K3.data.3.tr <- rbinom(35, 1, 0.7)
K3.data.3.ctr <- rbinom(35, 1, 0.2)
cont.table(K3.data.3.tr, K3.data.3.ctr) # 0.76

