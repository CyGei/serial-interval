# LHS package -------------------------------------------------------------
library(lhs)
library(truncnorm)
set.seed(123)
A <- randomLHS(5, 2)  # 5 sample points from 2 parameters
A
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B #incubation lhs matrix

min_mu = 4.9
min_sd = 2.2 
max_mu = 6.7 
max_sd = 5.2
min_cv <- min_sd/min_mu
max_cv <- max_sd/max_mu

B[,1] <- qtruncnorm(
  A[, 1],
  mean = median(min_mu:max_mu),
  sd = median(min_sd:max_sd),
  a = min_mu,
  b = max_mu
) # mean distribution

B[,2]<- qtruncnorm(
  A[, 1],
  mean = median(min_cv:max_cv),
  sd = median(min_sd:max_sd),
  a = min_cv,
  b = max_cv
) #cv distribution

B
f <- function(x){qtruncnorm(x, mean = median(min_cv:max_cv),
                            sd = median(min_sd:max_sd),
                            a = min_mu, b = max_mu)} 
g <- function(x){qtruncnorm(x, mean = median(min_cv:max_cv),
                            sd = median(min_sd:max_sd),
                            a = min_cv, b = max_cv)} 

graph2dLHSTransform(B[,1:2], f, g, min(B[,1]), max(B[,1]), min(A[,1]), max(B[,1])) # run function below



# 4 params ----------------------------------------------------------------
library(lhs)
library(truncnorm)
set.seed(123)
A <- randomLHS(5, 4)  # 5 sample points from 4 parameters
A
B <- matrix(nrow = nrow(A), ncol = ncol(A))
B #incubation lhs matrix

min_mu = 4.9
min_sd = 2.2 
max_mu = 6.7 
max_sd = 5.2
min_cv <- min_sd/min_mu
max_cv <- max_sd/max_mu

B[,1] <- qtruncnorm(
  A[, 1],
  mean = median(min_mu:max_mu),
  sd = median(min_sd:max_sd),
  a = min_mu,
  b = max_mu
) # mean distribution

B[,2]<- qtruncnorm(
  A[, 1],
  mean = median(min_cv:max_cv),
  sd = median(min_sd:max_sd),
  a = min_cv,
  b = max_cv
) #cv distribution

B

min_mu = 3.95
min_sd = 1.51
max_mu = 5.57
max_sd = 2.23


B[,3] <- qtruncnorm(
  A[, 1],
  mean = median(min_mu:max_mu),
  sd = median(min_sd:max_sd),
  a = min_mu,
  b = max_mu
) # mean distribution

B[,4]<- qtruncnorm(
  A[, 1],
  mean = median(min_cv:max_cv),
  sd = median(min_sd:max_sd),
  a = min_cv,
  b = max_cv
) #cv distribution



#What do I do from there?
B

source("scripts/make_dist.R")
incub = list()
gt = list()

for (i in 1:nrow(B)) {
  
  incub[[i]] <- make_dist("gamma",
            mu =B[i,][1],
            sd = B[i,][2])
  
  gt[[i]] <- make_dist("gamma",
            mu =B[i,][3],
            sd = B[i,][4])
}

##then use list functions inside outbreaker_parallel.R??
