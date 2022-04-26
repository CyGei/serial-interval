## this is my 1st parameter
## following a normal distribution
library(tidyverse)
set.seed(123)
x<- seq(0,8, by = 0.1)
n <- seq(0,1, 0.1)
mean = 4
sd = 1

d<- dnorm(x, mean = mean, sd = sd)
p <- pnorm(x, mean = mean, sd = sd)
q <-  qnorm(n, mean = mean, sd = mean)

df <- data.frame(x, d, p)
strata <- data.frame(n, q)

fig <- df %>% 
  ggplot(aes(x = x )) +
  geom_line(aes(y = d, col = "dnorm"))+
  geom_line(aes(y = p, col = "pnorm"))+
  scale_y_continuous(sec.axis = sec_axis(~., name = "p"))+
  scale_colour_manual(values = c("blue", "red"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

fig  

fig <- fig +
  geom_hline(data = strata,
             aes(yintercept = n), linetype = "dotted", col = "red")+
  geom_vline(data = strata,
             aes(xintercept = q), linetype = "dotted", col = "blue")+
  scale_x_continuous(n.breaks = 10)
fig
#dotted lines are now split in equal probabilities -- OK
strata
df


#from qnorm we know have 10 intervals of equal probabilities we want
# to sample from uniformly

strata<- strata %>% mutate(lower_q = q,
                     upper_q = lead(q))
strata 
r<- vector()
for (i in 1:nrow(strata)) {
  r[i] <- runif(1, min = strata$lower_q[i], strata$upper_q[i])
}

r #this is the parameter 1 sample point #check out the NAs

##### REPEAT procedure for the other distribution?





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


# plot function from LHS package ------------------------------------------

graph2dLHSTransform <- function(Alhs, transform1, transform2, min1, max1, min2, max2)
{
  stopifnot(ncol(Alhs) == 2)
  stopifnot(all(Alhs[,1] <= max1 & Alhs[,1] >= min1))
  stopifnot(all(Alhs[,2] <= max2 & Alhs[,2] >= min2))
  sims <- nrow(Alhs)
  breaks <- seq(0,1,length = sims + 1)[2:(sims)]
  breaksTransformed1 <- sapply(breaks, transform1)
  breaksTransformed2 <- sapply(breaks, transform2)
  par(mar = c(4,4,2,2))
  plot.default(Alhs[,1], Alhs[,2], type = "n", 
               ylim = c(min2, max2),
               xlim = c(min1, max1),
               xlab = "Parameter 1", ylab = "Parameter 2", 
               xaxs = "i", yaxs = "i", main = "")
  for (si in 1:sims)
  {
    temp <- Alhs[si,]
    for (i in 1:sims)
    {
      if ((i == 1 && min1 <= temp[1] && breaksTransformed1[i] >= temp[1]) ||
          (i == sims && max1 >= temp[1] && breaksTransformed1[i - 1] <= temp[1]) ||
          (breaksTransformed1[i - 1] <= temp[1] && breaksTransformed1[i] >= temp[1]))
      {
        for (j in 1:sims)
        {
          if ((j == 1 && min2 <= temp[2] && breaksTransformed2[j] >= temp[2]) ||
              (j == sims && max2 >= temp[2] && breaksTransformed2[j - 1] <= temp[2]) ||
              (breaksTransformed2[j - 1] <= temp[2] && breaksTransformed2[j] >= temp[2]))
          {
            if (i == 1)
            {
              xbot <- min1
              xtop <- breaksTransformed1[i]
            } else if (i == sims)
            {
              xbot <- breaksTransformed1[i - 1]
              xtop <- max1
            } else 
            {
              xbot <- breaksTransformed1[i - 1]
              xtop <- breaksTransformed1[i]
            }
            if (j == 1)
            {
              ybot <- min2
              ytop <- breaksTransformed2[j]
            } else if (j == sims)
            {
              ybot <- breaksTransformed2[j - 1]
              ytop <- max2
            } else 
            {
              ybot <- breaksTransformed2[j - 1]
              ytop <- breaksTransformed2[j]
            }
            rect(xbot, ybot, xtop, ytop, col = "grey")
          }
          
        }
      }
    }
  }
  points(Alhs[,1], Alhs[,2], pch = 19, col = "red")
  abline(v = breaksTransformed1, h = breaksTransformed2)
}

