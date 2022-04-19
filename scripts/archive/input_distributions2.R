
# INCUBATION --------------------------------------------------------------


incub_mus <- c(1.63 )
incub_sds <- c(0.5)


incub_list <- list()
for (i in 1:length(incub_mus)) {
  incub_list[[i]] <-
    distcrete::distcrete(
      "lnorm",
      meanlog = 1.63,
      sdlog = 0.5,
      interval = 1,
      w = 1
    )
  
}
data.frame(y = incub_list[[1]]$d(0:100), x = 0:100) %>% 
  ggplot(aes(x=x, y = y))+
  geom_col()


d <- distcrete::distcrete(
  "norm",
  mean = 6,
  sd = 1,
  interval = 1,
  w = 1
)

r<- d$r(0:10000)

hist(r, probability = TRUE)
p1 <- prop.table(table(r[r >= 1 ])) %>% 
  as.data.frame() %>%
  ggplot(aes(Var1, Freq))+
  geom_col()+
  ggtitle("Mean Incubation Period Explored")+
  xlab("Mean Incubation Period")
  
d <- distcrete::distcrete(
  "norm",
  mean = 2,
  sd = 2,
  interval = 1,
  w = 1
)

r<- d$r(0:10000)

p2 <- prop.table(table(r[r >= 1 & r <= 4])) %>% 
  as.data.frame() %>% 
  ggplot(aes(Var1, Freq))+
  geom_col()+
  ggtitle("SD Incubation Period Explored")+
  xlab("SD Incubation Period")


rnorm(10000, mean = 6, sd=1)
ggarrange(p1, p2, nrow = 2)
# GENERATION TIME ---------------------------------------------------------

gi_upper <- c(6.78, 4.91)
gi_lower <- c(3.78, 3.01)
gi_mus <- c(5.20, 3.95)
gi_n <- c(91, 135)
get_sd <- function(upper_ci, lower_ci, n){
  (upper_ci - lower_ci)*sqrt(n)/1.96
} 

gi_sds <- vector()
for( i in 1:length(gi_mus)){
  gi_sds[i] <- get_sd(gi_upper[i],
                 gi_lower[i],
                 gi_n[i])
}


gi_list <- list()
for( i in 1:length(gi_mus)){
  gi_list[[i]] <- VirusWatch::make_dist("gamma",
                                        gi_mus[i],
                                        gi_sds[i],
                                        w=1)
}


rm(gi_lower, gi_mus, gi_n, gi_sds, gi_upper, i, incub_mus, incub_sds)

cat(
  blue(
    "##################################################\n" %+%
      green$underline$bold("input_distributions2.R ran successfully\n") %+%
      "##################################################\n"
  )
)
