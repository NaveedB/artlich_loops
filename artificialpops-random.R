##### site set up for detection probabilities #####
##### with artificial lichen populations
### date created: 2017_08_10
### created by: Naveed Bhatti
### last modified: Fri Aug 25 10:33:36 2017
### last modified by: Naveed Bhatti
timestamp(stamp = date)
getwd()
setwd("M:/PhD/R/scripts/artificial_popns")

##### artificial lichens set up #####
## random binomial generation for occupied tree

setupFUN <- function(N, d)
{
# N = total number of trees
# d = density of population (e.g. 0.1 = rare, 0.6 = common)
# n = d * N = number of occupied trees
  
### 1: random series ####
  
  y <- vector(length = N)
# set number of occupied trees required (d * N)  
  while (sum(y) != d * N)
  {
    for (i in 1:N)
    {
      y[i] <- rbinom(n = 1, size = 1, prob = d)
    }
  }
# create data frame of occupied trees  
  ser <- data.frame(tree_no = 1:N, occ = y)
  
### 2: lichen size & position on bole ####
  
# 2.i select lichen size (small/large):
# create vector of length n for occupied trees from which to sample
  n <- d * N
# use 1:1 ratio of small:large
  sz1 <- rep(c("small", "large"), times = n/2)
# add random sample of small/large for odd length vector
  sz2 <- sample(c("small", "large"), size = 1)
# randomly select lichen size
  sz <- sample(c(sz1, sz2), n, replace = FALSE)
  
# 2.ii select aspect (measured to nearest 10?):
  asp <- round(x = sample(0:359, size = n), digits = -1)

# 2.iii select height bin (low/mid/high):
# use 1:1:1 ratio for bins
  htbin1 <- rep(c("low", "mid", "high"), times = n/3)
# add random sample for vector length not divisible by 3
  htbin2 <- sample(c("low", "mid", "high"), size = n - length(htbin1))
# randomly select height bins
  htbin <- sample(c(htbin1, htbin2), n, replace = FALSE)

# 2.iv select actual height 30-175cm (measured to nearest 5cm):
  h1 <- 30
  h4 <- 175
  hts <- seq(from = h1, to = h4, by = 5)
# split into three equal sections
  sect <- (h4 - h1) / 3
  h2 <- h1 + sect
  h3 <- h4 - sect
  low <- hts[hts < h2]
  mid <- hts[hts > h2 & hts < h3]
  high <- hts[hts > h3]
# sample actual height bin
  ht1 <- vector(length = n)
  for (i in 1:n)
  {
    if (htbin[i] == "low") {ht1[i] <- sample(low, 1)}
    else if (htbin[i] == "mid") {ht1[i] <- sample(mid, 1)}
    else if (htbin[i] == "high") {ht1[i] <- sample(high, 1)}
  }
 
# 2.v combine size & position data
  lic <- data.frame(size = sz, ht_bin = htbin, ht_cm = ht1, asp_? = asp)
  
### 3: join lichen size & position to occupied tree series ####

# occupied trees:
  serOcc <- data.frame(tree_no = ser$tree_no[ser$occ == 1])
# lichen positions:
  setup1 <- data.frame(serOcc, lic)
# join data frames:
  setup <- merge(ser, setup1, by = "tree_no", all = TRUE)
# print results
  print(setup)
  
}

# example run:
# for 30 trees
# 60% abundance of occupied trees
rndSetup <- setupFUN(30, 0.6)

# save results:
write.table(rndSetup, "M:/PhD/R/scripts/artificial_popns/rndSetup.txt", 
    col.names = TRUE, row.names = FALSE, sep = "\t")

#####

ls()
rm(list = ls())
