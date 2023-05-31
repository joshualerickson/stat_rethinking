### Chapters 1-3 coding

install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
devtools::install_github("rmcelreath/rethinking")


compute_posterior <- function(the_sample, poss=seq(0,1, .25)){

  W <- sum(the_sample=='W')
  L <- sum(the_sample=='L')
  ways <- sapply(poss, function(q) (q*4)^W*((1-q)*4)^L)
  post <- ways/sum(ways)
  #make_bar <- function(x) hist(x)$density
  #bars <- sapply(post, function(q) make_bar(q))
  data.frame(poss, ways, post=round(post, 3))


}

samp <- c(rep('W', 11), rep('L', 4))
post <- compute_posterior(samp, poss=seq(0,1, len = 11))

hist(post$ways)

b <- hist(1:50)

post <- compute_posterior(samp, seq(0,1, len = 11))$post

sa <- rep(sample(compute_posterior(samp, seq(0,1, len = 11))$post, size =  5, replace = T), 1000)

library(tidyverse)

curve(dbeta(x, 4+1, 11+1), 0, 1)

post_samp <- sample(post, size = 1e4, replace = T)

p_samples <- rbeta(1e4, 4+1, 11+1)

W_sim <- rbinom(1e4, 5, p = p_samples)

W_sim_post_samp <- rbinom(size = 1e4, 5,  p = post_samp)

hist(W_sim)
hist(W_sim_post_samp)
plot(table(W_sim))

mean(p_samples)*3

sum(W_sim >= 3)/sum(W_sim)

length(W_sim[W_sim >=3])/length(W_sim)

