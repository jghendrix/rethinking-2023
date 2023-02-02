# Week 1 homework - JGH
# 1 Feb 2023
library(tidyverse)

# 1. ----
## Suppose the globe tossing data (Lecture 2, Chapter 2) had turned out to be 4 water and 11 land. Construct the posterior distribution.

# four water and eleven land = 15 tosses. still assuming the tetrahedral earth I suppose?
# 0 water and 4 water are impossible; either 1W3L, 2w2L, 3W1L

# 25% water world = 1 ^ 4 * 3 ^ 11
3^11
# 25% water = 177147

# 50-50 = 2 ^ 4 * 2 ^ 11 = 
2^15
# 50-50 world = 32768

# 75% water world = 3 ^ 4 * 1 ^ 11
# 75% water = 81

177147+32768+81
# all options = 209996

# 25% water
177147/209996 # 84.36%
32768/209996 # 15.60%
81/209996 # 0.03%

# 2. ----
## Using the posterior distribution from 1, compute the posterior predictive distribution for the next 5 tosses of the same globe. I recommend you use the sampling method.

# rather than using the 'categorical' posterior described above, we're now going to simulate this posterior distribution using the beta function distribution

n <- 10000 # define an arbitrary big number of intervals on our x-axis
sampled_post <- rbeta(n, 4 + 1, 11 + 1) # why the plus one? who knows, that's just what you do for beta distribution code apparently

# now we want to estimate how many of the next five tosses will be water (or land, but let's just do water for ease) given that distribution of post values

df <- as.data.frame(sampled_post)
df %<>% mutate(nW = sampled_post*5)
df %<>% mutate(n_W = round(nW, digits = 0))
# now we have a whole number estimate of the number of water we'd get for each value in that distribution of 10k p values

pred <- df %>% group_by(n_W) %>% summarise(n = n())
ggplot(pred, aes(x = n_W, y = n)) +
  geom_bar(stat = 'identity') +
  theme_bw()

## ^ This is WRONG. Just multiplying any given value of p by 5 tosses means you're only ever going to get the average outcome at each value of p... but you need to allow for the randomisation of the coin flip at each of the five tosses. In the slides it shows iterating this like 1000 times for each value of p, and then randomly selecting one of those outcomes to feed into your predictor posterior
## in the actual code, you use that probability to simulate five tosses to get a semi-randomized outcome, and then just take that first case (rather than iterating and then throwing out 99.9% of your data)


# from bella & alec's solutions, using sim_globe() is easiest I guess
post_samples <- rbeta(10000, 4+1, 11+1)

sim_globe <- function(p, N){
  sample(c("W", "L"), size = N, prob=c(p, 1-p), replace = T)
}

# now that we've defined the function, sapply it to our p-values with five tosses and then count how many are W
pred_post <- sapply(post_samples, function(p) sum(sim_globe(p, 5)=="W"))

# this creates a table counting the number of incidences of each option (i.e., how many times will 0 W appear, 1 W, 2 W, 3 W, 4 W, 5 W?)
tab_post <- table(pred_post)
pred_post_df <- as.data.frame(tab_post)

# I feel like I could do the above with summarise and group_by?
df <- as.data.frame(pred_post) %>%
  group_by(pred_post) %>%
  summarise(n = n())
# sure can!

ggplot(df, aes(pred_post, n)) + 
  geom_col() + 
  theme_classic() + 
  xlab("number of W") + 
  ylab("count")


# 3. ----
## Use the posterior predictive distribution from 2 to calculate the probability of 3 or more water samples in the next 5 tosses.

# sum of 3, 4, or 5 over all options
df %>% filter(pred_post >= 3) %>% summarise(three = sum(n))
sum(df$n)
1800/10000
# 18% chance of three or more


# 4. ----
## OPTIONAL. This problem is an optional challenge for people who are taking the course for a second or third time. Suppose you observe W = 5 water points, but you forgot to write down how many times the globe was tossed, so you don’t know the number of land points L. Assume that p = 0.7 and compute the posterior distribution of the number of tosses N. Hint: Use the binomial distribution.


# with a 70% chance of water, there had to be at least 5 tosses but could have been as many as we want (no upper tail)
# How do we work backwards to estimate how many tosses it took? Could we just do this one by one? That feels tedious but doable...

(0.70)^5 # 16.807%
(0.70)^5*(0.3)^1 # 5.0421%

# you're just adding an additional 30% for every additional N, so we if we keep doing this...
 tosses <- as.data.frame(seq(1:100)) %>%
   rename(N = 'seq(1:100)') %>%
   filter(N > 4) %>%
   mutate(p = (0.70)^5*(0.3)^(N - 5))
 
 ggplot(tosses, aes(N, p)) +
   geom_col() + 
   xlab("Number of tosses") +
   xlim(c(0, 15)) +
   theme_bw()
# this doesn't make sense, it shouldn't peak at 5... the highest should be at 5 / 0.7 = 7 tosses
 
# I'm forgetting the factoria1!
 # for a given number of tosses (e.g. 6), 70%^5 * 30%^1, but there are six different ways that could happen
# for any given total number of tosses, it's N! / (5! * (N-5)!)
 tosses %<>%
   mutate(ways = factorial(N)/(factorial(5)*factorial(N-5)),
          pf = p*ways)
 
 ggplot(tosses, aes(N, pf)) +
   geom_col() + 
   xlab("Number of tosses") +
   xlim(c(4, 20)) +
   theme_bw() 

 # ok we got it finally, just forgot our grade 11 probability class 
 
 