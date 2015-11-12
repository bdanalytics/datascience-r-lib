require(vcd)

ymy <- glb_entity_df$bucket2008
# generate two processes for test
set.seed(2014); ypois <- rpois(200, 5)
set.seed(2014); ynorm <- rnorm(100, 5, 0.3) # goodfit asks for non-negative values
set.seed(2014); ybinom <- rbinom(100, size=5, prob=0.8)

# poisson check
print(gfmy <- goodfit(ymy - 1, type= "poisson", method= "ML"))
plot(gfmy ,main="Count data vs Poisson distribution")
summary(gfmy)

print(gfpois <- goodfit(ypois, type= "poisson", method= "ML"))
plot(gfpois ,main="Count data vs Poisson distribution")
summary(gfpois)

print(gfnorm <- goodfit(ynorm, type= "poisson", method= "ML"))
plot(gfnorm ,main="Count data vs Poisson distribution")
summary(gfnorm)

# binomial check
print(gfmy <- goodfit(ymy - 1, type= "binomial", method= "ML"))
plot(gfmy ,main="Count data vs Binomial distribution")
summary(gfmy)
qqplot(ybinom, ymy)

print(gfpois <- goodfit(ypois, type= "poisson", method= "ML"))
plot(gfpois ,main="Count data vs Poisson distribution")
summary(gfpois)

print(gfnorm <- goodfit(ynorm, type= "poisson", method= "ML"))
plot(gfnorm ,main="Count data vs Poisson distribution")
summary(gfnorm)

# # to automatically get the pvalue
# gf.summary = capture.output(summary(gf))[[5]]
# pvalue = unlist(strsplit(gf.summary, split = " "))
# pvalue = as.numeric(pvalue[length(pvalue)]); pvalue
#
# # to mannualy compute the pvalue
# chisq = sum(  (gf$observed-gf$fitted)^2/gf$fitted )
#
# df = length(gf$observed)-1-1
# pvalue = pchisq(chisq,df)
# pvalue