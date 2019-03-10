
# create some data ----
set.seed(2112)

# create 3 levels, 10 each
flevels <- factor(rep(c("A","B","C"),c(10,10,10))) 

# create some "nice" data,
# sorted so means at each level have good separation
vals <- sort(round(runif(30,3,15))) 

# calculate mean of each level for reference
means <- tapply(vals,flevels,mean) 
means

# treatment contrasts ---- 

# verify unordered contrast setting is Treatment contrast 
options("contrasts")
contrasts(flevels) <- "contr.treatment"
contrasts(flevels)

# linear model
m_trt <- lm(vals ~ flevels)
summary(m_trt)

# intercept = mean A
coef(m_trt)[1]
means[1]

# flevelsB = mean of B - mean of A
coef(m_trt)[2]
means[2] - means[1]

# flevelsC = mean of C - mean of A
coef(m_trt)[3]
means[3] - means[1]

# plug in row values into model 
means

# mean of A --> row 1: 0 0
coef(m_trt)[1] + 0*coef(m_trt)[2] + 0*coef(m_trt)[3]

# mean of B --> row 2: 0 0
coef(m_trt)[1] + 1*coef(m_trt)[2] + 0*coef(m_trt)[3]

# mean of C --> row 3: 0 0
coef(m_trt)[1] + 0*coef(m_trt)[2] + 1*coef(m_trt)[3]

# helmert contrasts ----
contrasts(flevels) <- "contr.helmert"
contrasts(flevels)

# linear model with helmert contrasts
m_helm <- lm(vals ~ flevels)
summary(m_helm)

# intercept - mean of means
coef(m_helm)[1]
mean(means)

# flevels1 = mean of first two levels minus first level
coef(m_helm)[2]
mean(means[1:2]) - means[1]

# flevels2 = mean of all three levels - mean of first two levels
coef(m_helm)[3]
mean(means) - mean(means[1:2])

# verify
means

# mean of A --> row 1: -1 -1
coef(m_helm)[1] + -1*coef(m_helm)[2] + -1*coef(m_helm)[3]

# mean of B --> row 2: 1 -1
coef(m_helm)[1] + 1*coef(m_helm)[2] + -1*coef(m_helm)[3]

# mean of C --> row 3: 0 2
coef(m_helm)[1] + 0*coef(m_helm)[2] + 2*coef(m_helm)[3]

# sum contrasts ----
contrasts(flevels) <- "contr.sum"
contrasts(flevels)

# linear model with sum contrasts
m_sum <- lm(vals ~ flevels)
summary(m_sum)

# intercept = mean of all means
coef(m_sum)[1]
mean(means)

# flevels1 = mean of all means - mean of level 1 (here A)
coef(m_sum)[2]
means[1] - mean(means)

# flevels 2 = mean of all means - mean of level 2 (here B)
coef(m_sum)[3]
means[2] - mean(means)

# verify
means

# mean of A -->row 1: 1 0
coef(m_sum)[1] + 1*coef(m_sum)[2] + 0*coef(m_sum)[3]

# mean of B -->row 2: 0 1
coef(m_sum)[1] + 0*coef(m_sum)[2] + 1*coef(m_sum)[3]

# mean of C -->row 3: -1 -1
coef(m_sum)[1] + -1*coef(m_sum)[2] + -1*coef(m_sum)[3]

# return to system default ----
contrasts(flevels) <- NULL
