
######## Preparations for data analyses  ##########
require("arm")
radon.MN <- read.table("radon.MN.txt", header=T)
attach(radon.MN)
n <- length(log.radon)
y <- log.radon
x <- floor
u.full <- uranium

# get county index variable
county.name <- as.vector(county.name)
uniq <- unique(county.name)
J <- length(uniq)
county <- rep (NA, J)
for (i in 1:J){
  county[county.name==uniq[i]] <- i
}
######## Preparation finished  ##########

  
######## Q1  ##########
Q1 <- lm(y~1+x)
display(Q1)

######## Q2-3  ##########
sample.size <- as.vector (table (county))

a.no.pool <- b.no.pool <- SE.a.no.pool <- SE.b.no.pool <- rep(NA, 6) 

pick6 <- c(3,9,54,61,19,70)
for (k in 1:6){
  a <- which(radon.MN[,"county"]== pick6[k])
  data <- radon.MN[a,]
  fit <- lm(log.radon ~ floor, data=data)
  a.no.pool[k] <- fit$coefficients[1]
  b.no.pool[k] <- fit$coefficients[2]
  SE.a.no.pool[k] <- summary(fit)$coef[1,2]
  SE.b.no.pool[k] <- summary(fit)$coef[2,2]
}

Q3 <- cbind(sample.size[pick6], a.no.pool, SE.a.no.pool, b.no.pool, SE.b.no.pool)

######## Q4  ##########
M <- lmer(y~1+x+(1+x|county))
display(M)
a.MLM <- coef(M)$county[pick6,1]
b.MLM <- coef(M)$county[pick6,2]
se.a.MLM <- se.coef(M)$county[pick6, 1]
se.b.MLM <- se.coef(M)$county[pick6, 2]

Q4 <- cbind(sample.size[pick6],a.MLM, se.a.MLM, b.MLM, se.b.MLM) 

######## Q5  ##########
M4a <- lmer(y~1+x+u.full + (1+x|county))
M4b <- lmer(y~1+x+u.full:x+ (1+x|county))

display(M4a)
display(M4b)

######## Q6  ##########
# First we get some ideas about the uranium value across the counties
U <- rep(NA, J)

for(j in 1:J){
  pick <- which(county==j)
  U[j] <- mean(u.full[pick])
  }

# U contains 85 uranium values for the 85 counties
# u.full contains 919 uranium values for the 919 houses
summary(U) # mean is about 0; median is about 0.15
hist(U) # mode is about 0.3

# fit the models in Q6
M4 <- lmer(y~1+x+u.full:x+u.full+(1+x|county))
display(M4)

x.c1 <- x - 0.5
M4c <- lmer(y~1+x.c1 + u.full:x.c1 + u.full + (1+x.c1|county))
display(M4c)

x.c2 <- x - 1
M4d <- lmer(y~1+x.c2 + u.full:x.c2 + u.full + (1+x.c2|county))
display(M4d)

u.full.c1 <- u.full - 0.3
M4e <- lmer(y~1+x + u.full.c1:x + u.full.c1 + (1+x|county))
display(M4e)

u.full.c2 <- u.full - 0.15
M4f <- lmer(y~1+x + u.full.c2:x + u.full.c2 + (1+x|county))
display(M4f)


M4g <- lmer(y~1+x.c1 + u.full.c1:x.c1 + u.full.c1 + (1+x.c1|county))
display(M4g)

M4h <- lmer(y~1+x.c1 + u.full.c2:x.c1 + u.full.c2 + (1+x.c1|county))
display(M4h)
