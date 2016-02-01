setwd("/users/cwillis/dev/uiucGSLIS/ecir-2016/analysis")
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

# Summaries

# Dakka et al, TREC 6-8 (301-450), LATimes
d <- read.csv("dakka-?latimes-trec678-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]          # Remove topic and file fields
d[is.na(d)] <- 0            # Replace NA with 0
d$Event <- d$PeriodicEvent + d$SpecificEvent + d$IndirectEventReference
d$Entity <- d$OrganizationEntity + d$OtherEntity + d$PersonEntity
d <- d[-c(1,2,3,4,6,8,9,10)]               # Remove ExplicitDate, Futuh
d[,c(1,2,6,7)][d[,c(1,2,6,7)] > 0] <- 1 # Replace non-zero with 1

#d[,1:10] <- lapply(d[,1:10], factor)
#d$Temporal <- as.factor(d$Temporal)

# Logistic model without ACF/DPS, stepwise selection, 10-fold cross-validation
m <- glm(Temporal ~ . - ACF - DPS, d, family=binomial(link=logit))
summary(m)
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta

# Logistic model with ACF/DPS, stepwise selection, 10-fold cross-validation
m <- glm(Temporal ~ ., d, family="binomial")
summary(m)
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
norm

# Dakka et al, TREC 6-8 (301-450), Financial Times
d <- read.csv("dakka-ft-trec678-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
d <- d[-c(6)] 

# Logistic model without ACF/DPS, stepwise selection, 10-fold cross-validation
m <- glm(Temporal ~ . - ACF - DPS, d, family="binomial")
summary(m)
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta

# Logistic model with ACF/DPS, stepwise selection, 10-fold cross-validation
m <- glm(Temporal ~ DPS, d, family=binomial(link=logit))
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta

m <- glm(Temporal ~ DPS, d, family=binomial(link=logit))
summary(m)

m <- logistf(Temporal ~ SpecificEvent, d, family=binomial(link=logit))
summary(m)



# Efron & Golovchinsky, TREC 6-8 (301-450), Financial Times
d <- read.csv("efron-ft-trec678-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1

m <- glm(Temporal ~ . - DPS - ACF, d, family=binomial)
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta

m <- glm(Temporal ~ ., d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta

# Efron & Golovchinsky, TREC 6-8 (301-450), LA Times
d <- read.csv("efron-latimes-trec678-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1

m <- glm(Temporal ~ . - DPS - ACF, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta

m <- glm(Temporal ~ ., d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta


# Peetz et al, Blog06-08 (900-1050), Blog 06
d <- read.csv("peetz-blog0608-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1

m <- glm(Temporal ~ . - ACF - DPS, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta

m <- glm(Temporal ~ ., d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta


# Novelty 03-04.  Predict whether the topic is an "Event" (1) or "Opinion" (0)
d <- read.csv("novelty-0304-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1

m <- glm(Temporal ~ . - ACF - DPS, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta

m <- glm(Temporal ~ ., d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta

