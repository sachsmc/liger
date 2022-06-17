
# covid example

set.seed(64)
n <- 1e6
Z<-rbinom(n,1, 0.5)

C<-rnorm(n,0.5+Z*1, 1)
Q<-exp(rnorm(n, 6.5+.5*Z, .3))

Yz1<-rbinom(n,1, exp((0.8690378/-900)*Q +  -0.3*C -2)*0.1)
Yz0<-rbinom(n,1, 0.1)

binq<-ifelse(Q>=1000,1,0)
cinq<-ifelse(C>=1,1,0)
Infection<-ifelse(Z==1,Yz1, Yz0)

coviddata<-as.data.frame(cbind(Z,cinq,binq,Infection))

boundsdata <- with(coviddata, data.frame(trt = Z, outcome = Infection,
                                         M1 = cinq, M2 = binq))


## figure 1a
## need components of the form
## E(Y(X, M1(x1), M2 = m2))

fig1ests <- function(boundsdata) {

  mod.y.all <- glm(outcome ~ trt * M1 + trt * M2,
                   family = "binomial", data = boundsdata)

  mod.m1.cx <- glm(M1 ~ trt,
                   family = "binomial", data = boundsdata)


  mod.m2.all <- glm(M2 ~ trt,
                    family = "binomial", data = boundsdata)

  ecn1cd2e <- function(x, x1, m2) {

    cframe <- boundsdata[, "outcome", drop = FALSE]
    cfm0 <- cbind(cframe, M1 = 0, M2 = m2, trt = x1)
    cfm1 <- cbind(cframe, M1 = 1, M2 = m2, trt = x1)
    yfm0 <- cbind(cframe, M1 = 0, M2 = m2, trt = x)
    yfm1 <- cbind(cframe, M1 = 1, M2 = m2, trt = x)

    mean((1 - predict(mod.m1.cx, newdata = cfm0, type = "response")) *
           predict(mod.y.all, newdata = yfm0, type = "response") +
           predict(mod.m1.cx, newdata = cfm1, type = "response") *
           predict(mod.y.all, newdata = yfm1, type = "response"))

  }

  c(ecn1cd2e(1, 0, 0) - ecn1cd2e(0, 0, 0),
    ecn1cd2e(1, 0, 1) - ecn1cd2e(0, 0, 1),
    ecn1cd2e(1, 1, 0) - ecn1cd2e(0, 1, 0),
    ecn1cd2e(1, 1, 1) - ecn1cd2e(0, 1, 1),
    ecn1cd2e(1, 0, 0) / ecn1cd2e(0, 0, 0),
    ecn1cd2e(1, 0, 1) / ecn1cd2e(0, 0, 1),
    ecn1cd2e(1, 1, 0) / ecn1cd2e(0, 1, 0),
    ecn1cd2e(1, 1, 1) / ecn1cd2e(0, 1, 1))

}

ests <- fig1ests(boundsdata)

boundsdataswap <- boundsdata
boundsdataswap$M1 <- boundsdata$M2
boundsdataswap$M2 <- boundsdata$M1

ests.swap <- fig1ests(boundsdataswap)


point.estimates <- data.frame(estimate = c(ests, ests.swap),
                              estimand = c("cn1cd2e.00", "cn1cd2e.01", "cn1cd2e.10", "cn1cd2e.11",
                                           "cn1cd2e.00", "cn1cd2e.01", "cn1cd2e.10", "cn1cd2e.11",
                                           "cn2cd1e.00", "cn2cd1e.01", "cn2cd1e.10", "cn2cd1e.11",
                                           "cn2cd1e.00", "cn2cd1e.01", "cn2cd1e.10", "cn2cd1e.11"),
                              type = rep(rep(c("diff", "ratio"), each = 4), 2))


saveRDS(point.estimates, file = "true-values-covid.rds")


