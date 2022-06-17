library(mediation)
library(causaloptim)
library(ggplot2)
library(patchwork)

data(framing)
names(framing)

boundsdata<-as.data.frame(framing$treat)
names(boundsdata)<-"trt"

boundsdata$outcome<-(framing$immigr>=3)*1
boundsdata$M1<-(framing$emo>=8)*1
boundsdata$M2<-(framing$p_harm>=6)*1

covariates <- c("age", "educ", "gender", "income")
boundsdata <- cbind(boundsdata, framing[, covariates])

set.seed(211208)
## figure 1a
## need components of the form
## E(Y(X, M1(x1), M2 = m2))

fig1ests <- function(boundsdata) {

mod.y.all <- glm(outcome ~ trt * M1 + trt * M2 + M1 * M2 + trt * age +
                   trt * educ + trt * gender + trt * income +
                   M1 * age + M1 * educ + M1 * gender + M1 * income +
                   M2 * age + M2 * educ + M2 * gender + M2 * income +
                   age * educ + age * gender + age * income +
                   educ * gender + educ * income + gender * income,
                 family = "binomial", data = boundsdata)

mod.m1.cx <- glm(M1 ~ trt * age + trt * educ + trt * gender + trt * income +
                   age * educ + age * gender + age * income +
                   educ * gender + educ * income + gender * income,
                 family = "binomial", data = boundsdata)


mod.m2.all <- glm(M2 ~ trt * M1 + trt * age +
                   trt * educ + trt * gender + trt * income +
                   M1 * age + M1 * educ + M1 * gender + M1 * income +
                   age * educ + age * gender + age * income +
                   educ * gender + educ * income + gender * income,
                 family = "binomial", data = boundsdata)

ecn1cd2e <- function(x, x1, m2) {

  cframe <- boundsdata[, covariates]
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
  ecn1cd2e(1, 1, 1) - ecn1cd2e(0, 1, 1))

}

ests <- fig1ests(boundsdata)
boots <- do.call(rbind, lapply(1:1000, function(i) {

  bs <- sample(1:nrow(boundsdata), nrow(boundsdata), replace = TRUE)
  bdbs <- boundsdata[bs, ]
  fig1ests(bdbs)

}))


po <- function(mod, newdata) {

  predict(mod, newdata = newdata, type = "response")
}

fig2ests <- function(boundsdata) {

  mod.y.all <- glm(outcome ~ trt * M1 + trt * M2 + M1 * M2 + trt * age +
                     trt * educ + trt * gender + trt * income +
                     M1 * age + M1 * educ + M1 * gender + M1 * income +
                     M2 * age + M2 * educ + M2 * gender + M2 * income +
                     age * educ + age * gender + age * income +
                     educ * gender + educ * income + gender * income,
                   family = "binomial", data = boundsdata)

  mod.m1.cx <- glm(M1 ~ trt * age + trt * educ + trt * gender + trt * income +
                     age * educ + age * gender + age * income +
                     educ * gender + educ * income + gender * income,
                   family = "binomial", data = boundsdata)


  mod.m2.all <- glm(M2 ~ trt * M1 + trt * age +
                      trt * educ + trt * gender + trt * income +
                      M1 * age + M1 * educ + M1 * gender + M1 * income +
                      age * educ + age * gender + age * income +
                      educ * gender + educ * income + gender * income,
                    family = "binomial", data = boundsdata)

  ecn2cd11e <- function(m1, x2, m1_) {

    cframe <- boundsdata[, covariates]
    cfm0 <- cbind(cframe, M1 = m1_, trt = x2)
    cfm1 <- cbind(cframe, M1 = m1_, trt = x2)

    yfm01 <- cbind(cframe, M1 = m1, M2 = 0, trt = 1)
    yfm11 <- cbind(cframe, M1 = m1, M2 = 1, trt = 1)

    yfm00 <- cbind(cframe, M1 = m1, M2 = 0, trt = 0)
    yfm10 <- cbind(cframe, M1 = m1, M2 = 1, trt = 0)

    mean((1 - predict(mod.m2.all, newdata = cfm0, type = "response")) *
           predict(mod.y.all, newdata = yfm01, type = "response") +
           predict(mod.m2.all, newdata = cfm1, type = "response") *
           predict(mod.y.all, newdata = yfm11, type = "response")) -
      mean((1 - predict(mod.m2.all, newdata = cfm0, type = "response")) *
             predict(mod.y.all, newdata = yfm00, type = "response") +
             predict(mod.m2.all, newdata = cfm1, type = "response") *
             predict(mod.y.all, newdata = yfm10, type = "response"))

  }

  ecn21cd1e <- function(m1, x2, x3) {

    cframe <- boundsdata[, covariates]
    cfm <- cbind(cframe, trt = x3)

    mean(((1 - po(mod.m1.cx, cfm)) * (1 - po(mod.m2.all,
                                            cbind(cframe, M1 = 0, trt = x2)) *
                                       po(mod.y.all, cbind(cframe, M1 = m1, trt = 1,
                                                           M2 = 0)))) +
           ((po(mod.m1.cx, cfm)) * (1 - po(mod.m2.all,
                                               cbind(cframe, M1 = 1, trt = x2)) *
                                          po(mod.y.all, cbind(cframe, M1 = m1, trt = 1,
                                                              M2 = 0)))) +
           ((1 - po(mod.m1.cx, cfm)) * (po(mod.m2.all,
                                               cbind(cframe, M1 = 0, trt = x2)) *
                                          po(mod.y.all, cbind(cframe, M1 = m1, trt = 1,
                                                              M2 = 1)))) +
           ((po(mod.m1.cx, cfm)) * (po(mod.m2.all,
                                               cbind(cframe, M1 = 1, trt = x2)) *
                                          po(mod.y.all, cbind(cframe, M1 = m1, trt = 1,
                                                              M2 = 1))))) -
      mean(((1 - po(mod.m1.cx, cfm)) * (1 - po(mod.m2.all,
                                               cbind(cframe, M1 = 0, trt = x2)) *
                                          po(mod.y.all, cbind(cframe, M1 = m1, trt = 0,
                                                              M2 = 0)))) +
             ((po(mod.m1.cx, cfm)) * (1 - po(mod.m2.all,
                                             cbind(cframe, M1 = 1, trt = x2)) *
                                        po(mod.y.all, cbind(cframe, M1 = m1, trt = 0,
                                                            M2 = 0)))) +
             ((1 - po(mod.m1.cx, cfm)) * (po(mod.m2.all,
                                             cbind(cframe, M1 = 0, trt = x2)) *
                                            po(mod.y.all, cbind(cframe, M1 = m1, trt = 0,
                                                                M2 = 1)))) +
             ((po(mod.m1.cx, cfm)) * (po(mod.m2.all,
                                         cbind(cframe, M1 = 1, trt = x2)) *
                                        po(mod.y.all, cbind(cframe, M1 = m1, trt = 0,
                                                            M2 = 1)))))

  }

  ecn12cd1e <- function(x1, x2, m1_) {

    cframe <- boundsdata[, covariates]
    cfm <- cbind(cframe, trt = x1)

    mean(((1 - po(mod.m1.cx, cfm)) * (1 - po(mod.m2.all,
                                             cbind(cframe, M1 = m1_, trt = x2)) *
                                        po(mod.y.all, cbind(cframe, M1 = 0, trt = 1,
                                                            M2 = 0)))) +
           ((po(mod.m1.cx, cfm)) * (1 - po(mod.m2.all,
                                           cbind(cframe, M1 = m1_, trt = x2)) *
                                      po(mod.y.all, cbind(cframe, M1 = 1, trt = 1,
                                                          M2 = 0)))) +
           ((1 - po(mod.m1.cx, cfm)) * (po(mod.m2.all,
                                           cbind(cframe, M1 = m1_, trt = x2)) *
                                          po(mod.y.all, cbind(cframe, M1 = 0, trt = 1,
                                                              M2 = 1)))) +
           ((po(mod.m1.cx, cfm)) * (po(mod.m2.all,
                                       cbind(cframe, M1 = m1_, trt = x2)) *
                                      po(mod.y.all, cbind(cframe, M1 = 1, trt = 1,
                                                          M2 = 1))))) -
      mean(((1 - po(mod.m1.cx, cfm)) * (1 - po(mod.m2.all,
                                               cbind(cframe, M1 = m1_, trt = x2)) *
                                          po(mod.y.all, cbind(cframe, M1 = 0, trt = 0,
                                                              M2 = 0)))) +
             ((po(mod.m1.cx, cfm)) * (1 - po(mod.m2.all,
                                             cbind(cframe, M1 = m1_, trt = x2)) *
                                        po(mod.y.all, cbind(cframe, M1 = 1, trt = 0,
                                                            M2 = 0)))) +
             ((1 - po(mod.m1.cx, cfm)) * (po(mod.m2.all,
                                             cbind(cframe, M1 = m1_, trt = x2)) *
                                            po(mod.y.all, cbind(cframe, M1 = 0, trt = 0,
                                                                M2 = 1)))) +
             ((po(mod.m1.cx, cfm)) * (po(mod.m2.all,
                                         cbind(cframe, M1 = m1_, trt = x2)) *
                                        po(mod.y.all, cbind(cframe, M1 = 1, trt = 0,
                                                            M2 = 1)))))

  }

  res <- NULL
  nmes <- NULL
  for(i in 0:1) {
    for(j in 0:1) {
      for(k in 0:1) {
        nmes <- c(nmes, sprintf(c("cn2cd11e.%s%s%s", "cn21cd1e.%s%s%s", "cn12cd1e.%s%s%s"),
                                i, j, k))
        res <- c(res, ecn2cd11e(i,j,k),
                 ecn21cd1e(i,j,k),
                 ecn12cd1e(i,j,k))

      }
    }
  }
  names(res) <- nmes
  res

}



ests2 <- fig2ests(boundsdata)
boots2 <- do.call(rbind, lapply(1:1000, function(i) {

  bs <- sample(1:nrow(boundsdata), nrow(boundsdata), replace = TRUE)
  bdbs <- boundsdata[bs, ]
  fig2ests(bdbs)

}))


point.estimates <- data.frame(estimate = c(ests, ests2),
lower.limit = c(apply(boots, 2, quantile, .025), apply(boots2, 2, quantile, .025)),
upper.limit = c(apply(boots, 2, quantile, .975), apply(boots2, 2, quantile, .975)),
estimand = c("cn1cd2e.00", "cn1cd2e.01", "cn1cd2e.10", "cn1cd2e.11",
             names(ests2)))


saveRDS(point.estimates, file = "point-ests.rds")


# covid example

coviddata <- read.csv("dataoutcovidliger.csv")
boundsdata <- with(coviddata, data.frame(trt = Z, outcome = Infection,
                                         M1 = cinq, M2 = binq))


set.seed(211208)
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

boots <- do.call(rbind, lapply(1:1000, function(i) {

  bs <- sample(1:nrow(boundsdata), nrow(boundsdata), replace = TRUE)
  bdbs <- boundsdata[bs, ]
  fig1ests(bdbs)

}))

boots.swap <- do.call(rbind, lapply(1:1000, function(i) {

  bs <- sample(1:nrow(boundsdataswap), nrow(boundsdataswap), replace = TRUE)
  bdbs <- boundsdataswap[bs, ]
  fig1ests(bdbs)

}))


point.estimates <- data.frame(estimate = c(ests, ests.swap),
                              lower.limit = c(apply(boots, 2, quantile, .025),
                                              apply(boots.swap, 2, quantile, .025)),
                              upper.limit = c(apply(boots, 2, quantile, .975),
                                              apply(boots.swap, 2, quantile, .975)),
                              estimand = c("cn1cd2e.00", "cn1cd2e.01", "cn1cd2e.10", "cn1cd2e.11",
                                             "cn1cd2e.00", "cn1cd2e.01", "cn1cd2e.10", "cn1cd2e.11",
                                             "cn2cd1e.00", "cn2cd1e.01", "cn2cd1e.10", "cn2cd1e.11",
                                             "cn2cd1e.00", "cn2cd1e.01", "cn2cd1e.10", "cn2cd1e.11"),
                              type = rep(rep(c("diff", "ratio"), each = 4), 2))


saveRDS(point.estimates, file = "point-ests-covid.rds")


