library(causaloptim)
library(parallel)
b <- graph_from_literal(X -+ Y, Ul -+ X, X -+ M1, X -+ M2, M1 -+ Y, M2 -+ Y,
                        Ur -+ M1, Ur -+ M2, Ur -+ Y)
V(b)$leftside <- c(1, 0, 1, 0, 0, 0)
V(b)$latent <- c(0, 0, 1, 0, 0, 1)
V(b)$nvals <- c(2, 2, 2, 2, 2, 2)
E(b)$rlconnect <- rep(0, 9)
E(b)$edge.monotone <- rep(0, 9)


plot(b)

save_bounds <- function(obj, bnds, fname) {

  fn_tmp <- interpret_bounds(bnds$bounds, obj$parameters)
  saveRDS(fn_tmp, file.path("bounds-functions", paste0(fname, ".rds")))
  bnds$bounds <- gsub("p([0-1]{3})_([0-1])", "p_{\\1\\\\cdot \\2}", bnds$bounds)
  bnds$bounds <- gsub(",", "\\\\", bnds$bounds, fixed = TRUE)
  cat(bnds$bounds, file = file.path("bounds-latex", paste0(fname, ".txt")))

}



## query for results 1-4
cn1cde2 <- "p{Y(X = 1, M1(X = %s), M2 = %s) = 1} - p{Y(X = 0, M1(X = %s), M2 = %s) = 1}"

obj <- analyze_graph(b, constraints = NULL,
                     effectt = sprintf(cn1cde2, 0, 0, 0, 0))

bnds <- optimize_effect_2(obj)

save_bounds(obj, bnds, sprintf("cn1cde2.%s%s", 0, 0))

obj2 <- update_effect(obj, effectt = sprintf(cn1cde2, 0, 1, 0, 1))
bnds <- optimize_effect_2(obj2)
save_bounds(obj2, bnds, sprintf("cn1cde2.%s%s", 0, 1))


obj2 <- update_effect(obj, effectt = sprintf(cn1cde2, 1, 0, 1, 0))
bnds <- optimize_effect_2(obj2)
save_bounds(obj2, bnds, sprintf("cn1cde2.%s%s", 1, 0))


obj2 <- update_effect(obj, effectt = sprintf(cn1cde2, 1, 1, 1, 1))
bnds <- optimize_effect_2(obj2)
save_bounds(obj2, bnds, sprintf("cn1cde2.%s%s", 1, 1))

### figure b

b2 <- graph_from_literal(X -+ Y, Ul -+ X, X -+ M1, X -+ M2, M1 -+ Y, M2 -+ Y,
                        Ur -+ M1, Ur -+ M2, Ur -+ Y, M1 -+ M2)
V(b2)$leftside <- c(1, 0, 1, 0, 0, 0)
V(b2)$latent <- c(0, 0, 1, 0, 0, 1)
V(b2)$nvals <- c(2, 2, 2, 2, 2, 2)
E(b2)$rlconnect <- rep(0, 10)
E(b2)$edge.monotone <- rep(0, 10)

plot(b2)
## new query result 2
cn2cde1 <- "p{Y(X = 1, M1 = %s, M2(X = %s)) = 1} - p{Y(X = 0, M1 = %s, M2(X = %s)) = 1}"

cn2cde1a <- "p{Y(X = 0, M1 = %s, M2(X = %s)) = 1}"

obj <- analyze_graph(b2, constraints = NULL,
                     effectt = sprintf(cn2cde1a, 0, 0))


bnds <- optimize_effect_2(obj)
save_bounds(obj, bnds, fname = sprintf("cn2cde1.%s%s", 0,0))

obj2 <- update_effect(obj, effectt = sprintf(cn2cde1, 0, 1, 0, 1))
bnds <- optimize_effect_2(obj2)
save_bounds(obj, bnds, fname = sprintf("cn2cde1.%s%s", 0,1))


obj2 <- update_effect(obj, effectt = sprintf(cn2cde1, 1, 1, 1, 1))
bnds <- optimize_effect_2(obj2)
save_bounds(obj, bnds, fname = sprintf("cn2cde1.%s%s", 1,1))


obj2 <- update_effect(obj, effectt = sprintf(cn2cde1, 1, 0, 1, 0))
bnds <- optimize_effect_2(obj2)
save_bounds(obj, bnds, fname = sprintf("cn2cde1.%s%s", 1,0))


## check result 1 is the same

cn1cde2 <- "p{Y(X = 1, M1(X = %s), M2 = %s) = 1} - p{Y(X = 0, M1(X = %s), M2 = %s) = 1}"

obj <- analyze_graph(b2, constraints = NULL,
                     effectt = sprintf(cn1cde2, 0, 0, 0, 0))

bnds <- optimize_effect_2(obj)

## query for crossed effects
cn2cd11e <- "p{Y(X = 1, M1 = %s, M2(X = %s, M1 = %s)) = 1} - p{Y(X = 0, M1 = %s, M2(X = %s, M1 = %s)) = 1}"
cn21cd1e <- "p{Y(X = 1, M1 = %s, M2(X = %s, M1(X = %s))) = 1} - p{Y(X = 0, M1 = %s, M2(X = %s, M1(X = %s))) = 1}"
cn12cd1e <- "p{Y(X = 1, M1(X = %s), M2(X = %s, M1 = %s)) = 1} - p{Y(X = 0, M1(X = %s), M2(X = %s, M1 = %s)) = 1}"


obj <- analyze_graph(b2, constraints = NULL,
                     effectt = sprintf(cn2cd11e, 0, 0, 0, 0, 0, 0))

bnds <- optimize_effect_2(obj)
save_bounds(obj, bnds, fname = sprintf("cn2cd11e.%s%s%s", 0,0,0))


obj2 <- update_effect(obj, effectt = sprintf(cn21cd1e, 0, 0, 0, 0, 0, 0))
bnds <- optimize_effect_2(obj2)
save_bounds(obj2, bnds, sprintf("cn21cd1e.%s%s%s", 0, 0, 0))

obj2 <- update_effect(obj, effectt = sprintf(cn12cd1e, 0, 0, 0, 0, 0, 0))
bnds <- optimize_effect_2(obj2)
save_bounds(obj2, bnds, sprintf("cn12cd1e.%s%s%s", 0, 0, 0))

cl <- makeCluster(7)
bigtab <- expand.grid(m1 = 0:1, m2 = 0:1, m3 = 0:1)[-1, ]

init <- clusterEvalQ(cl, {
  library(causaloptim)
})
clusterExport(cl, c("save_bounds", "obj", "obj2", "bigtab", "cn12cd1e", "cn21cd1e", "cn2cd11e"))


btext_outer <- clusterApply(cl, 1:7, function(i) {

  m1 <- bigtab$m1[i]
  m2 <- bigtab$m2[i]
  m3 <- bigtab$m3[i]

  obj2 <- update_effect(obj, effectt = sprintf(cn2cd11e, m1, m2, m3, m1, m2, m3))
  bnds <- optimize_effect_2(obj2)
  save_bounds(obj2, bnds, sprintf("cn2cd11e.%s%s%s", m1, m2, m3))

  obj2 <- update_effect(obj, effectt = sprintf(cn21cd1e, m1, m2, m3, m1, m2, m3))
  bnds <- optimize_effect_2(obj2)
  save_bounds(obj2, bnds, sprintf("cn21cd1e.%s%s%s", m1, m2, m3))

  obj2 <- update_effect(obj, effectt = sprintf(cn12cd1e, m1, m2, m3, m1, m2, m3))
  bnds <- optimize_effect_2(obj2)
  save_bounds(obj2, bnds, sprintf("cn12cd1e.%s%s%s", m1, m2, m3))

})

stopCluster(cl)
