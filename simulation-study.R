## generate a dataset

task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(floor(runif(1)*1000) + task_id)

generate_data <- function(n = 100, um1eff = 0, um2eff = 0, uyeff = 0) {

  U <- rbinom(n, 1, .35)
  X <- rbinom(n, 1, .5)

  M1 <- rbinom(n, 1, (.4 - X * .14 + um1eff * U))
  M2 <- rbinom(n, 1, (.3 + X * .25 + .15 * M1 + um2eff * U))

  Y <- rbinom(n, 1, ifelse(X == 1, .125 + .15 * M1 + .2 * M2,
                           .75) + uyeff * U)

  data.frame(X = X, M1 = M1, M2 = M2, Y = Y, U = U)


}


true_values <- function(um1eff = 0, um2eff = 0, uyeff = 0) {

  pU <- .35

  fM1 <- function(X, U) {
    .4 - X * .14 + um1eff * U
  }
  fM2 <- function(X, M1, U) {
    (.3 + X * .25 + .15 * M1 + um2eff * U)
  }
  fY <- function(X, M1, M2, U) {
    ifelse(X == 1, .125 + .15 * M1 + .2 * M2,
           .75) + uyeff * U
  }


  ## delta E(Y(1->0, M_1(x_1 = 0), m_2 = 1))

  delta1 <- fY(1, 1, 1, 0) * fM1(0, 0) * (1 - pU) +
            fY(1, 1, 1, 1) * fM1(0, 1) * (pU) +
            fY(1, 0, 1, 0) * (1 - fM1(0, 0)) * (1 - pU) +
            fY(1, 0, 1, 1) * (1 - fM1(0, 1)) * (pU) -
    (fY(0, 1, 1, 0) * fM1(0, 0) * (1 - pU) +
     fY(0, 1, 1, 1) * fM1(0, 1) * (pU) +
     fY(0, 0, 1, 0) * (1 - fM1(0, 0)) * (1 - pU) +
     fY(0, 0, 1, 1) * (1 - fM1(0, 1)) * (pU))


  ## delta E(Y(1->0, m_1 = 0, M_2(x_2 = 1, M_1(x_3 = 1))))

  delta2a <- fY(1, 0, 1, 0) * fM2(1, 1, 0) * fM1(1, 0) * (1 - pU) +
    fY(1, 0, 0, 0) * (1 - fM2(1, 1, 0)) * fM1(1, 0) * (1 - pU) +
    fY(1, 0, 1, 0) * fM2(1, 0, 0) * (1 - fM1(1, 0)) * (1 - pU) +
    fY(1, 0, 0, 0) * (1 - fM2(1, 0, 0)) * (1 - fM1(1, 0)) * (1 - pU) +
    fY(1, 0, 1, 1) * fM2(1, 1, 1) * fM1(1, 1) * (pU) +
    fY(1, 0, 0, 1) * (1 - fM2(1, 1, 1)) * fM1(1, 1) * (pU) +
    fY(1, 0, 1, 1) * fM2(1, 0, 1) * (1 - fM1(1, 1)) * (pU) +
    fY(1, 0, 0, 1) * (1 - fM2(1, 0, 1)) * (1 - fM1(1, 1)) * (pU)

  delta2b <- fY(0, 0, 1, 0) * fM2(1, 1, 0) * fM1(1, 0) * (1 - pU) +
    fY(0, 0, 0, 0) * (1 - fM2(1, 1, 0)) * fM1(1, 0) * (1 - pU) +
    fY(0, 0, 1, 0) * fM2(1, 0, 0) * (1 - fM1(1, 0)) * (1 - pU) +
    fY(0, 0, 0, 0) * (1 - fM2(1, 0, 0)) * (1 - fM1(1, 0)) * (1 - pU) +
    fY(0, 0, 1, 1) * fM2(1, 1, 1) * fM1(1, 1) * (pU) +
    fY(0, 0, 0, 1) * (1 - fM2(1, 1, 1)) * fM1(1, 1) * (pU) +
    fY(0, 0, 1, 1) * fM2(1, 0, 1) * (1 - fM1(1, 1)) * (pU) +
    fY(0, 0, 0, 1) * (1 - fM2(1, 0, 1)) * (1 - fM1(1, 1)) * (pU)

  delta2 <- delta2a - delta2b

  c(delta1, delta2)

}




estimate_effects <- function(data) {


  ecn1cd2e <- function(x, x1, m2) {

    with(subset(data, X == x & M1 == 0 & M2 == m2), mean(Y)) *
      with(subset(data, X == x1), mean(1 - M1)) +
      with(subset(data, X == x & M1 == 1 & M2 == m2), mean(Y)) *
      with(subset(data, X == x1), mean(M1))
  }

  delta1hat <- ecn1cd2e(1, 0, 1) - ecn1cd2e(0, 0, 1)


  ## delta E(Y(1->0, m_1 = 0, M_2(x_2 = 1, M_1(x_3 = 1))))


  ecn21cd1e <- function(x, m1, x2, x3) {

    with(subset(data, X == x & M1 == m1 & M2 == 0), mean(Y)) *
      with(subset(data, X == x2 & M1 == 0), mean(1 - M2)) *
      with(subset(data, X == x3), mean(1 - M1)) +
      with(subset(data, X == x & M1 == m1 & M2 == 1), mean(Y)) *
      with(subset(data, X == x2 & M1 == 0), mean(M2)) *
      with(subset(data, X == x3), mean(1 - M1)) +
      with(subset(data, X == x & M1 == m1 & M2 == 0), mean(Y)) *
      with(subset(data, X == x2 & M1 == 1), mean(1 - M2)) *
      with(subset(data, X == x3), mean(M1)) +
      with(subset(data, X == x & M1 == m1 & M2 == 1), mean(Y)) *
      with(subset(data, X == x2 & M1 == 1), mean(M2)) *
      with(subset(data, X == x3), mean(M1))

  }

  delta2hat <- ecn21cd1e(1, 0, 1, 1) - ecn21cd1e(0, 0, 1, 1)


  c(delta1hat, delta2hat)

  }


bound1 <- readRDS(file.path("bounds-functions", paste0("cn1cde2.01", ".rds")))
bound2 <- readRDS(file.path("bounds-functions", paste0("cn21cd1e.011", ".rds")))

compute_bounds <- function(data) {

  con.probs <- list()
  typs <- expand.grid(y = 0:1, m1 = 0:1, m2 = 0:1, x = 0:1)
  for(j in 1:nrow(typs)) {
    btmp <- data[data$X == typs[j,"x"],]
    con.probs[[with(typs[j, ], sprintf("p%s%s%s_%s", y, m1, m2, x))]] <-
      mean(btmp$Y == typs[j,"y"] &
             btmp$M1 == typs[j,"m1"] &
             btmp$M2 == typs[j,"m2"])
  }

  c(unlist(do.call(bound1, con.probs)),
  unlist(do.call(bound2, con.probs)))

}

# looks good, run some replicates

run_one <- function(n = 100, um1eff = 0, um2eff = 0, uyeff = 0) {

  data <- generate_data(n,
                        um1eff, um2eff, uyeff)

  true <- true_values(um1eff, um2eff, uyeff)
  est <- estimate_effects(data)

  bootest <- matrix(NA, nrow = 1000, ncol = 2)
  for(i in 1:nrow(bootest)) {

    dboot <- data[sample(1:nrow(data), nrow(data), replace = TRUE),]
    bootest[i, ] <- estimate_effects(dboot)

  }


  bnds <- compute_bounds(data)

  data.frame(n = n,
             true = true,
             est = est,
             lower.ci = apply(bootest, 2, quantile, .025, na.rm = TRUE),
             upper.ci = apply(bootest, 2, quantile, .975, na.rm = TRUE),
             lower.bnd = bnds[c(1, 3)],
             upper.bnd = bnds[c(2, 4)],
             what = c("delta1", "delta2"),
             um1eff = um1eff,
             um2eff = um2eff,
             uyeff = uyeff
  )


}

fname <- paste0("runs-", task_id, ".csv")
write.table(run_one(50), file = fname, append = TRUE,
            row.names = FALSE, sep = ";", col.names = FALSE)

write.table(run_one(100), file = fname, append = TRUE,
            row.names = FALSE, sep = ";", col.names = FALSE)

write.table(run_one(200), file = fname, append = TRUE,
            row.names = FALSE, sep = ";", col.names = FALSE)


write.table(run_one(50, -.1, -.15, -.1), file = fname, append = TRUE,
            row.names = FALSE, sep = ";", col.names = FALSE)

write.table(run_one(100, -.1, -.15, -.1), file = fname, append = TRUE,
            row.names = FALSE, sep = ";", col.names = FALSE)

write.table(run_one(200, -.1, -.15, -.1), file = fname, append = TRUE,
            row.names = FALSE, sep = ";", col.names = FALSE)


