library(data.table)
library(xtable)

files <- list.files(pattern = "runs")
res <- do.call(rbind,lapply(files,
       \(x) {
         tmp <- readLines(x)
         tmp <- tmp[sapply(strsplit(tmp, ";"), length) == 11]
         tmpf <- tempfile()
         writeLines(tmp, tmpf)
         read.table(tmpf, header = FALSE, sep = ";", dec = ".")
       }))

res <- read.table("allruns.csv", header = FALSE, sep = ";", dec =".")

colnames(res) <- c("n",
                   "true",
                   "est",
                   "lower.ci",
                   "upper.ci",
                   "lower.bnd",
                   "upper.bnd",
                   "what",
                   "um1eff",
                   "um2eff",
                   "uyeff")

res <- as.data.table(res)


res$setting <- ifelse(res$um1eff != 0, "confounding", "no confounding")

res$bias <- res$est - res$true
res$cover <- 1.0 * (res$true > res$lower.ci & res$true < res$upper.ci)
res <- res[!is.na(est) & !is.na(n),]

tres <- res[, .(percentbias= mean(bias / true)*100, sdbias = sd(bias), coverage = mean(cover),
        meanlowerbound = mean(lower.bnd), meanupperbound = mean(upper.bnd),
        meanboundwidth = mean(upper.bnd - lower.bnd)), keyby = .(setting, what, n)]



write.csv(tres, file = "simtable.csv", row.names = FALSE)
