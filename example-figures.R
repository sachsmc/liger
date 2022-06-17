library(mediation)
library(causaloptim)
library(ggplot2)
library(patchwork)

## covid figure first

coviddata <- read.csv("dataoutcovidliger.csv")
boundsdata <- with(coviddata, data.frame(trt = Z, outcome = Infection,
                                         M1 = cinq, M2 = binq))


con.probs <- list()
typs <- expand.grid(y = 0:1, m1 = 0:1, m2 = 0:1, x = 0:1)
for(j in 1:nrow(typs)) {
  btmp <- boundsdata[boundsdata$trt == typs[j,"x"],]
  con.probs[[with(typs[j, ], sprintf("p%s%s%s_%s", y, m1, m2, x))]] <-
    mean(btmp$outcome == typs[j,"y"] &
           btmp$M1 == typs[j,"m1"] &
           btmp$M2 == typs[j,"m2"])
}


figa.bnds.nms <- sprintf("cn1cde2.%s%s", c(0,0,1,1), c(0,1,0,1))
bfuncfiga <- lapply(figa.bnds.nms, function(x) {
  readRDS(file.path("bounds-functions", paste0(x, ".rds")))
})
names(bfuncfiga) <- figa.bnds.nms

woo <- expand.grid(x1 = 0:1, x2 = 0:1, x3 = 0:1)
figb.bnds.nms <- c(sprintf("cn2cd11e.%s%s%s", woo$x1, woo$x2, woo$x3),
                   sprintf("cn12cd1e.%s%s%s", woo$x1, woo$x2, woo$x3),
                   sprintf("cn21cd1e.%s%s%s", woo$x1, woo$x2, woo$x3))

bfuncfigb <- bfuncfigb <- lapply(figb.bnds.nms, function(x) {
  readRDS(file.path("bounds-functions", paste0(x, ".rds")))
})
names(bfuncfigb) <- figb.bnds.nms

figa.bnds <- do.call(rbind, lapply(bfuncfiga, function(bndfun) do.call(bndfun, con.probs)))
#figb.bnds <- do.call(rbind, lapply(bfuncfigb, function(bndfun) do.call(bndfun, con.probs)))

## swap M1 and M2 then recompute


boundsdataswap <- boundsdata
boundsdataswap$M1 <- boundsdata$M2
boundsdataswap$M2 <- boundsdata$M1

con.probs.swap <- list()
typs <- expand.grid(y = 0:1, m1 = 0:1, m2 = 0:1, x = 0:1)
for(j in 1:nrow(typs)) {
  btmp <- boundsdataswap[boundsdataswap$trt == typs[j,"x"],]
  con.probs.swap[[with(typs[j, ], sprintf("p%s%s%s_%s", y, m1, m2, x))]] <-
    mean(btmp$outcome == typs[j,"y"] &
           btmp$M1 == typs[j,"m1"] &
           btmp$M2 == typs[j,"m2"])
}



figa.bnds.sw <- do.call(rbind, lapply(bfuncfiga, function(bndfun) do.call(bndfun, con.probs.swap)))

figa.bnds$name <- gsub("e2", "2e", names(bfuncfiga))
figa.bnds$figure <- "Under the DAG in Fig. 1a"

figa.bnds.sw$name <- gsub("e2", "2e", names(bfuncfiga))
figa.bnds.sw$name <- gsub("cn1", "cn2", figa.bnds.sw$name)
figa.bnds.sw$name <- gsub("cd2", "cd1", figa.bnds.sw$name)

figa.bnds.sw$figure <- "Under the DAG in Fig. 1a"

figa.bnds$name2 <-  sprintf("Delta*E*(Y(1 %%->%% 0, M[1](%s), %s))", c(0,0,1,1), c(0,1,0,1))
figa.bnds.sw$name2 <- sprintf("Delta*E*(Y(1 %%->%% 0, %s, M[2](%s)))", c(0,0,1,1), c(0,1,0,1))

figa.bnds <- rbind(figa.bnds, figa.bnds.sw)

point.estimates <- subset(readRDS("point-ests-covid.rds"), type == "diff")

figa.bnds <- merge(figa.bnds, point.estimates, by.x = "name", by.y = "estimand")



true.vals <- subset(readRDS("true-values-covid.rds"), type == "diff")
colnames(true.vals)[1] <- "truth"

figa.bnds <- merge(figa.bnds, true.vals, by.x = "name", by.y = "estimand")

thisplot <- list(geom_linerange(aes(y = name2,
                                    xmin = lower, xmax = upper)),
                 geom_point(aes(x = estimate, y = name2)),
                 geom_point(aes(x = lower.limit, y = name2), shape = "|", size = 3),
                 geom_point(aes(x = upper.limit, y = name2), shape = "|", size = 3),
                 geom_point(aes(x = truth, y = name2), shape = 5),
              #   facet_wrap(~ figure, scales = "free", labeller = label_value),
                 theme_bw(), ylab(""), xlab("Risk difference"),
                 scale_y_discrete(labels = scales::label_parse()))



p1 <- ggplot(figa.bnds) + thisplot
p1

ggsave("covid-figure.pdf", width = 4.25, height = 6.5)


point.estimates <- readRDS("point-ests-covid.rds")
true.vals <- readRDS("true-values-covid.rds")

fintab <- figa.bnds[, c(1, 2, 3, 5)]
fintab$type <- "diff"

fintab2 <- merge(fintab, point.estimates, by.x = c("name", "type"),
      by.y = c("estimand", "type"), all = TRUE)

colnames(true.vals)[1] <- "true"
fintab3 <- merge(fintab2, true.vals,  by.x = c("name", "type"),
by.y = c("estimand", "type"), all = TRUE)

outtab <- data.frame(fintab3$name2, fintab3$type, fintab3$true,
           sprintf("%.2f (%.2f to %.2f)", fintab3$estimate,
                   fintab3$lower.limit, fintab3$upper.limit),
           sprintf("[%.2f to %.2f]", fintab3$lower,
                   fintab3$upper))
outtab <- outtab[order(outtab[, 1], outtab[,2 ]),-2]

colnames(outtab) <- c("estimand", "true",
                      "estimate (95\\% CI)", "bounds")
library(xtable)
print(xtable(outtab), include.rownames = FALSE)

### framing


data(framing)
names(framing)

boundsdata<-as.data.frame(framing$treat)
names(boundsdata)<-"trt"

boundsdata$outcome<-(framing$immigr>=3)*1
boundsdata$M1<-(framing$emo>=8)*1
boundsdata$M2<-(framing$p_harm>=6)*1


con.probs <- list()
typs <- expand.grid(y = 0:1, m1 = 0:1, m2 = 0:1, x = 0:1)
for(j in 1:nrow(typs)) {
  btmp <- boundsdata[boundsdata$trt == typs[j,"x"],]
  con.probs[[with(typs[j, ], sprintf("p%s%s%s_%s", y, m1, m2, x))]] <-
    mean(btmp$outcome == typs[j,"y"] &
           btmp$M1 == typs[j,"m1"] &
           btmp$M2 == typs[j,"m2"])
}


figa.bnds.nms <- sprintf("cn1cde2.%s%s", c(0,0,1,1), c(0,1,0,1))
bfuncfiga <- lapply(figa.bnds.nms, function(x) {
  readRDS(file.path("bounds-functions", paste0(x, ".rds")))
})
names(bfuncfiga) <- figa.bnds.nms

woo <- expand.grid(x1 = 0:1, x2 = 0:1, x3 = 0:1)
figb.bnds.nms <- c(sprintf("cn2cd11e.%s%s%s", woo$x1, woo$x2, woo$x3),
                   sprintf("cn12cd1e.%s%s%s", woo$x1, woo$x2, woo$x3),
                   sprintf("cn21cd1e.%s%s%s", woo$x1, woo$x2, woo$x3))

bfuncfigb <- bfuncfigb <- lapply(figb.bnds.nms, function(x) {
  readRDS(file.path("bounds-functions", paste0(x, ".rds")))
})
names(bfuncfigb) <- figb.bnds.nms

figa.bnds <- do.call(rbind, lapply(bfuncfiga, function(bndfun) do.call(bndfun, con.probs)))
figb.bnds <- do.call(rbind, lapply(bfuncfigb, function(bndfun) do.call(bndfun, con.probs)))

figa.bnds$name <- gsub("e2", "2e", names(bfuncfiga))
figb.bnds$name <- names(bfuncfigb)
figa.bnds$figure <- "Under the DAG in Fig. 1a"
figb.bnds$figure <- "Under the DAG in Fig. 1b"

point.estimates <- readRDS("point-ests.rds")

figa.bnds <- merge(figa.bnds, point.estimates, by.x = "name", by.y = "estimand")
figb.bnds <- merge(figb.bnds, point.estimates, by.x = "name", by.y = "estimand")


# figa.bnds$name2 <-  sprintf("CN[1]*CD[2]*E*'-10:%s%s'", c(0,0,1,1), c(0,1,0,1))
# figb.bnds$name2 <- c(sprintf("CN[2]*CD[11]*E*'-10:%s%s%s'", woo$x1, woo$x2, woo$x3),
#                      sprintf("CN[12]*CD[1]*E*'-10:%s%s%s'", woo$x1, woo$x2, woo$x3),
#                      sprintf("CN[21]*CD[1]*E*'-10:%s%s%s'", woo$x1, woo$x2, woo$x3))


figa.bnds$name2 <-  sprintf("Delta*E*(Y(1 %%->%% 0, M[1](%s), %s))", c(0,0,1,1), c(0,1,0,1))

figb.bnds$name2 <- c(sprintf("Delta*E(Y(1 %%->%% 0, %s, M[2](%s, %s)))", woo$x1, woo$x2, woo$x3),
                     sprintf("Delta*E(Y(1 %%->%% 0, M[1](%s), M[2](%s, %s)))", woo$x1, woo$x2, woo$x3),
                     sprintf("Delta*E(Y(1 %%->%% 0, M[1](%s), M[2](%s, M[1](%s))))", woo$x1, woo$x2, woo$x3))


thisplot <- list(geom_linerange(aes(y = name2,
                                    xmin = lower, xmax = upper)),
                 geom_point(aes(x = estimate, y = name2)),
                 geom_point(aes(x = lower.limit, y = name2), shape = "|", size = 3),
                 geom_point(aes(x = upper.limit, y = name2), shape = "|", size = 3),
                 #facet_wrap(~ figure, scales = "free", labeller = label_value),
                 theme_bw(), ylab(""), xlab("Risk difference"),
                 scale_y_discrete(labels = scales::label_parse()))



p2 <- ggplot(rbind(figa.bnds, figb.bnds)) + thisplot
p2

ggsave("framing-figure.pdf", width = 4.25, height = 6.5)


p2 + p1 + plot_layout(ncol = 2)
ggsave("combined-figure.pdf", width = 7.75, height = 6.5)

