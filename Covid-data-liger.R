
set.seed(64)
Z<-rbinom(1000,1, 0.5)

C<-rnorm(1000,0.5+Z*1, 1)
Q<-exp(rnorm(1000, 6.5+.5*Z, .3))

Yz1<-rbinom(1000,1, exp((0.8690378/-900)*Q +  -0.3*C -2)*0.1)
Yz0<-rbinom(1000,1, 0.1)

1-(mean(Yz1)/mean(Yz0))

table(Q > 1000, Z)

1 - (exp((0.8690378/-900)*100 +  -0.3*1 -2))
1 - (exp((0.8690378/-900)*1000 +  -0.3*1 -2))

binq<-ifelse(Q>=1000,1,0)
cinq<-ifelse(C>=1,1,0)
Infection<-ifelse(Z==1,Yz1, Yz0)

dataoutcovidliger<-as.data.frame(cbind(Z,cinq,binq,Infection))
write.csv(dataoutcovidliger, "dataoutcovidliger.csv")
