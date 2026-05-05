library(rioja)
library(riojaPlot)
library(dplyr)
poll <- aber$spec
chron <- aber$ages
colnames(poll) <- aber$names$Name 
mx <- sapply(poll, max) 
selTaxa <- names(mx[mx > 2]) 
clust <- chclust(dist(sqrt(poll)))
chron$Zone <- cutree(clust, k=5)
zones <- chron %>% group_by(Zone) %>% summarise(zm=mean(`Age (years BP)`)) %>%
  mutate(name=paste("Zone", Zone)) %>% select(-Zone)
zone.names <- paste("Zone", 1:5)
riojaPlot(poll, chron, selVars=selTaxa,
          yvar.name="Age (years BP)",
          ymin=6000, ymax=14300, yinterval=500,
          scale.percent=TRUE,
          cex.xaxis=0.5, 
          cex.xlabel=0.7,
          xRight=0.82) |>
  addRPZoneNames(zones, xRight=0.9, cex=0.6)  |>
  addRPClustZone(clust, col="red") |>
  addRPClust(clust)

Marcel