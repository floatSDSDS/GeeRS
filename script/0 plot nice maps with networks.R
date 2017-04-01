library(maps)
library(geosphere)

map("world", fill=T, col="grey8", bg="grey15")


for (i in (1:dim(meltCN)[1])) { 
}

points(geoState$meanlon,geoState$meanlat, pch=3, cex=0.1, col="chocolate1")

  
  inter <- gcIntermediate(
    as.vector(geoState[geoState$state==meltCN$Var1[i],2:3]),
    as.vector(geoState[geoState$state==meltCN$Var2[i],2:3])
  )
  
  lines(inter, lwd=0.1, col="turquoise2")    