<!-- DBSCAN_Martingale.R -->
library(dbscan)
minpts = 50
eps_max = 0.5
T = 5
realizations = 10

### realizations of the DBSCAN-Martingale ###
### requires as input a matrix x with the dataset to be clustered ### 

realizations.DBSCAN_martingale = matrix(0, nrow = realizations, ncol = T)

principal.clustering.all = matrix(0, nrow = realizations, ncol = dim(x)[1])


for(r in 1:realizations) {
  
  number.of.clusters = c()
  
  ### generate 10 random numbers from the uniform distribution in [0, eps_max] ###
  random.epsilon = runif(T, min=0, max = eps_max)
  random.epsilon = sort(random.epsilon, decreasing = FALSE)
  
  dbscan.results.all = matrix(0,nrow=dim(x)[1],ncol=T)
  for(j in 1:T) dbscan.results.all[,j] = dbscan(x, random.epsilon[j], minpts)$cluster
  
  principal.clustering = dbscan.results.all[,1]
  
  for(j in 1:T) {
    if((principal.clustering%*%dbscan.results.all[,j])[1,1]==0) {
      b = max(principal.clustering)
      for(i in 1:length(dbscan.results.all[,j])) if(dbscan.results.all[i,j]!=0) dbscan.results.all[i,j] = dbscan.results.all[i,j] + b
      principal.clustering = principal.clustering + dbscan.results.all[,j]
    } else {
      h = c()
      clh = c()
      for(i in 1:length(principal.clustering)) {
        h = c(h,0)
        clh = c(clh,0)
      }
      for(i in 1:length(principal.clustering)) if(principal.clustering[i]==0 && dbscan.results.all[i,j] != 0) h[i]=dbscan.results.all[i,j]
      b = max(principal.clustering)
      u = 0
      if(max(h)>0) {
        for(j in 1:max(h)) if(sum(h==j)>=minpts+1) {
          u = u + 1
          clh[which(h==j)]= u
        }
        for(i in 1:length(principal.clustering)) if(clh[i]!=0) clh[i] = clh[i] + b
        principal.clustering = principal.clustering + clh
      }
    }
    number.of.clusters = c(number.of.clusters,max(principal.clustering)) 
  }
  
  principal.clustering.all[r,] = principal.clustering
  
  print(max(principal.clustering))
  print(r)
    
  realizations.DBSCAN_martingale[r,] = number.of.clusters
}

#realizations.DBSCAN_martingale[,T]

### number of clusters probability ###
table(realizations.DBSCAN_martingale[,T])/realizations


### probability distribution - barplot ###
barplot(table(realizations.DBSCAN_martingale[,T])/realizations, col = "blue", xlab = "clusters", ylab = "probability")
