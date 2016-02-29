<!—topic_detection_module.R -->
input_folder = "<path to folder here or folder name>"


library(tm)
corp = VCorpus(DirSource(input_folder, encoding = "UTF-8", mode = "text"))
dtm1 = DocumentTermMatrix(corp, control=list(bounds = list(global = c(20,Inf))))
dtm1_forLDA = DocumentTermMatrix(corp, control=list(bounds = list(global = c(3,Inf))))
rowTotals = apply(dtm1_forLDA , 1, sum)
dtm1_forLDA = dtm1_forLDA[rowTotals> 0, ]
dtm1_forDBSCAN = as.matrix(dtm1)

# l1 normalization
for(j in 1:dim(dtm1_forDBSCAN)[2]) dtm1_forDBSCAN[,j] = dtm1_forDBSCAN[,j]/sum(dtm1_forDBSCAN[,j])


### DBSCAN-Martingale
library(dbscan)
minpts = 5
T = 5


### generate 10 random numbers from the uniform distribution in [0, 0.1]
random.epsilon = runif(T, min=0, max = 0.1)
random.epsilon = sort(random.epsilon, decreasing = FALSE)
dbscan.results.all = matrix(0, nrow = dim(dtm1)[1], ncol=T)

for(j in 1:T) dbscan.results.all[,j] = dbscan(dtm1_forDBSCAN, random.epsilon[j], minpts)$cluster
# giant cluster removal
for(j in 1:T) {
  for(i in 1:length(dbscan.results.all[,j])) dbscan.results.all[i,j]= dbscan.results.all[i,j] - 1
  for(i in 1:length(dbscan.results.all[,j])) if(dbscan.results.all[i,j]==-1) dbscan.results.all[i,j]=0
}
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
      for(j in 1:max(h)) if(sum(h==j)>=minpts) {
        u = u + 1
        clh[which(h==j)]= u
      }
      for(i in 1:length(principal.clustering)) if(clh[i]!=0) clh[i] = clh[i] + b
      principal.clustering = principal.clustering + clh
    }
  }
}
num_of_topics = max(principal.clustering)

### LDA using the num_of_topics
library(topicmodels)

k = if(num_of_topics<2) 2 else num_of_topics
LDA.results = LDA(dtm1_forLDA, k)
LDA.clustering.vector = rep(0, dtm1_forLDA$nrow)
for(i in 1:dtm1_forLDA$nrow) LDA.clustering.vector[i] = which.max(LDA.results@gamma[i,])


### assign the documents in each topic
topics.list.IDs = vector("list", k+1)
names(topics.list.IDs) = as.character(0:k)

for(i in 1:k) {
  topics.list.IDs[[i+1]] = vector("list", 3)
  names(topics.list.IDs[[i+1]]) = c("labels", "scores", "articles")
  topics.list.IDs[[i+1]][[1]] = paste(LDA.results@terms[sort(LDA.results@beta[i,], decreasing = TRUE, index.return = TRUE)$ix[1:8]], collapse = " ")
  topics.list.IDs[[i+1]][[2]] = abs(sort(LDA.results@beta[i,], decreasing = TRUE)[1:8])
  topics.list.IDs[[i+1]][[3]] = dtm1_forLDA$dimnames$Docs[which(LDA.clustering.vector==i)]
}


### create a collection of "noise"-empty documents
if(length(which(rowTotals==0))>0) {
  topics.list.IDs[[1]] = vector("list", 2)
  names(topics.list.IDs[[1]]) = c("labels", "articles")
  topics.list.IDs[[1]][[1]] = "noise"
  topics.list.IDs[[1]][[2]] = dtm1$dimnames$Docs[which(rowTotals==0)]
} else topics.list.IDs = topics.list.IDs[-1]


### write the results to a JSON file
library(rjson)
exportJSON = toJSON(topics.list.IDs)

write(exportJSON, file = "topics.json")
