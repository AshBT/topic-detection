# topic-detection

Provides the implementation of a topic detection framework developed for the MULTISENSOR project. In this framework, topic detection is tackled as a clustering problem and a hybrid clustering approach for assigning news articles into topics is realized. In this approach, prior knowledge of the correct number of clusters/topics is not required, as this number is automatically estimated by means of a novel methodology named DBSCAN-Martingale. The assignment of news articles into topics is done using Latent Dirichlet Allocation (LDA).

#Description

The ```DBSCAN_Martingale.r``` script has been developed in R, version 3.2.3 and requires the “dbscan” R package. The input is a data matrix to be clustered. The output is a probability distribution over the number of clusters and a barplot, showing the number of clusters that is more probable to describe the optimal partitioning of a dataset.

The ```topic_detection_module.r``` script requires as input a folder, which contains a list of text documents. The output is a JSON file with a list of topics. The requirements are the R packages “tm”, “dbscan”, “topicmodels” and “rjson”.

# Version
1.0.0


  




