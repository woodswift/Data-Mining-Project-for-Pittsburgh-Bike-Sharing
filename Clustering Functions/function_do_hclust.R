do.hclust <- function(dataset,lbls, dist.mtd='single', k=4, do.dendrogram=F) {
  switch(dist.mtd,
         single={
           data.dist = dist(dataset)
           hc = hclust(data.dist, method='single')
           if (do.dendrogram) plot(hc)
           hc1 = cutree(hc,k)
           print(hc1)
           hc1
         },
         complete={
           data.dist = dist(dataset)
           hc = hclust(data.dist, method='complete')
           if (do.dendrogram) plot(hc)
           hc1 = cutree(hc,k)
           print(hc1)
           hc1
         },
         average={
           data.dist = dist(dataset)
           hc = hclust(data.dist, method='average')
           if (do.dendrogram) plot(hc)
           hc1 = cutree(hc,k)
           print(hc1)
           hc1
         })
}