do.kmeans <- function(dataset,lbls, k=4, do.scatter=F) {
  set.seed(123)
  data.clu = kmeans(dataset, centers=k, nstart=10)
  if (do.scatter) {
    plot(dataset,type='n')
    text(dataset,labels=lbls,col=rainbow(k)[data.clu$cluster])    
  }
  data.clu
}