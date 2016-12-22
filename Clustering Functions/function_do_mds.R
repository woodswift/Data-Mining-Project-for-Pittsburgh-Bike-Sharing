do.mds <- function(dataset,lbls,do.scatter=T) {
  data.dist = dist(dataset)
  data.mds = cmdscale(data.dist)
  if (do.scatter) {
    plot(data.mds, type = 'n', main = 'MDS map in a 2-dimensional space')
    text(data.mds,labels=lbls)       
  }
  data.mds
}