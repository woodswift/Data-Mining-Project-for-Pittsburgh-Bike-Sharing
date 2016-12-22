do.pca <- function(dataset,lbls,
                   do.screeplot=F,do.scatter=F,do.biplot=F,do.loadingplot=F) {
  data.pca = prcomp(dataset, scale=TRUE) 
  data.pc = predict(data.pca)
  if (do.screeplot) {
    plot(data.pca, main='screeplot for PCA')
  }
  if (do.scatter) {
    plot(data.pc[,1:2], type="n", main='scatterplot on the first two PCs')
    text(x=data.pc[,1], y=data.pc[,2], labels=lbls)    
  }
  if (do.biplot) biplot(data.pca)
  if (do.loadingplot) {
    plot(data.pca$rotation[,1], type='l', main='loadings for the first PC')
    # plot(data.pc[,1],type='l')
  }
  data.pc
}