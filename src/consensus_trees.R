require(tidyverse)
require(stylo)
require(ape)

### make data frame containing all relevant data; the following loop makes a data frame from files in a directory

files.v <- dir(pattern = ".RDS") # be sure to switch to correct target directory

holder.l <- vector(mode = "list", length(files.v)) # a list to hold results


for (i in seq_along(files.v)) {
  
  x <- readRDS(files.v[i])
  
  x <-  x %>%
    filter(upos != "PUNCT")
  
  # identify columns with binary values (drop meta data columns)
  a <- which(str_detect(colnames(x ), "_Is_"))
  b <- which(str_detect(colnames(x ), "_Are_"))
  index.v <- c(a, b)
  
  x <- x[, index.v]
  
  holder.l[[i]] <- x %>%
    colSums() / nrow(x) # store relative frequencies in list element
  
  print(paste("finished file", i))
  
}



result.df <- do.call(bind_rows, holder.l) #combine list elements into single data frame

# reorder columns from most to least frequent input variables

m <- colMeans(result.df)
o <- order(m, decreasing = TRUE)
result.df <-  result.df[, o]


#loops to generate data for consensus clustering 
# all loops create multiple cluster analyses using an incrementally increasing selection of input variables (columns)

# set 
first.cols.v <- 1:19
last.col.v <- seq(20, ncol(result.df), 2) # make vector of numbers to select different sets of columns

last.col.v <- c(last.col.v, 1025) # may be needed if number of columns is odd; if so, set the final parameter to the number of the final column




# each loop uses a different combination of parameters for the cluster analysis. Only a few possible combinations are implemented below.
# parameters to vary: 
### the distance measure: package stylo offers these measures associated with textual studies: dist.cosine, dist.delta, dist.entropy
# dist.minmax, dist.simple, and dist.wurzburg; there are also subclasses of delta (dist.argamon and dist.eder)

### the linkage parameter in hsclust(): ward.D, ward.D2, single, complete, average, mcquitty, median, centroid.

###### a series of loops

keeper.l <- vector(mode = "list", length(last.col.v)) # a list to store results
for (i in seq_along(keeper.l)) {
  a <- result.df[,c(first.cols.v,1:last.col.v[i])] # select sets of columns, from most to least frequent
  b <- dist.delta(as.matrix(a))# generate a distance matrix
  c <- hclust(b,  method = "average") # make a cluster analysis
  d <- as.phylo(c) #class phylo is needed for the ape package
  
  keeper.l[[i]] <- d # store the result
  
  print(paste0("finished delta average ", i))
  
}

big.keeper.l <- keeper.l

for (i in seq_along(keeper.l)) {
  a <- result.df[,c(first.cols.v,1:last.col.v[i])] # select sets of columns, from most to least frequent
  b <- dist.delta(as.matrix(a))# generate a distance matrix
  c <- hclust(b,  method = "single") # make a cluster analysis
  d <- as.phylo(c) #class phylo is needed for the ape package
  
  keeper.l[[i]] <- d # store the result
  
  print(paste0("finished delta single ", i))
  
}

big.keeper.l <- keeper.l

keeper.l <- vector(mode = "list", length(last.col.v))
for (i in seq_along(keeper.l)) {
  a <- result.df[,c(first.cols.v,1:last.col.v[i])] # select sets of columns, from most to least frequent
  b <- dist.cosine(as.matrix(a))
  c <- hclust(b, method = "average")
  d <- as.phylo(c)
  
  keeper.l[[i]] <- d
  
  print(paste0("finished cosine average ", i))
  
}

big.keeper.l <- c(big.keeper.l, keeper.l) # a list to combine results from several loops

keeper.l <- vector(mode = "list", length(last.col.v))
for (i in seq_along(keeper.l)) {
  a <- result.df[,c(first.cols.v,1:last.col.v[i])] # select sets of columns, from most to least frequent
  b <- dist.eder(as.matrix(a))
  c <- hclust(b, method = "single")
  d <- as.phylo(c)
  
  keeper.l[[i]] <- d
  
  print(paste0("finished cosine single ", i))
  
}
big.keeper.l <- c(big.keeper.l, keeper.l)

#########
### end of loops series


## make consensus trees of the multiple analyses

c2 <- consensus(big.keeper.l, p = .5) 

c2$tip.label <- files.v %>%
  gsub("_binary_val.RDS", "", .) # add labels made from file names; the gsub() parameters will need to be adjusted depending on your file names


plot.phylo(c2, "u") #an unrooted dendrogram 


