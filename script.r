# Copyright (c) Microsoft Corporation.  All rights reserved.

# Third Party Programs. This software enables you to obtain software applications from other sources. 
# Those applications are offered and distributed by third parties under their own license terms.
# Microsoft is not developing, distributing or licensing those applications to you, but instead, 
# as a convenience, enables you to use this software to obtain those applications directly from 
# the application providers.
# By using the software, you acknowledge and agree that you are obtaining the applications directly
# from the third party providers and under separate license terms, and that it is your responsibility to locate, 
# understand and comply with those license terms.
# Microsoft grants you no license rights for third-party software or applications that is obtained using this software.

##PBI_R_VISUAL: VIZGAL_CLUSTERING  Graphical display of a clustering applied to point cloud 
# Computes and visualizes a clustering performed with KMEANS clustering algorithm. 
# Allows user to control number of clusters or to find it automatically. 
# Provides several options for scaling the data and for visualization of clusters. 
# INPUT: 
# The input dataset should include at least two numerical non-constant columns  
#
# EXAMPLES:
# for R environment
#  data(iris)
#  dataset=iris[,c(5,1,2,3,4)]
#  source("visGal_clustering.R") #create graphics
#
# WARNINGS:  Time consuming for large datasets
#
# CREATION DATE: 06/01/2016
#
# LAST UPDATE: 04/12/2016
#
# VERSION: 0.0.2
#
# R VERSION TESTED: 3.2.2
# 
# AUTHOR: pbicvsupport@microsoft.com
#
# REFERENCES: http://www.inside-r.org/r-doc/stats/kmeans
#             https://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set
#             https://cran.r-project.org/web/packages/NbClust/NbClust.pdf


#DEBUG purposes
#save(list = ls(all.names = TRUE), file='C:/Users/boefraty/projects/PBI/R/tempData.Rda')
#load(file='C:/Users/boefraty/projects/PBI/R/tempData.Rda')


if(!exists("dataset") && exists("Values"))
  dataset = Values

if(!exists("dataset") && !exists("Values") && exists("PointLabels"))
  dataset = PointLabels


#PBI_EXAMPLE_DATASET for debugging purposes
if(!exists("dataset"))
{
  data(iris) #Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,Species
  dataset = iris[,c(5, 1, 2, 3, 4)]
}

if(!exists("PointLabels"))
  PointLabels = NULL  

if(!is.null(PointLabels))
  dataset = cbind(PointLabels,dataset)

##PBI_PARAM: Specify if legend is to be present on the plot
#Type: logical, Default:TRUE, Range:NA, PossibleValues:NA, Remarks: NA
addLegend = TRUE
if(exists("settings_legend_params_show"))
  addLegend = settings_legend_params_show

############ User Parameters #########

if(exists("settings_prepocessing_params_show") && settings_prepocessing_params_show == FALSE)
  rm(list= ls(pattern = "settings_prepocessing_params_"))
if(exists("settings_clusterNum_params_show") && settings_clusterNum_params_show == FALSE)
  rm(list= ls(pattern = "settings_clusterNum_params_"))
if(exists("settings_viz_params_show") && settings_viz_params_show == FALSE)
  rm(list= ls(pattern = "settings_viz_params_"))
if(exists("settings_labeling_params_show") && settings_labeling_params_show == FALSE)
  rm(list= ls(pattern = "settings_labeling_params_"))
if(exists("settings_representative_params_show") && settings_representative_params_show == FALSE)
  rm(list= ls(pattern = "settings_representative_params_"))
if(exists("settings_legend_params_show") && settings_legend_params_show == FALSE)
  rm(list= ls(pattern = "settings_legend_params_"))
if(exists("settings_additional_params_show") && settings_additional_params_show == FALSE)
  rm(list= ls(pattern = "settings_additional_params_"))

##PBI_PARAM: Should warnings text be displayed?
#Type:logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: NA
showWarnings = TRUE 
if(exists("settings_additional_params_showWarnings"))
  showWarnings = settings_additional_params_showWarnings

##PBI_PARAM: Number of clusters to detect
#Type:integer/string, Default:'auto', Range:NA, PossibleValues:2,3,...,15, "auto" , Remarks: NA
numOfClusters = 0 
if(exists("settings_clusterNum_params_numOfClusters"))
{
  numOfClusters = as.numeric(settings_clusterNum_params_numOfClusters)
  if(is.na(numOfClusters))
    numOfClusters = 0
}

##PBI_PARAM: Method to apply for automatic number of clusters detection
# possible values "fast", "moderate","slow"
#Type: string, Default:"fast", Range:NA, PossibleValues:{"fast","moderate","slow"}, Remarks: NA
numClustersMethods = "fast"
if(exists("settings_clusterNum_params_numClustersMethods"))
{
  numClustersMethods = settings_clusterNum_params_numClustersMethods
  if(numClustersMethods=="None")
    numClustersMethods = "fast"
}

##PBI_PARAM: standardize each column (zero mean, unit standard deviation)
#Type: logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks:strongly recommended if each column measured in different units
scaleData = FALSE
if(exists("settings_prepocessing_params_scaleData"))
  scaleData = settings_prepocessing_params_scaleData 

##PBI_PARAM: apply Principle Component Analysis?
#an orthogonal transformation to convert a set of observations of possibly correlated variables 
#into a set of values of linearly uncorrelated variables. New dimensions are sorted by the amount of variance they explain
#Type: logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: Original units got lost. 
applyPCA = FALSE
if(exists("settings_prepocessing_params_applyPCA"))
  applyPCA = settings_prepocessing_params_applyPCA 

##PBI_PARAM:Specify if ellipse of 85% confidence should be drawn
#Type: logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: NA
drawEllipse = FALSE
if(exists("settings_viz_params_drawEllipse"))
  drawEllipse = settings_viz_params_drawEllipse 

##PBI_PARAM: Specify if convex hull of each cluster should be drawn
#Type: logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: NA
drawConvexHull = FALSE
if(exists("settings_viz_params_drawConvexHull"))
  drawConvexHull = settings_viz_params_drawConvexHull 

##PBI_PARAM: If addLabel2clusterDelegate is TRUE, 
# the data of one observation closest to the center of cluster is added on top of graph 
#Type: logical, Default:TRUE, Range:NA, PossibleValues:NA, Remarks: NA
addLabel2clusterDelegate = FALSE
if(exists("settings_representative_params_show"))
  addLabel2clusterDelegate = settings_representative_params_show 

##PBI_PARAM: If addLabel2points is TRUE will use first column to label points on the plot
#Type: logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: NA
addLabel2points = TRUE
if(exists("settings_labeling_params_show"))
  addLabel2points = settings_labeling_params_show 

# add label to points only if such labels are provided as input
addLabel2points <- (addLabel2points==TRUE && exists("PointLabels") && !is.null(PointLabels))

##PBI_PARAM: Specify if cluster centers should be plotted
#Type: logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: NA
drawCenters = FALSE
if(exists("settings_viz_params_drawCentroid"))
  drawCenters = settings_viz_params_drawCentroid

##PBI_PARAM: Specify if observation points should be plotted
#Type: logical, Default:TRUE, Range:NA, PossibleValues:NA, Remarks: NA
drawPoints = TRUE # deprecated because we can control transperancy instead 

##PBI_PARAM: minimum number of clusters
#Type: positive integer, Default:1, Range:[1:10], PossibleValues:NA, Remarks: NA
minClusters = 1 
if(exists("settings_additional_params_minClusters"))
  minClusters = settings_additional_params_minClusters
minClusters= max(min(minClusters,15),1)

##PBI_PARAM: maximum number of clusters
#Type: positive integer, Default:12, Range:[3:15], PossibleValues:NA, Remarks: NA
maxClusters = 12
if(exists("settings_additional_params_maxClusters"))
  maxClusters = settings_additional_params_maxClusters

maxClusters= max(min(maxClusters,15),minClusters)

##PBI_PARAM: max iterations in kmeans
#Type: positive integer, Default:7, Range:[1:100], PossibleValues:NA, Remarks: Influences running time
iter.max = 7 
if(exists("settings_additional_params_maxIter"))
  iter.max = settings_additional_params_maxIter

iter.max= max(min(iter.max,100),1)

##PBI_PARAM: max initializations in kmeans
#Type: positive integer, Default:5, Range:[1:100], PossibleValues:NA, Remarks: Influences running time
nstart = 5 
if(exists("settings_additional_params_nStart"))
  nstart = settings_additional_params_nStart
nstart= max(min(nstart,100),1)

##PBI_PARAM: transparency of points on plot, 0 is invisible, 1 is opaque
#Type: numeric, Default:0.3, Range:[0,1], PossibleValues:NA, Remarks: NA
pointTransparency = 0.3
if(exists("settings_viz_params_percentile"))
  pointTransparency = as.numeric(settings_viz_params_percentile)/100

##PBI_PARAM: maximum characters per delagate label
#Type: positive integer, Default:30, Range:[1,100], PossibleValues:NA, Remarks: NA
maxLenDelegate = 30  
if(exists("settings_representative_params_maxLenDelegateLabel"))
  maxLenDelegate = max(1,min(100,settings_representative_params_maxLenDelegateLabel))


##PBI_PARAM: font size for delegate text 
#Type: positive numeric, Default:0.5, Range:[0.1,2], PossibleValues:NA, Remarks: NA
delegateCex = 0.5
if(exists("settings_representative_params_textSize"))
  delegateCex = as.numeric(settings_representative_params_textSize)/10


##PBI_PARAM: font size for text on each point 
#Type: positive numeric, Default:0.5, Range:[0.1,2], PossibleValues:NA, Remarks: NA
cexLabel2points = 0.5
if(exists("settings_labeling_params_textSize"))
  cexLabel2points = as.numeric(settings_labeling_params_textSize)/10

##PBI_PARAM: transparency of labels for points on plot, 0 is invisible, 1 is opaque
#Type: numeric, Default:0.75, Range:[0,1], PossibleValues:NA, Remarks: NA
transparencyLabel2points = 1
if(exists("settings_labeling_params_percentile"))
  transparencyLabel2points = as.numeric(settings_labeling_params_percentile)/100

##PBI_PARAM: size of point marker
#Type: numeric, Default:0.75, Range:[0,1], PossibleValues:NA, Remarks: NA
pointMarkerSize = 1
if(exists("settings_viz_params_weight"))
  pointMarkerSize = as.numeric(settings_viz_params_weight)/10

##PBI_PARAM: use up to maxLenLabel2points for points labels (only first column is used)
#Type: positive integer, Default:4, Range:[1,100], PossibleValues:NA, Remarks: NA
maxLenLabel2points = 4
if(exists("settings_labeling_params_maxLenPointLabel"))
  maxLenLabel2points = max(1,min(100,settings_labeling_params_maxLenPointLabel))

##PBI_PARAM: show only each skipLabel2points for points labels
#Type: positive number,can be float, Default:1, Range:[1,100], PossibleValues:NA, Remarks: NA
skipLabel2points = 1
if(exists("settings_labeling_params_percentile1"))
{
  skipLabel2points = 100/as.numeric(settings_labeling_params_percentile1) 
  skipLabel2points = max(skipLabel2points,1)
}

##PBI_PARAM: pallete type for color of clusters
#Type: string , Default:"rainbow",  Range:NA, PossibleValues:"rainbow", "terrain" etc,  Remarks: NA
palleteType = "rainbow"
if(exists("settings_legend_params_palleteType"))
  palleteType = settings_legend_params_palleteType 



###############Library Declarations###############

libraryRequireInstall = function(packageName, ...)
{
  if(!require(packageName, character.only = TRUE)) 
    warning(paste("*** The package: '", packageName, "' was not installed ***",sep=""))
}

#Remark: most of the packages are required because of several automatic cluster number algorithms 
libraryRequireInstall("nloptr") 
libraryRequireInstall("seriation") 
libraryRequireInstall("NbClust")
libraryRequireInstall("cluster")
libraryRequireInstall("car")
libraryRequireInstall("scales")
libraryRequireInstall("fpc")
libraryRequireInstall("mclust")
libraryRequireInstall("apcluster")
libraryRequireInstall("vegan")

###############Internal parameters definitions#################

##PBI_PARAM: the random number generator (RNG) state for random number generation 
#Type: numeric, Default:42, Range:NA, PossibleValues:NA, Remarks: NA
randSeed = 42


##PBI_PARAM: minimum required samples (rows in data table)
#Type: positive integer, Default:8, Range:[5,100], PossibleValues:NA, Remarks: NA
minSamplesToRun = 8

##PBI_PARAM: maximum samples to use inside autoNumClusters function
#Type: positive integer, Default:5000, Range:[100,10000], PossibleValues:NA, Remarks: NA
maxSamples4autoNumClusters = 5000 



##PBI_PARAM: insignificant principle component threshold
# If PCA is applied all dimensions, that explain less than insigPC percentage of variance are removed
#Type: positive numeric, Default:0.05, Range:[0,1], PossibleValues:NA, Remarks: NA
insigPC = 0.05

##PBI_PARAM: font size for legend
#Type: positive , Default:1, Range:[0,5], PossibleValues:NA, Remarks: NA
legendTextSize = 1

###############Internal functions definitions#################

# validate if plot space is enough for legend to appear
validateIfToShowLegend = function(numClust, textSize)
{
  ppp = par()$din
  return((2.5*textSize) < ppp[1]) 
}

# cut long text on plot
cutStr2Show = function(strText, strCex = 0.8, abbrTo = 100, isH = TRUE, maxChar = 0, partAvailable = 1)
{
  # strText = text to modify 
  # strCex = font size 
  # abbrTo = very long string will be abbreviated to "abbrTo" characters
  # isH = "is horizontal" ?
  # maxChar = text smaller than maxChar is replaced by NULL
  # partAvailable = which portion of window is available for text, in [0,1]
  
  if(is.null(strText))
    return (NULL)
  
  SCL = 0.094*strCex
  pardin = par()$din
  gStand = partAvailable*(isH*pardin[1]+(1-isH)*pardin[2]) /SCL
  
  # if very very long abbreviate
  if(nchar(strText)>abbrTo && nchar(strText)> 1)
    strText = abbreviate(strText, abbrTo)
  
  # if looooooong convert to lo...
  if(nchar(strText)>round(gStand) && nchar(strText)> 1)
    strText = paste(substring(strText,1,floor(gStand)),"...",sep="")
  
  # if shorter than maxChar remove 
  if(gStand<=maxChar)
    strText = NULL
  
  return(strText) 
}



#partition all autoNumCluster methods in three categories, see autoNumClusters 
listMethods = list(fast = c(1:5), moderate = c(1:11), slow = c(1:15))


#verify if the column is numeric and non-constant
correctColumn<-function(someColumn){is.numeric(someColumn)&&length(unique(someColumn))>1}

# Within Groups Sum of Squares function
calcWSS<-function(mydata, maxClust = maxClusters)
{
  wss <- (nrow(mydata)-1)*sum(apply(mydata, 2, var))
  for (i in 2:maxClust) wss[i] <- sum(kmeans(mydata, nstart = 3,
                                             centers = i)$withinss)
  return(wss)
}

#plot convex hull
plotCH<-function(xcoord, ycoord, lcolor){
  hpts <- chull(x = xcoord, y = ycoord)
  hpts <- c(hpts, hpts[1])
  lines(xcoord[hpts], ycoord[hpts], col = lcolor, lty = 3)
}  

#get datapoints closest to centers 
getDelegates<-function(orig_data, clean_data, cluster_centers)
{
  nc<-nrow(cluster_centers)
  dc<-ncol(cluster_centers)# dimension 
  nr<-nrow(clean_data)
  delegates<-NULL
  for(clustr in c(1:nc))
  {
    B<-matrix(rep(cluster_centers[clustr,], times = nr), nrow = nr, ncol = dc, byrow = TRUE)
    D<-clean_data[,c(1:dc)]-B
    ed<-apply(D^2, 1, sum)
    delegates<-rbind(delegates, orig_data[which.min(ed),])
  }
  return(delegates) 
}

#return TRUE if covariance matrix of mydata is nearly-singular
checkSingularity = function(mydata, thresh = 1e-10)
{
  covMat<-cov(mydata)
  svd4cov<-svd(covMat)
  ratio<-min(svd4cov$d)/max(svd4cov$d) 
  return(ratio<thresh)
}


# get the minimum number of clusters that explain at least 90% (part=0.9) of variance 
# "wss" parameter stands for the "Within Groups Sum of Squares"
explainWSS = function (wss, part = 0.90)
{
  wss <- wss/max(wss)
  min(c(seq(1, length.out = length(wss))[wss < (1-part)], length(wss)))
}


#sum of square errors for linear fit 
SSE = function(x, y) {sum( abs( lm( formula = y ~ x, data = data.frame(x = x, y = y) )$residuals )^2)}

# find knee point which corresponds to best cut-point of two linear fits
findKnee <- function( inX, inY )
{
  orderX = order( inX )
  inX = inX[orderX];inY = inY[orderX]
  
  L = length(inX)
  if(L<=3) 
    return(c(inX[2], inY[2]))
  
  resV = rep(Inf, L)
  first = 1
  last = L
  
  for (i in (first+1):(last-1))
  {
    x = inX[first:i]
    y = inY[first:i]
    resid = SSE(x, y)
    x = inX[i:last]
    y = inY[i:last]
    resV[i]=resid+SSE(x, y)
  }
  
  mi = which.min(resV)
  return( c(inX[mi], inY[mi]) )
}



# find number of clusters using several methods. The result is a median of the recommendations of all methods 
autoNumClusters<-function(mydata, minClus = 2, maxClus = 15, methods = c(1:5), maxSamples = maxSamples4autoNumClusters)
{
  #fast methods: explainWSS,robustElbow, "db", "ratkowsky", "ball"   (1,2,3,4,5)
  #moderate methods: "ccc", "scott", "marriot", "trcovw", "friedman", "rubin" (6,7,8,9,10,11)
  #slow methods: "sdindex",fpc, Calinski, apcluster (12,13,14,15)
  
  allInds<- 1:15
  allMethods<-c("explainWSS", "robustElbow", "db", "ratkowsky", "ball", 
                "ccc", "scott", "marriot", "trcovw", "friedman", "rubin", 
                "sdindex", "fpc", "Calinski", "apcluster")
  
  names(allInds) = allMethods
  
  #if dataset is too large subsample it
  if(nrow(mydata)>maxSamples)
    mydata<-mydata[sample(1:nrow(mydata), maxSamples, replace = FALSE), ]
  
  maxClus<-min(min(maxClus, nrow(mydata)-2), ceiling(nrow(mydata)/2.5))
  wss<-calcWSS(mydata, maxClus)
  recommend<-NULL
  
  
  nbclustIndexes<-c("db", "ratkowsky", "ball", "ccc", "scott", "marriot", "trcovw", "friedman", "rubin", "sdindex")
  singularityRisk<-allInds[c("ccc", "scott", "marriot", "trcovw", "friedman", "rubin","Calinski")]
 
  
  #remove methods that crash because of singularity (if singlarity found)
  if(length(intersect(methods, singularityRisk)) )
  {
    if(checkSingularity(mydata))
    {
      methods<-setdiff(methods, singularityRisk)
      if(!length(methods))
        methods = allInds["db"] 
    }
  }
  
  #1: explainWSS
  if(sum(methods == allInds["explainWSS"]))
  {
    tempNC<-explainWSS(wss)
    recommend<-rbind(recommend, tempNC)
  }
  #2: robustElbow
  if(sum(methods == allInds["robustElbow"]))
  {
  
    tempNC<-findKnee(1:length(wss),wss)[1]
    recommend<-rbind(recommend, tempNC)
  }
  
  #3: fpc
  if(sum(methods == allInds["fpc"]))
  {
    asw <- numeric(maxClus)
    for (k in 2:maxClus)
      asw[[k]] <- pamk(mydata, k) $ pamobject $ silinfo $ avg.width
    tempNC <- which.max(asw)
    recommend<-rbind(recommend, tempNC)
  }
  #4: calinski
  if(sum(methods == allInds["Calinski"]))
  {
    
    fit <- cascadeKM(scale(mydata, center = TRUE, scale = TRUE), 1, maxClus, iter = 100)
    tempNC<-as.numeric(which.max(fit$results[2, ]))
    recommend<-rbind(recommend, tempNC)
  }
  #5: apcluster
  if(sum(methods == allInds["apcluster"]))
  {
    d.apclus <- apcluster(negDistMat(r = 2), mydata)
    tempNC<-length(d.apclus@clusters)
    recommend<-rbind(recommend, tempNC)
  }
  
  
  mydata1 = mydata
  for (m in allInds[nbclustIndexes])
    if(sum(methods == m) && maxClus>=4)
    {
      if(sum(m == singularityRisk))
        mydata1 = as.data.frame(jitter(as.matrix(mydata)), factor = 0.2)
      nb <- NbClust(mydata1, distance = "euclidean", 
                    min.nc = 2, max.nc = maxClus, method = "kmean", 
                    index  = names(allInds[m]) )
      tempNC<-nb$Best.nc[1]
      recommend<-rbind(recommend, tempNC)
    }
  recommend<-round(median(recommend, na.rm = TRUE))
  
}

myPallete = function(n=100,palleteType = "rainbow")
{
  mp = rainbow(n)
  
  if(palleteType == "heat")
    mp = heat.colors(n)
  
  if(palleteType == "terrain")
    mp = terrain.colors(n)
  
  if(palleteType == "topo")
    mp = topo.colors(n)
  
  if(palleteType == "cm")
    mp = cm.colors(n+1)[-1] #remove white
  
  if(palleteType == "gray")
    mp = gray(0:n/ n)
  
  return(mp)
  
}


###############Upfront input correctness validations (where possible)#################

pbiWarning<-NULL

if(is.null(numOfClusters)||!is.numeric(numOfClusters)||numOfClusters<1||numOfClusters>maxClusters)
  numOfClusters = NULL

dataset <- na.omit(dataset) # deletion of missing
orig_dataset <- dataset #used later for delegates

# verify correctness of dataset
useColumns<-sapply(dataset, correctColumn)

if(showWarnings && sum(useColumns[-1])<ncol(dataset)-1)
  pbiWarning<-"At least one of the columns was not numeric, or constant"

#exclude incopmatible columns
dataset<-as.data.frame(dataset[,useColumns])
nc<-ncol(dataset)
nr<-nrow(dataset)
maxClusters<-min(maxClusters, nr-1)
checkDimiensionality<-TRUE
if(nc<2 || nr<minSamplesToRun)
{
  checkDimiensionality<-FALSE
  if(showWarnings)
    pbiWarning<-paste(pbiWarning, " Not enough input dimensions");
}

##############Main Visualization script###########


set.seed(randSeed)

if(!checkDimiensionality)
{
  plot.new()
}else{
  if(scaleData)
  {
    dataset<-as.data.frame(scale(dataset))
    names(dataset) = paste(names(dataset), "scaled", sep = ".")
  }
  if(applyPCA)
  {
    dataset.pca <- prcomp(dataset, center =  TRUE, scale =  F) 
    pExplained<-dataset.pca$sdev^2/sum(dataset.pca$sdev^2)
    flags<-(pExplained>insigPC); flags[1:2] = TRUE #at least 2 dimensions
    dataset = as.data.frame(dataset.pca$x[, flags])#reduce dimensions with less than 5% variance
  }
  
  if(is.null(numOfClusters))
  {
    numClustersMethodsVector = listMethods [[ numClustersMethods ]]
    numOfClusters<-autoNumClusters(dataset, methods = numClustersMethodsVector)
    numOfClusters <- min(max(numOfClusters,minClusters),maxClusters)
  }
  # KMEANS with known numberOfClusters
  cl <- kmeans(dataset, centers = numOfClusters, iter.max = iter.max, nstart = nstart)
  
  numOfClusters = length(unique(cl$cluster))
  drawColors<-myPallete(numOfClusters,palleteType = palleteType)
 
  # visualize first two coordinates 
  if(drawPoints) colpoints = drawColors[cl$cluster] else colpoints = NULL 
  
  # in case of legend extend xlim to the right by 20%
  xrange = range( dataset[, 1] )
  drange = xrange[2] - xrange[1]
  xlim = c(xrange[1] - 0.01*drange, xrange[2] + 0.01*drange + drange*0.20*addLegend)
  
 
  
  plot(dataset[, 1], dataset[, 2], col = alpha(colpoints, pointTransparency), pch = 19,
       xlab = cutStr2Show(names(dataset)[1], strCex =1.1, isH = TRUE), 
       ylab = cutStr2Show(names(dataset)[2], strCex =1.1, isH = FALSE), 
       xlim = xlim, cex = pointMarkerSize)
  
  leg<-paste("Cluster " ,seq(1, length.out  = numOfClusters))
  pc<-rep(19, numOfClusters)
  
  if(drawCenters)
  {
    points(cl$centers, pch = 7, col = drawColors)
    leg<-cbind(leg, paste("Cluster center " , seq(1, length.out  = numOfClusters)))
    pc<-cbind(pc, rep(7, numOfClusters))
  }
  
  if(drawEllipse)
  {
    for(clustr in c(1:numOfClusters))
    {
      iii<-(cl$cluster == clustr)
      if(sum(iii)>2)
        dataEllipse(dataset[iii, 1], dataset[iii, 2], add = T, plot.points = F, levels = 0.85, col = drawColors[clustr], lwd = 1, 
                    fill = TRUE, fill.alpha = 0.075, center.pch = NULL)
    }
  }
  
  if(drawConvexHull)
  {
    for(clustr in c(1:numOfClusters))
    {
      iii<-(cl$cluster == clustr)
      if(sum(iii)>2)
        plotCH(dataset[iii, 1], dataset[iii, 2],lcolor = drawColors[clustr])
    }
  }
  
  
  if(addLabel2clusterDelegate)
  {
    clean_data = dataset
    cluster_centers = (cl$centers)
    deleg <- getDelegates(orig_dataset, dataset, cl$centers)
    delegateText = abbreviate(apply(deleg, 1, toString), maxLenDelegate)
    delegateText = sapply(delegateText, cutStr2Show, strCex = delegateCex, partAvailable = 0.75)
      
    text(cl$centers[, c(1, 2)], 
         delegateText,
         col = "black", 
         cex = delegateCex)
  }
  
  if(addLabel2points)
  {
    iii=sample(1:nrow(dataset),max(1,floor(nrow(dataset)/skipLabel2points)))
    text(x = dataset[iii, 1], y = dataset[iii, 2], labels = abbreviate(orig_dataset[iii,1],maxLenLabel2points),
         col = alpha(colpoints[iii], transparencyLabel2points), cex = cexLabel2points)
  }
  
  if(addLegend && validateIfToShowLegend(numClust = numOfClusters, textSize = legendTextSize ))
    legend("topright", legend = leg, pch = pc, col = alpha(drawColors, 1), cex = legendTextSize)
  
}

if(showWarnings && !is.null(pbiWarning))
{
  pbiWarning = cutStr2Show(pbiWarning, strCex = 0.8)
  title(main = NULL, sub = pbiWarning, outer = FALSE, col.sub = "gray50", cex.sub = 0.8)
}