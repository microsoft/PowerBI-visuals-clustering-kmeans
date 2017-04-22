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


source('./r_files/flatten_HTML.r')


#DEBUG 
fileRda = "C:/Users/boefraty/projects/PBI/R/tempData.Rda"
if(file.exists(dirname(fileRda)))
{
  if(Sys.getenv("RSTUDIO")!="")
    load(file= fileRda) 
  else
    save(list = ls(all.names = TRUE), file=fileRda)
}


options(warn = -1)

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

# if(exists("settings_prepocessing_params_show") && settings_prepocessing_params_show == FALSE)
#   rm(list= ls(pattern = "settings_prepocessing_params_"))
# if(exists("settings_clusterNum_params_show") && settings_clusterNum_params_show == FALSE)
#   rm(list= ls(pattern = "settings_clusterNum_params_"))
# if(exists("settings_viz_params_show") && settings_viz_params_show == FALSE)
#   rm(list= ls(pattern = "settings_viz_params_"))

# if(exists("settings_labeling_params_show") && settings_labeling_params_show == FALSE)
#   rm(list= ls(pattern = "settings_labeling_params_"))
# if(exists("settings_representative_params_show") && settings_representative_params_show == FALSE)
#   rm(list= ls(pattern = "settings_representative_params_"))
# if(exists("settings_legend_params_show") && settings_legend_params_show == FALSE)
#   rm(list= ls(pattern = "settings_legend_params_"))
# if(exists("settings_additional_params_show") && settings_additional_params_show == FALSE)
#   rm(list= ls(pattern = "settings_additional_params_"))



##PBI_PARAM: Should warnings text be displayed?
#Type:logical, Default:FALSE, Range:NA, PossibleValues:NA, Remarks: NA
showWarnings = FALSE 
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


##PBI_PARAM: Sparsify dense regions of the scatter plot? Recommended for overcoming the overdraw and memory issues
#Type:logical, Default:TRUE, Range:NA, PossibleValues:NA, Remarks: NA
sparsify = TRUE 
if(exists("settings_additional_params_sparsify"))
  showWarnings = settings_additional_params_sparsify


##PBI_PARAM: pallete type for color of clusters
#Type: string , Default:"rainbow",  Range:NA, PossibleValues:"rainbow", "terrain" etc,  Remarks: NA
palleteType = "qPBI"
if(exists("settings_legend_params_palleteType"))
  palleteType = settings_legend_params_palleteType 

#PBI_PARAM Size of labels on axes
sizeLabel = 12

#PBI_PARAM Size of warnings font
sizeWarn = 11

#PBI_PARAM Size of ticks on axes 
sizeTicks = 8

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
libraryRequireInstall("Redmonder")



############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly")
####################################################

libraryRequireInstall("ggplot2")

###############Internal parameters definitions#################

##PBI_PARAM: the random number generator (RNG) state for random number generation 
#Type: numeric, Default:42, Range:NA, PossibleValues:NA, Remarks: NA
randSeed = 42


##PBI_PARAM: minimum required samples (rows in data table)
#Type: positive integer, Default:8, Range:[5,100], PossibleValues:NA, Remarks: NA
minSamplesToRun = 12

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

#plot CH in ggplot
ggPlotCH = function (xcoord, ycoord, lcolor,ggp)
{
  
  hpts <- chull(x = xcoord, y = ycoord)
  hpts <- c(hpts, hpts[1])
  
  x = as.numeric(xcoord[hpts])
  y = as.numeric(ycoord[hpts])
  D = data.frame(x = x, y = y)
  
  ggp <- ggp + geom_path(data = D, mapping = aes(x = x, y = y),colour = lcolor,  inherit.aes = FALSE, show.legend = FALSE)
  return(ggp)
  
  
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
    mp = gray(1:n/ n)
  
  if(palleteType == "qPBI" && n <= 10)
    mp = redmonder.pal(n,"qPBI")
  
  if(palleteType == "qMSOStd"  && n <= 10)
    mp = redmonder.pal(n,"qMSOStd")
  
  return(mp)
  
}


#ggplot points
ggplotPoints = function(dfPoints, xla = "X", yla = "Y", with_ellipse = TRUE)
{
  
  dfPoints= dfPoints[order(dfPoints$labels),]
  
  gg1 = ggplot(dfPoints, aes(x = xx,y = yy, colour = labels, labels = labels, alpha = labels, size = labels, shape = labels))
  
  gg1 = gg1 + geom_point() 
  
  
  if(with_ellipse)
    gg1 = gg1 + stat_ellipse(level = 0.75, show.legend = FALSE, size = 1,linetype = 2)
  
  
  uniqueInd = !duplicated(dfPoints$labels) 
  
  
  gg1 = gg1 + scale_colour_manual(name = "",
                                  labels = dfPoints$labels[uniqueInd],
                                  values = dfPoints$col[uniqueInd], 
                                  breaks = dfPoints$labels[uniqueInd]) 
  
  gg1 = gg1 + scale_shape_manual(name = "",
                                 labels = dfPoints$labels[uniqueInd],
                                 values = dfPoints$shape[uniqueInd], 
                                 breaks = dfPoints$labels[uniqueInd]) 
  
  gg1 = gg1 + scale_size_manual(name = "",
                                labels = dfPoints$labels[uniqueInd],
                                values = dfPoints$size[uniqueInd], 
                                breaks = dfPoints$labels[uniqueInd]) 
  
  gg1 = gg1 + scale_alpha_manual(name = "",
                                 labels = dfPoints$labels[uniqueInd],
                                 values = dfPoints$alpha[uniqueInd], 
                                 breaks = dfPoints$labels[uniqueInd])   
  
  
  gg1 = gg1 + xlab(xla) + ylab(yla)
  
  print(gg1)
  
  
  return(gg1)
  
}


#ggplot points
ggplotPoints1 = function(dfPoints, xla = "X", yla = "Y")
{
  dfPoints= dfPoints[order(dfPoints$labels),]
  
  gg1 = ggplot()
  gg1 = gg1 + geom_point(data = dfPoints, mapping = aes(x = xx,y = yy, colour = labels, labels = labels, alpha = labels, size = labels, shape = labels)) 
  
  
  uniqueInd = !duplicated(dfPoints$labels) 
  
  
  gg1 = gg1 + scale_colour_manual(name = "",
                                  labels = dfPoints$labels[uniqueInd],
                                  values = dfPoints$col[uniqueInd], 
                                  breaks = dfPoints$labels[uniqueInd]) 
  
  gg1 = gg1 + scale_shape_manual(name = "",
                                 labels = dfPoints$labels[uniqueInd],
                                 values = dfPoints$shape[uniqueInd], 
                                 breaks = dfPoints$labels[uniqueInd]) 
  
  gg1 = gg1 + scale_size_manual(name = "",
                                labels = dfPoints$labels[uniqueInd],
                                values = dfPoints$size[uniqueInd], 
                                breaks = dfPoints$labels[uniqueInd]) 
  
  gg1 = gg1 + scale_alpha_manual(name = "",
                                 labels = dfPoints$labels[uniqueInd],
                                 values = dfPoints$alpha[uniqueInd], 
                                 breaks = dfPoints$labels[uniqueInd])   
  
  
  
  gg1 = gg1 + xlab(xla) + ylab(yla)
  
  print(gg1)
  
  
  return(gg1)
  
}



UpdateTextInPlotlyMarkers = function(p,usePoints,orig_dataset,mapOrig2markers)
{
  # for each marker 
  allColNames = colnames(orig_dataset)
  usePointsIndexes = seq(1,nrow(orig_dataset))[usePoints]
  
  for (co in seq(1,ncol(orig_dataset)))
    orig_dataset[,co] = as.character(orig_dataset[,co])
  
  # layers in p sorted as 1,10,11,12,2,...
  charClusSort = as.numeric(sort(as.character(unique(mapOrig2markers$cluster))))
  clusSort = sort(unique(mapOrig2markers$cluster))
  
  for (pi in usePointsIndexes)
  {
    c1 = clusSort[charClusSort==mapOrig2markers$cluster[pi]]
    i1 = mapOrig2markers$map[pi]
    
    tempText = paste(allColNames, "=",orig_dataset[pi,], sep =" ", collapse = "<br>")
    
    p$x$data[[c1]]$text[i1] = tempText
    
    
  }
  
  return(p)
  
}


SparsifyMarkers <- function(p, usePoints, mapOrig2markers)
{
  for (cla in unique(mapOrig2markers$cluster))
  {
    iii = mapOrig2markers$map[mapOrig2markers$cluster == cla & !usePoints]
    if(length(iii))
    {
      p$x$data[[cla]]$text = p$x$data[[cla]]$text[-iii]
      p$x$data[[cla]]$x = p$x$data[[cla]]$x[-iii]
      p$x$data[[cla]]$y = p$x$data[[cla]]$y[-iii]
    }
  }
  
  
  
  
  return(p)
}


#randomly remove points from scatter if too many 
SparsifyScatter = function (xyDataFrame, numXstrips = 8, numYstrips = 8, minMaxPoints = c(3000,10000), minmaxInStrip =  c(900,9000), maxInCell = 300, remDuplicated = TRUE)
{
  
  N_big = N = nrow(xyDataFrame)
  usePoints = rep(TRUE,N)
  
  if(N <= minMaxPoints[1]) # do nothing
    return (usePoints)
  
  if(remDuplicated) # remove duplicated
  {
    usePoints = usePoints & (!duplicated(xyDataFrame))
    N = sum(usePoints)
  }
  
  if(N <= minMaxPoints[1]) # do nothing
    return (usePoints)
  
  rangeX = range(xyDataFrame[,1])
  rangeY = range(xyDataFrame[,2])
  
  gridX = seq(rangeX[1],rangeX[2], length.out = numXstrips + 1)
  gridY = seq(rangeY[1],rangeY[2], length.out = numYstrips + 1)
  
  #go cell by cell and sparsify 
  for (iX in seq(1,numXstrips))
  {
    smallRangeX = c(gridX[iX],gridX[iX+1])
    inStrip = xyDataFrame[,1]>= smallRangeX[1] & xyDataFrame[,1]<= smallRangeX[2] &  usePoints
    if(sum(inStrip) > minmaxInStrip[1])
      for (iY in seq(1,numYstrips))
      {
        smallRangeY = c(gridY[iY],gridY[iY+1])
        inCell = xyDataFrame[,2]>= smallRangeY[1] & xyDataFrame[,2]<= smallRangeY[2] &  inStrip
        if(sum(inCell) > maxInCell)
        {
          inCellIndexes = seq(1,N_big)[inCell]
          #randomly select maxInCell out of inCellIndexes
          iii = sample(inCellIndexes,size = sum(inCell) - maxInCell, replace = FALSE)
          usePoints[iii] = FALSE
        }
      }
    
  }
  N = sum(usePoints)
  
  #if by the end still too many points --> go on whole set  
  if(N > minMaxPoints[2])
  {
    inIndexes = seq(1,N_big)[usePoints]
    #randomly select minMaxPoints[2] out of inIndexes
    iii = sample(inIndexes,size = minMaxPoints[2], replace = FALSE)
    usePoints[-iii] = FALSE
    
  }
  
  return (usePoints)
  
}


goodPlotDimension = function(minWidthInch = 3,minHeightInch = 2.2)
{
  re = (par()$din[1] > minWidthInch) & (par()$din[2] > minHeightInch)
  return(re)
}



###############Upfront input correctness validations (where possible)#################

pbiWarning<-NULL

if(is.null(numOfClusters)||!is.numeric(numOfClusters)||numOfClusters<1||numOfClusters>maxClusters)
  numOfClusters = NULL

dataset <- na.omit(dataset) # deletion of missing
orig_dataset <- dataset #used later for delegates

# verify correctness of dataset
useColumns<-sapply(dataset, correctColumn)

 if(sum(useColumns[-1])<ncol(dataset)-1)
   pbiWarning<-cutStr2Show("At least one of the columns was not numeric, or constant",strCex = sizeWarn/6, partAvailable = 0.85)

#exclude incopmatible columns
dataset<-as.data.frame(dataset[,useColumns])
nc<-ncol(dataset)
nr<-nrow(dataset)
maxClusters<-min(maxClusters, nr-1)
checkDimiensionality = checkVisualSize = TRUE
if(nc<2 || nr<minSamplesToRun)
{
  showWarnings = TRUE
  checkDimiensionality<-FALSE
  pbiWarning2 <- cutStr2Show("Not enough input dimensions", strCex = sizeWarn/6, partAvailable = 0.85)
  pbiWarning <- paste(pbiWarning, "<br>", pbiWarning2);
}

#check if output window is large enough 
if(!goodPlotDimension(minWidthInch = 3,minHeightInch = 2.2))
{
  showWarnings = TRUE
  checkVisualSize<-FALSE
  pbiWarning2 <- cutStr2Show("Visual size is too small", strCex = sizeWarn/12, partAvailable = 0.85)
  pbiWarning <- paste(pbiWarning, "<br>", pbiWarning2);
  
}
#addLegend, check if output window is small turn off the legend 
if(!goodPlotDimension(minWidthInch = 5,minHeightInch = 3.5))
  addLegend = FALSE
  

##############Main Visualization script###########


set.seed(randSeed)

if(!checkDimiensionality || !checkVisualSize)
{ 
  gg = ggplot()    
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
  
  numOfClusters <- length(unique(cl$cluster))
  drawColors <- myPallete(numOfClusters,palleteType = palleteType)
  
  # visualize first two coordinates 
  #drawColors = c("red","green","blue")#TEMP
  
  if(drawPoints) colpoints = drawColors[cl$cluster] else colpoints = NULL 
  
  # # in case of legend extend xlim to the right by 20%
  # xrange = range( dataset[, 1] )
  # drange = xrange[2] - xrange[1]
  # xLim = c(xrange[1] - 0.01*drange, xrange[2] + 0.01*drange + drange*0.20*addLegend)
  
  xLab = cutStr2Show(names(dataset)[1], strCex = sizeLabel/6, isH = TRUE, partAvailable = 0.8)
  yLab = cutStr2Show(names(dataset)[2], strCex = sizeLabel/6, isH = FALSE, partAvailable = 0.8)
  
  
  
  
  myl = factor(colpoints)
  
  levels(myl)=seq(1,length(levels(myl)))
  #cl$cluster1 = as.numeric(myl)
  
  #myl = paste("Cluster ", myl,sep="")
  myl = paste("Cluster ", as.character(cl$cluster),sep="")
  
  names(colpoints) = myl
  
  NP = nrow(dataset)
  df_points = data.frame(xx = dataset[, 1], yy = dataset[, 2], shape = rep(19,NP), 
                         labels = myl, col = colpoints, alpha = rep(pointTransparency,NP), 
                         size = rep(pointMarkerSize*2.5,NP), stringsAsFactors = FALSE)
  
  if(drawEllipse)
    drawCenters = FALSE
  
  
  
  if(drawCenters)
  {
    # wrong centroids numbers
    
    
    df_centers = data.frame(xx = cl$centers[,1], yy = cl$centers[,2], shape = rep(7,numOfClusters), 
                            labels = paste("Cluster center " , seq(1, length.out  = numOfClusters)), col = drawColors, alpha = rep(1,numOfClusters), 
                            size = rep(pointMarkerSize*2.5,numOfClusters)) 
    
    df_points = rbind(df_points, df_centers)
  }
  
  
  
  gg = ggplotPoints(df_points,xLab,yLab, with_ellipse = drawEllipse)
  
  
  
  if(drawConvexHull)
  {
    for(clustr in c(1:numOfClusters))
    {
      iii<-(cl$cluster == clustr)
      if(sum(iii)>2)
        gg = ggPlotCH (dataset[iii, 1], dataset[iii, 2],lcolor = drawColors[clustr],gg)
    }
  }
  
  
  
  
  
  if(addLabel2clusterDelegate)
  {
    clean_data = dataset
    cluster_centers = (cl$centers)
    deleg <- getDelegates(orig_dataset, dataset, cl$centers)
    delegateText = abbreviate(apply(deleg, 1, toString), maxLenDelegate)
    delegateText = sapply(delegateText, cutStr2Show, strCex = delegateCex, partAvailable = 0.75)
    
    
    D = data.frame(xpos = cl$centers[, 1], ypos = cl$centers[, 2], alabels = delegateText, col = drawColors)
    
    gg = gg +
      annotate(geom="text", x=D$xpos, y=D$ypos, label = D$alabels, size = 3*delegateCex,
               colour = D$col)
    
    
  }
  
  if(addLabel2points && transparencyLabel2points > 0.05)
  {
    iii=sample(1:nrow(dataset),max(1,floor(nrow(dataset)/skipLabel2points)))
    alabels = abbreviate(orig_dataset[iii,1],maxLenLabel2points)
    
    D = data.frame(xpos = dataset[iii, 1], ypos = dataset[iii, 2], alabels = alabels, col = colpoints[iii], cex = rep(cexLabel2points,length(alabels)))
    
    gg = gg +
      annotate(geom="text", x=D$xpos, y=D$ypos, label = D$alabels, size = 3*cexLabel2points,
               alpha = transparencyLabel2points, colour = D$col)
    
    
    
  }
  
  gg = gg + theme_bw()
  
 
  
  
  
}

if(!showWarnings)
  pbiWarning = NULL

# if(!is.null(pbiWarning) && showWarnings)
# {
  #showWarnings = TRUE
  #pbiWarning = cutStr2Show(pbiWarning, strCex = 0.8)
  
  gg = gg + labs (title = pbiWarning, caption = NULL) + theme_bw() +
    theme(plot.title  = element_text(hjust = 0.5, size = 12),
          axis.title=element_text(size =  sizeLabel),
          axis.text=element_text(size =  sizeTicks),
          panel.border = element_blank())
  
  if(!addLegend)
    gg = gg +  theme(legend.position="none")
  
#}

# convert to plotly 
p <- plotly_build(gg)

if(is.null(pbiWarning))
{
  #map backward original dataset to p-object markers 
  mapOrig2markers = data.frame(cluster = as.numeric(cl$cluster), map =seq(1,nrow(orig_dataset)))
  
  for (cla in seq(1,numOfClusters))
  {
    temp2 = (cl$cluster== cla)
    mapOrig2markers$map[temp2] = seq(1,sum(temp2))
  }
  
  
  #sparsify original dataset  
  if(sparsify)
    usePoints = SparsifyScatter(dataset)
  else
    usePoints = SparsifyScatter(dataset,minMaxPoints = c(Inf,Inf))
  
  
  
  
  #update text: p$x$data[[1]]$text,..., p$x$data[[numCluster]]$text
  p <- UpdateTextInPlotlyMarkers(p,usePoints,orig_dataset,mapOrig2markers)
  
  
  # remove markers from dense regions (x,y,text is removed) to make html object smaller
  p <- SparsifyMarkers(p, usePoints, mapOrig2markers)
  
  
  # if centroids' text 
  if(drawCenters)
  {#layer 
    cluCh= sort(as.character(1:numOfClusters))
    for (i in seq(numOfClusters+1,2*numOfClusters))
      p$x$data[[i]]$text = paste("Cluster center ", cluCh[i - numOfClusters], sep ="")
  }
  
  
  # re-design plotly object, make sure no extra legend elements (like ellipses) 
  numLayers = length(p$x$data) 
  for (lay in seq(1,numLayers))
  {
    if(p$x$data[[lay]]$mode!= "markers")
      p$x$data[[lay]]$showlegend = FALSE
    
    if(p$x$data[[lay]]$mode == "lines")
      p$x$data[[lay]]$text = NULL
    
    if(p$x$data[[lay]]$mode == "text")
      p$x$data[[lay]]$hoverinfo = 'none'
  }
  
}
############# Create and save widget ###############


disabledButtonsList <- list('toImage', 'sendDataToCloud', 'zoom2d', 'pan', 'pan2d', 'select2d', 'lasso2d', 'hoverClosestCartesian', 'hoverCompareCartesian')
p$x$config$modeBarButtonsToRemove = disabledButtonsList
p <- config(p, staticPlot = FALSE, editable = FALSE, sendData = FALSE, showLink = FALSE,
            displaylogo = FALSE,  collaborate = FALSE, cloud=FALSE)

internalSaveWidget(p, 'out.html')

####################################################

#display in R studio
if(Sys.getenv("RSTUDIO")!="")
{print(p)
  print(gg)}

