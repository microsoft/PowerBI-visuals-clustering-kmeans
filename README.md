# PowerBI-visuals-clustering-kmeans
R-powered custom visual. Implements k-means clustering

![k-means clustering screenshot](https://az158878.vo.msecnd.net/marketing/Partner_21474836617/Product_42949680600/Asset_3d67d8ac-096b-40e7-821e-d51e13a75511/Clusteringscreenshot2.png)
# Overview
Everyone is trying to make sense of, and extract value from, their data. In the real world, data is often not easy to separate, and patterns are not usually obvious. Clustering helps you find similarity groups in your data and it is one of the most common tasks in the Data Science; it provides analysts the ability to achieve better results for initiatives and understand customers and processes at a much deeper level than a human can achieve alone.

This visual uses a well known k-means clustering algorithm. You can control the algorithm parameters and the visual attributes to suit your needs.

Here is how it works:
* Define the fields to be used in clustering (two or more numerical variables)
* Optionally, provide the labels to be shown on top of each observation
* If the dimensionality of data is higher than two, consider data preprocessing
* One of the most challenging tasks in clustering is defining the number of output clusters. To facilitate this task we provide both automatic and manual options for the control.
* When you are sattisfied with clustering output, use numerous formatting controls to refine the visual apperance of the plot
* If you are the advanced user, control the inner parameters of k-means clustering algorithm

R package dependencies(auto-installed): nloptr, seriation, pbkrtest,NbClust, cluster, car, scales, fpc, mclust, apcluster, vegan

Supports R versions: R 3.3.1, R 3.3.0, MRO 3.3.1, MRO 3.3.0, MRO 3.2.2

See also [Clustering chart at Microsoft Office store](https://store.office.com/en-us/app.aspx?assetid=WA104380861&sourcecorrid=7d90c605-7d53-45f5-a5c4-0703fe7b6e29&searchapppos=0&ui=en-US&rs=en-US&ad=US&appredirect=false)