# https://codeberg.org/Soloof/Iteridense
# License is LGPL-3.0-or-later (https://spdx.org/licenses/LGPL-3.0-or-later.html)
#
# author: Uwe Stöhr
#=
 This script demonstrates how Iteridense clustering is performed on different data sets.
 It also shows the clustering of the same datasets using the k-means and the DBSCAN algorithm
=#

# uncomment the following 2 lines on the first run to install missing packages
# using Pkg; Pkg.add("Clustering"); Pkg.add("CSV"); Pkg.add("DataFrames");
#  Pkg.add("Plots"); Pkg.add("PlotlyBase"); Pkg.add("PlotlyJS")
using Clustering, CSV, DataFrames, Plots

begin
# getting interactive plots
plotlyjs()
# setting default plot size
plotly(size= (512, 512))
# import the Iteridense library
include(joinpath(@__DIR__, "IteridenseLibrary.jl"))
using .IteridenseLibrary: Iteridense, PlotIteridenseHeatmap
end

# The data to be clustered are all in CSV files. The CSV files re expected to be in a subfolder
# called "datasets" of this Iteridense-Clustering.jl file.
begin
filePath = joinpath(@__DIR__, "datasets/SparseClustering.csv")
# its separator is a tabulator
if isfile(filePath)
    data = CSV.read(filePath, DataFrame; delim= '\t')
end
end

# In VS code you can show the data inline.
# For this uncomment this line.
#vscodedisplay(data)

begin
# extract names and values
dataLabels = String.(names(data))
dataMatrix = Matrix(data)
# initialize plot
DataPlot = Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2],
                        xlabel= dataLabels[1], ylabel= dataLabels[2],
                        title= "Unclustered Data", legend= false, markersize= 5)
end

# Iteridense clustering
ρ = 3.0
# perform the clustering
IteridenseResult = Iteridense(dataMatrix, density= ρ);
# list the different clustering results
IteridenseResult.numOfClusters
IteridenseResult.finalResolution
IteridenseResult.clusterDensities
IteridenseResult.clusterSizes
# plot the count map
PlotIteridenseHeatmap(dataMatrix, IteridenseResult.finalResolution)
# plot clustering result
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= dataLabels[1], ylabel= dataLabels[2],
                title= "Iteridense ρ = $ρ",
                group= IteridenseResult.assignments, markersize= 5)

# for comparison k-means clustering for 2 expected clusters
# for k-means, cluster "0" is a real cluster while for Iteridense and DBSCAN is not
begin
result = Clustering.kmeans(dataMatrix', 2; maxiter= 20, display= :iter)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
clusterCenter = result.centers
# plot results
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= dataLabels[1], ylabel= dataLabels[2],
                title= "k-means k = 2", group= assign, markersize= 5)
end

# DBSCAN clustering
begin
ε = 10.0
result = Clustering.dbscan(dataMatrix', ε, min_neighbors= 6, min_cluster_size= 3)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
# plot results
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= dataLabels[1], ylabel= dataLabels[2],
                title= "DBSCAN ε = $ε", group= assign, markersize= 5)
end
# By setting min_neighbors to 6, one might get the "correct" result but be careful,
# because it "looks nice" to a human does not mean the clustering is sensible.
# DBSCAN is prone for human bias to adjust the algorithm parameters to make the result
# looking nice. For 2D data then a human could do the clustering directly by looking at the plot.


#---------------------------------------------
# Now clustering just 3 points as an extreme test case.
begin
dataMatrix = [[19, 23, 57] [42, 39, 34]]
# initialize plot
DataPlot = Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2],
                        xlabel= dataLabels[1], ylabel= dataLabels[2],
                        title= "Unclustered Data", legend= false, markersize= 5)
end

# Iteridense clustering
ρ = 2.0
IteridenseResult = Iteridense(dataMatrix, density= ρ);
IteridenseResult.numOfClusters
IteridenseResult.finalResolution
IteridenseResult.clusterDensities
IteridenseResult.clusterSizes
PlotIteridenseHeatmap(dataMatrix, IteridenseResult.finalResolution)
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= dataLabels[1], ylabel= dataLabels[2],
                title= "Iteridense ρ = $ρ",
                group= IteridenseResult.assignments, markersize= 5)


#---------------------------------------------
# next dataset: two moon-shape clusters
begin
filePath = joinpath(@__DIR__, "datasets/NoisyMoons.csv")
if isfile(filePath)
    # its separator is a comma
    data = CSV.read(filePath, DataFrame; delim= ',')
end
dataMatrix = Matrix(data)
DataPlot = Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
                        legend= false)
end

# if one has no idea for a sensible setting, follow the the Iteridense path:
# 1. start with a low ρ, e.g. 2.0
# 2. increase ρ
# in our case at ρ = 2.2 we get 2 clusters
ρ = 2.2
IteridenseResult = Iteridense(dataMatrix, density= ρ,
# uncomment the next line to see the effect of the noDiagonal option
# set then also ρ = 5.0 to see the effect
                                        #noDiagonals= true,
);
IteridenseResult.numOfClusters
IteridenseResult.finalResolution
IteridenseResult.clusterDensities
IteridenseResult.clusterSizes
PlotIteridenseHeatmap(dataMatrix, IteridenseResult.finalResolution)
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
                title= "Iteridense ρ = $ρ",
                group= IteridenseResult.assignments, markersize= 5)

# k-means clustering
begin
result = Clustering.kmeans(dataMatrix', 2; maxiter= 20, display= :iter)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
clusterCenter = result.centers
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
        title= "k-means k = 2", group= assign, markersize= 5)
end

# DBSCAN clustering
begin
result = Clustering.dbscan(dataMatrix', 0.10, min_neighbors= 3, min_cluster_size= 10)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
        title= "DBSCAN ε = 0.10", group= assign, markersize= 5)
end


#---------------------------------------------
# next dataset: two concentric circles
begin
filePath = joinpath(@__DIR__, "datasets/NoisyCircles.csv")
if isfile(filePath)
    data = CSV.read(filePath, DataFrame; delim= ',')
end
dataMatrix = Matrix(data)
DataPlot = Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
                        legend= false)
end

# We can use that we know there are 2 circles and thus set minClusters.
# For this we must useClusters set to true. useClusters acts as a switch between
# the 2 ways of clustering Iteridense provides.
IteridenseResult = Iteridense(dataMatrix, useClusters= true, minClusters= 2);
IteridenseResult.finalResolution
IteridenseResult.clusterDensities
IteridenseResult.clusterSizes
#PlotIteridenseHeatmap(dataMatrix, IteridenseResult.finalResolution)
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
                title= "Iteridense MinClusters = 2",
                group= IteridenseResult.assignments, markersize= 5)

# To measure the computation time, we run 10000 times the clustering.
# By setting startResolution to a higher values the computation speeds up a lot
# because less grid iterations are run. Setting startResolution to 16 leads actually
# to only a single grid run.
# By using the option noDiagonals the computation is of course faster.
@elapsed for i in 1:10000
IteridenseResult = Iteridense(dataMatrix, density= ρ,
                                useClusters= true, minClusters= 2,
                                startResolution= 2,
                                #noDiagonals= true
);
end

# k-means clustering
begin
result = Clustering.kmeans(dataMatrix', 2; maxiter= 20, display= :iter)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
clusterCenter = result.centers
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
        title= "k-means k = 2", group= assign, markersize= 5)
end

# for comparison to Iteridense, 10000 runs with DBSCAN
@elapsed for i in 1:10000
result = Clustering.dbscan(dataMatrix', 0.10, min_neighbors= 3, min_cluster_size= 10)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
end
# plot the results
begin
result = Clustering.dbscan(dataMatrix', 0.10, min_neighbors= 3, min_cluster_size= 10)
assign = Clustering.assignments(result)
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
        title= "DBSCAN ε = 0.10", group= assign, markersize= 5)
end


#---------------------------------------------
# next dataset: 3 distorted blobs
begin
filePath = joinpath(@__DIR__, "datasets/NoisyAnisotrope.csv")
if isfile(filePath)
    data = CSV.read(filePath, DataFrame; delim= ',')
end
dataMatrix = Matrix(data)
DataPlot = Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
                        legend= false)
end

# Try the two different ways, either specify ρ or minClusters.
# For ρ until 6.8 there is only one cluster, increase it to get more clusters.
ρ = 6.8
IteridenseResult = Iteridense(dataMatrix, density= ρ, minClusterSize= 6,
                                        #useClusters= true, minClusters= 3
);
IteridenseResult.finalResolution
IteridenseResult.clusterDensities
#PlotIteridenseHeatmap(dataMatrix, IteridenseResult.finalResolution)
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel="x", ylabel= "y",
                title= "Iteridense ρ = $ρ",
                group= IteridenseResult.assignments, markersize= 5)

# k-means clustering
begin
result = Clustering.kmeans(dataMatrix', 3; maxiter= 20, display= :iter)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
clusterCenter = result.centers
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
        title= "k-means k = 3", group = assign, markersize= 5)
end

# DBSCAN clustering
begin
# we generate a series of plots with ε ∈ [0.05, 0.3] in steps of 0.05
for ε in 0.05:0.05:0.3
    result = Clustering.dbscan(dataMatrix', ε, min_neighbors= 3, min_cluster_size= 6)
    assign = Clustering.assignments(result)
    clusterCounts = Clustering.counts(result)
    DBSCANPlots = Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
                    title= "DBSCAN ε = $ε", group= assign, markersize= 5)
    display(DBSCANPlots)
end
end
# So there is no ε at which DBSCAN finds exactly 3 clusters. With the setting
# ε = 0.25 and min_cluster_size = 10 one will get 3 clusters but this is hard to guess.
# For high-dimensional data it would be even harder because one can then not plot.


#---------------------------------------------
# next dataset: 3 blobs, one has low density
begin
filePath = joinpath(@__DIR__, "datasets/NoisyBlobsVaried.csv")
if isfile(filePath)
    data = CSV.read(filePath, DataFrame; delim= ',')
end
dataMatrix = Matrix(data)
DataPlot = Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
                        legend= false)
end

# Hereby you can decide what result you want to have:
# A. 3 clusters, 2 with high-density, one with low density
# B. 2 clusters with each high-density
# For A. set minClusters to 3
# For B. start with a low ρ, increase it and also increase minClusterSize.
#        For example set minClusterSize to a high value, e.g. to 50. This is easy to guess since
#        there are 1500 data points and every cluster will roughly contain a third of all points.
#        You get the desired result for ρ = 3.8 or higher
ρ = 3.8
IteridenseResult = Iteridense(dataMatrix, density= ρ,
                                        useClusters= true, minClusters= 3,
                                        minClusterSize= 20
);
IteridenseResult.finalResolution
IteridenseResult.clusterDensities
IteridenseResult.clusterSizes
#PlotIteridenseHeatmap(dataMatrix, IteridenseResult.finalResolution)
densityPlots = Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
                    title= "Iteridense",
                    group= IteridenseResult.assignments, markersize= 5)

# k-means clustering
begin
result = Clustering.kmeans(dataMatrix', 3; maxiter= 20, display= :iter)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
clusterCenter = result.centers
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
        title= "k-means k = 3", group= assign, markersize= 5)
end

# DBSCAN clustering
begin
# we create a series of plots to find a suitable ε
for ε in 0.1:0.1:0.7
    result = Clustering.dbscan(dataMatrix', ε, min_neighbors= 3, min_cluster_size= 20)
    assign = Clustering.assignments(result)
    clusterCounts = Clustering.counts(result)
    DBSCANPlots = Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
                    title= "DBSCAN ε = $ε", group= assign, markersize= 5)
    display(DBSCANPlots)
end
end


#---------------------------------------------
# next dataset: random noise
begin
filePath = joinpath(@__DIR__, "datasets/NoisyRandom.csv")
if isfile(filePath)
    data = CSV.read(filePath, DataFrame; delim= ',')
end
dataMatrix = Matrix(data)
DataPlot = Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
                        legend= false)
end

# As the data is pure noise there should not be any clusters. However, you can set minClusterSize
# to a low value compares to the 1500 points in the dataset, for example to 10.
# By increasing ρ you can identify regions with higher density.
ρ = 3.0
IteridenseResult = Iteridense(dataMatrix, density= ρ, minClusterSize= 10);
IteridenseResult.numOfClusters
IteridenseResult.finalResolution
IteridenseResult.clusterSizes
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
                title= "Iteridense ρ = $ρ",
                group= IteridenseResult.assignments, markersize= 5)

# k-means will always "find" the amount of specified clusters
begin
result = Clustering.kmeans(dataMatrix', 3; maxiter= 20, display= :iter)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
clusterCenter = result.centers
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
        title= "k-means k = 3", group= assign, markersize= 5)
end

# DBSCAN cannot identify any clusters
begin
result = Clustering.dbscan(dataMatrix', 0.50, min_neighbors= 3, min_cluster_size= 10)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
Plots.scatter(dataMatrix[:, 1], dataMatrix[:, 2], xlabel= "x", ylabel= "y",
        title= "DBSCAN ε = 0.10", group= assign, markersize= 5)
end


#---------------------------------------------
# next dataset: Fisher's dataset of 3 iris flower types
begin
filePath = joinpath(@__DIR__, "datasets/Iris.csv")
if isfile(filePath)
    data = CSV.read(filePath, DataFrame; delim= ',')
end
#vscodedisplay(data)
# extract names and values
dataLabels = String.(names(data))
dataMatrix = Matrix(data)
# strip the first and last column and convert data type to float
inputMatrix = Float64.(dataMatrix[:, 2:end-1])
# plot data
DataPlot = Plots.scatter(inputMatrix[:, end-1], inputMatrix[:, end],
            xlabel= dataLabels[end-2], ylabel= dataLabels[end-1], legend= false)
end

# As it is known that there are 3 iris types, set minClusters to 3.
# By inputting dataMatrix[:, end-2:end-1], only the 2 plotted dimensions are used for the
# clustering. To use all dimensions, input the inputMatrix.
# Setting stopResolution as a safe guard is never wrong. It helps in case the algorithm cannot
# find at least 3 clusters to stop it after some loops.
IteridenseResult = Iteridense(
                                dataMatrix[:, end-2:end-1],
                                #inputMatrix,
                                minClusterSize= 10,
                                #noDiagonals= true,
                                useClusters= true, minClusters= 3,
                                stopResolution= 100
);
IteridenseResult.finalResolution
IteridenseResult.clusterDensities
IteridenseResult.clusterSizes
Plots.scatter(inputMatrix[:, end-1], inputMatrix[:, end],
                title= "Iteridense  MinClusters = 3",
                xlabel= dataLabels[end-2], ylabel= dataLabels[end-1],
                group= IteridenseResult.assignments)
# Because of its design Iteridense will fail on datasets like this. It cannot detect clusters
# that overlap each others.

# k-means clustering
begin
result = Clustering.kmeans(inputMatrix', 3; maxiter= 20, display= :iter)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
clusterCenter = result.centers
Plots.scatter(inputMatrix[:, end-1], inputMatrix[:, end], title= "k-means k = 3",
                xlabel= dataLabels[end-2], ylabel= dataLabels[end-1], group= assign)
end

# DBSCAN clustering
begin
ε = 0.4
# The minimum for min_neighbors should at least be dimension+1, better 2*dimension.
result = Clustering.dbscan(inputMatrix', ε, min_neighbors= 4, min_cluster_size= 10)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
Plots.scatter(dataMatrix[:, end-2], dataMatrix[:, end-1], title= "DBSCAN ε = $ε",
                xlabel= dataLabels[end-2], ylabel= dataLabels[end-1], group= assign)
end


#---------------------------------------------
# next dataset: concentrations of Plutonium isotopes in ore samples
begin
filePath = joinpath(@__DIR__, "datasets/Pluton.csv")
if isfile(filePath)
    data = CSV.read(filePath, DataFrame; delim= '\t')
end
#vscodedisplay(data)
# extract names and values
dataLabels = String.(names(data))
dataMatrix = Matrix(data)
DataPlot = Plots.scatter(dataMatrix[:, 2], dataMatrix[:, 3], dataMatrix[:, 4],
                xlabel= "Pu-239", ylabel= "Pu-240", zlabel= "Pu-241", legend= false)
end

# To take only the 3 plotted dimensions into account, input dataMatrix[:, 2:end] otherwise
# input the complete dataMatrix.
# As there are many dimensions, the density of the clusters will be higher than in the previous
# examples that had only 2 dimensions. But you can, as always, start with a low ρ and then
# increase it according to the output of clusterDensities.
ρ = 2
IteridenseResult = Iteridense(
                                dataMatrix[:, 2:end],
                                #dataMatrix,
                                density= ρ, minClusterSize= 4
);
IteridenseResult.finalResolution
IteridenseResult.clusterDensities
IteridenseResult.clusterSizes
Plots.scatter(dataMatrix[:, 2], dataMatrix[:, 3], dataMatrix[:, 4], xlabel= "Pu-239",
                ylabel= "Pu-240", zlabel= "Pu-241", title= "Iteridense ρ = $ρ",
                group= IteridenseResult.assignments)

# k-means clustering
begin
result = Clustering.kmeans(dataMatrix', 4; maxiter= 20, display= :iter)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
clusterCenter = result.centers
Plots.scatter(dataMatrix[:, 2], dataMatrix[:, 3], dataMatrix[:, 4],
                xlabel= "Pu-239", ylabel= "Pu-240", zlabel= "Pu-241",
                title= "k-means k = 4", group= assign)
end

# DBSCAN clustering
begin
ε = 2.2
result = Clustering.dbscan(dataMatrix', ε, min_neighbors= 4, min_cluster_size= 10)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
Plots.scatter(dataMatrix[:, 2], dataMatrix[:, 3], dataMatrix[:, 4],
                xlabel= "Pu-239", ylabel= "Pu-240", zlabel= "Pu-241",
                title= "DBSCAN ε = $ε", group= assign)
end
# DBSCAN performs poorly on this dataset. It can find maximal 2 clusters.


#---------------------------------------------
# last dataset: Ph.D. publications
begin
filePath = joinpath(@__DIR__, "datasets/PhdPubs.csv")
if isfile(filePath)
    data = CSV.read(filePath, DataFrame; delim= ',')
end
#vscodedisplay(data)
# extract names and values
dataLabels = String.(names(data))
dataMatrix = Matrix(data)
# create a subset with 3 dimensions to display
subLabels = hcat(dataLabels[2], dataLabels[6], dataLabels[7])
subMatrix = hcat(dataMatrix[:, 2], dataMatrix[:, 6], dataMatrix[:, 7])
# create a set with all dimensions
# the first column contains only the row number and will be therefore excluded
completeMatrix = dataMatrix[:, 2:end]
# plot data
DataPlot = Plots.scatter(dataMatrix[:, 2], dataMatrix[:, 6], dataMatrix[:, 7],
                        xlabel= subLabels[1], ylabel= subLabels[2], zlabel= subLabels[3],
                        legend= false, title= "Subset of PhDPublications dataset")
end

# The dataset has 6 dimensions and we purposely plot the dimension "phdprestige". This dimension
# divides the data into 5 classes. The aim of the clustering is therefore to identify clusters
# within these classes.
# This is possible by setting minClusters to 5 and inputting only the
# 3 plot dimensions (subMatrix).
# When inputting all 6 dimensions (completeMatrix), the clustering will be across the classes.
# One would then need to extract every class to different datasets and and run the clustering
# on every class. This is more work but one can then also go the way to specify ρ.

# At first the clustering of the whole dataset:
IteridenseResult = Iteridense(
                                subMatrix,
                                #completeMatrix,
                                minClusterSize= 20, useClusters= true, minClusters= 5
);
IteridenseResult.finalResolution
IteridenseResult.clusterDensities
IteridenseResult.clusterSizes
PlotResult = Plots.scatter(dataMatrix[:, 2], dataMatrix[:, 6], dataMatrix[:, 7],
                            xlabel= subLabels[1], ylabel= subLabels[2], zlabel= subLabels[3],
                            title= "Iteridense MinClusters = 5",
                            group= IteridenseResult.assignments)

# Now split the dataset into separate ones for every class and then cluster.
# The following code will apply the same ρ for all classes. This is not very sensible but
# the aim is just to demonstrate how the clustering is done.
for phdprestige in 1:5
    classDataFrame = data[data.phdprestige .== phdprestige, :]
    # remove column of phdprestige as this is no longer a dimension, also remove rownames column
    classDataFrame = select(classDataFrame, Not(:rownames, :phdprestige))
    classMatrix = Matrix(classDataFrame)
    # cluster the classMatrix
    ρ = 4.0
    IteridenseResult = Iteridense(classMatrix, density= ρ, minClusterSize= 20, stopResolution= 20)
    println("\nphdprestige = $(phdprestige)")
    println("numOfClusters = $(IteridenseResult.numOfClusters)")
    println("finalResolution = $(IteridenseResult.finalResolution)")
    println("clusterDensities = $(IteridenseResult.clusterDensities)")
    println("clusterSizes = $(IteridenseResult.clusterSizes)")
    ClassResult = Plots.scatter(classMatrix[:, 1], classMatrix[:, end],
                    xlabel= subLabels[1], ylabel= subLabels[3],
                    title= "phdprestige $phdprestige, ρ = $ρ",
                    group= IteridenseResult.assignments)
    display(ClassResult)
end

# k-means clustering
begin
result = Clustering.kmeans(subMatrix', 5; maxiter= 20, display= :iter)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
clusterCenter = result.centers
Plots.scatter(dataMatrix[:, 2], dataMatrix[:, 6], dataMatrix[:, 7], title= "k-means k = 5",
                xlabel= subLabels[1], ylabel= subLabels[2], zlabel= subLabels[3], group= assign)
end

begin
# DBSCAN clustering
ε = 0.7
# DBSCAN needs Float64 but subMatrix is Int64
inputMatrix = Float64.(subMatrix)
result = Clustering.dbscan(inputMatrix', ε, min_neighbors= 3, min_cluster_size= 4)
assign = Clustering.assignments(result)
clusterCounts = Clustering.counts(result)
Plots.scatter(dataMatrix[:, 2], dataMatrix[:, 6], dataMatrix[:, 7], title= "DBSCAN ε = $ε",
                xlabel= subLabels[1], ylabel= subLabels[2], zlabel= subLabels[3], group= assign)
end


#---------------------------------------------
# it is also possible to cluster 1-dimensional data:
DataPlot = Plots.scatter(subMatrix[:, end], repeat([0], length(subMatrix[:, end])), 
                        xlabel= dataLabels[7], ylimits= (-1, 1), yticks=[],
                        title= "Unclustered Data", legend= false, markersize= 5)

IteridenseResult = Iteridense(subMatrix[:, end], minClusterSize= 6,
                                useClusters= true, minClusters= 3);
IteridenseResult.finalResolution
IteridenseResult.clusterDensities
IteridenseResult.clusterSizes
PlotIteridenseHeatmap(subMatrix[:, end], IteridenseResult.finalResolution)
Plots.scatter(subMatrix[:, end], repeat([0], length(subMatrix[:, end])), 
                xlabel= dataLabels[7], ylimits= (-1, 1), yticks=[],
                title= "Iteridense MinClusters = 3",
                group= IteridenseResult.assignments, markersize= 5)
