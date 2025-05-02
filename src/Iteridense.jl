# License is LGPL-3.0-or-later (https://spdx.org/licenses/LGPL-3.0-or-later.html)
#
# author: Uwe Stöhr
"""
The `Iteridense` package provides functions used for the Iteridense clustering algorithm.
"""
module Iteridense

#using 

export  CreateCountTensor,
        AnalyzeClusters,
        Clustering,
        PlotIteridenseHeatmap


#-------------------------------------------------------------------------------------------------
# function to merge and to renumber clusters
function mergeRenumberClusters!(clusterTensor; currentNumber::Int, newNumber::Int)
    for k in eachindex(clusterTensor)
        if clusterTensor[k] == currentNumber
            clusterTensor[k] = newNumber
        end
    end
end


#-------------------------------------------------------------------------------------------------
# function to get valid neighbors for a given index and dimension
function getNeighbors!(maxIdxRange, dim::Int, CheckIdxTuple, neighborIndices, noDiagonals,
                        ::Val{dimensions}) where dimensions
    # get the index of the current dimension
    idx = CheckIdxTuple[dim]
    # if we exclude diagonal cells, there is only one possible neighbor
    if noDiagonals
        idxTuple = ntuple(i -> i == dim ? idx : CheckIdxTuple[i], Val(dimensions))
        push!(neighborIndices, idxTuple)
        return neighborIndices
    end
    # vary idx, if we are at the last dimension we can output the indices
    # otherwise we have to call getNeighbors! for every variation to check further dimensions
    for offset in -1:1
        neighborIdx = idx + offset
        if neighborIdx in maxIdxRange
            # there are furter dimensions to be checked, thus call getNeighbors! recursively
            if dim > 1
                CheckIdxTupleNew = ntuple(i -> i == dim ? neighborIdx : CheckIdxTuple[i],
                                            Val(dimensions))
                getNeighbors!(maxIdxRange, dim-1, CheckIdxTupleNew, neighborIndices, noDiagonals,
                                Val(dimensions))
            else
                idxTuple = ntuple(i -> i == dim ? neighborIdx : CheckIdxTuple[i], Val(dimensions))
                push!(neighborIndices, idxTuple)
            end
        end
    end
    return neighborIndices
end


#-------------------------------------------------------------------------------------------------
# function to get the cluster numbers of neighboring cells
function getClusterNumbers(clusterTensor, neighborIndices)
    clusterNumbers::Vector{Int} = []
    for idx in neighborIndices
        push!(clusterNumbers, clusterTensor[idx...])
    end
    return clusterNumbers
end


#-------------------------------------------------------------------------------------------------
# function to test if a cell is part of a cluster
function checkNeighbors(clusterTensor, currentIdx, numClusters::Int, maxIdxRange, dimOrder,
                            noDiagonals, ::Val{dimensions}) where dimensions
    # create vector in which the indices of all neighboring cells will be stored
    neighborIndices::Vector = []
    # Check neighbors dimension by dimension in reverse order
    for dim in dimOrder
        # go with the index one down in the current dimension
        checkDimIdx::Int = currentIdx[dim] - 1
        # if checkDimIdx is valid, check the neighbors that share the same dimension index
        if checkDimIdx in maxIdxRange
            # if we are at the last index, there is only one neighbor
            if dim == 1
                idxTuple = ntuple(i -> i == dim ? checkDimIdx : currentIdx[i], Val(dimensions))
                push!(neighborIndices, idxTuple)
            else
                # pass the new index to the function to determine neighbors
                CheckIdxTuple = ntuple(i -> i == dim ? checkDimIdx : currentIdx[i], Val(dimensions))
                neighborIndices = getNeighbors!(maxIdxRange, dim-1, CheckIdxTuple,
                                                neighborIndices, noDiagonals, Val(dimensions))
            end
        else
            continue
        end
    end
    # the neighbors can be in different clusters, in this case we need to unite them
    clusterNumbers = getClusterNumbers(clusterTensor, neighborIndices)
    # check all clusterNumbers
    currentClusterNumber = 0
    for i in eachindex(clusterNumbers)
        if clusterNumbers[i] > 0 && currentClusterNumber == 0
            currentClusterNumber = clusterNumbers[i]
            continue
        end
        if clusterNumbers[i] > 0 && currentClusterNumber != clusterNumbers[i]
            # merge the greater cluster index to the lower one
            if currentClusterNumber > clusterNumbers[i]
                mergeRenumberClusters!(clusterTensor, currentNumber= currentClusterNumber,
                                        newNumber= clusterNumbers[i])
                currentClusterNumber = clusterNumbers[i]
                # update clusterNumbers
                clusterNumbers = getClusterNumbers(clusterTensor, neighborIndices)
            else
                mergeRenumberClusters!(clusterTensor, currentNumber= clusterNumbers[i],
                                        newNumber= currentClusterNumber)
                clusterNumbers = getClusterNumbers(clusterTensor, neighborIndices)
            end
        end
    end
    # if there are no neighbors, add a 0 to clusterNumbers to assure we later start a new cluster
    if isempty(clusterNumbers)
        push!(clusterNumbers, 0)
    end
    if maximum(clusterNumbers) != minimum(clusterNumbers) && minimum(clusterNumbers) > 0
        error("Clustering failed to correctly merge connected clusters")
    end
    # if at least one neighbor is in a cluster, add the current cell to it
    if maximum(clusterNumbers) > 0
        clusterTensor[currentIdx...] = maximum(clusterNumbers)
    else
        # otherwise start a new cluster
        # due to possible cluster merges we need to check the currently highest cluster number
        numClusters = Int(maximum(clusterTensor)) + 1
        clusterTensor[currentIdx...] = numClusters
    end
    # assure we return the currently highest cluster number
    numClusters = Int(maximum(clusterTensor))
    return numClusters, clusterTensor
end


#-------------------------------------------------------------------------------------------------
# function determine the number of points within the cells
function CreateCountTensor(dataMatrix, resolution::Int, minMatrix::Vector, maxMatrix::Vector,
                            ::Val{dimensions}) where dimensions
    if !(length(minMatrix) == length(maxMatrix) == size(dataMatrix, 2) == dimensions)
        error("Dimensions mismatch")
    end
    # at first calculate the cell size for every dimension
    sizeMatrix = ntuple(i -> (maxMatrix[i] - minMatrix[i]) / resolution, Val(dimensions))
    # we need now a tensor in that has resolution entries for every dimension
    countTensor = zeros(Int, ntuple(i -> resolution, Val(dimensions)))

    # create a matrix with coordinates in our grid
    # every of its columns will get the coordinate values for the particular data point
    # the countTensor is increased at the coordinates of the data point
    smallValue = 1e-6
    for k in axes(dataMatrix, 1)
        # Julia swaps in matrices x and y, thus reverse to use the coordinate system of the plot
        # due to precision issues, we subtract a small value from the values
        coordMatrix = ntuple(i -> trunc(Int, (dataMatrix[k, dimensions-i+1] - smallValue -
                                                minMatrix[dimensions-i+1]) /
                                            sizeMatrix[dimensions-i+1]) + 1, Val(dimensions))
        countTensor[coordMatrix...] += 1
    end
    return countTensor
end


#-------------------------------------------------------------------------------------------------
# function to remove empty clusters
function removeEmptyClusters!(clusterTensor, numClusters::Int)
    # as we can have any pattern of empty clusters, we evaluate first every cluster
    isEmpty = zeros(Int, numClusters)
    for n in 1:numClusters
        if count(i -> (i == n), clusterTensor) == 0
            isEmpty[n] = 1
        end
    end
    cluster = 1
    while cluster < numClusters + 1
        if isEmpty[cluster] == 1
            if cluster == numClusters
                # if last cluster is empty
                numClusters -= 1
            else
                # copy next non-empty cluster number to current one
                n = cluster+1
                while n < numClusters + 1 && isEmpty[n] == 1
                    n += 1
                end
                # if all clusters till end are empty
                if n > numClusters
                    numClusters = cluster-1
                else
                    mergeRenumberClusters!(clusterTensor, currentNumber= n,
                                            newNumber= cluster)
                    isEmpty[cluster] = 0
                    isEmpty[n] = 1
                end
            end
        end
        cluster += 1
    end
    return numClusters, clusterTensor
end


#-------------------------------------------------------------------------------------------------
# function that actually performs the clustering
function InternalClustering(countTensor, resolution::Int, noDiagonals,
                            ::Val{dimensions}) where dimensions
    # first create a tensor to store later the information about the clusters
    clusterTensor = zeros(Int, ntuple(i -> resolution, Val(dimensions)))

    numClusters = 0
    # to later check the maximal index range of the clusterTensor
    maxIdxRange = 1:resolution
    # to later check neighbors dimension by dimension in reverse order
    dimOrder = reverse(1:dimensions)
    # check neighbors for all cells of the countTensor
    iterRange = Iterators.product(ntuple(_ -> 1:resolution,  Val(dimensions))...)
    for indices in iterRange
        if countTensor[indices...] > 1
            numClusters, clusterTensor = checkNeighbors(clusterTensor, indices, numClusters,
                                                            maxIdxRange, dimOrder, noDiagonals,
                                                            Val(dimensions))
        end
    end
    # due to cluster merges in the clustering process we might end up with non-sequent cluster
    # numbering where e.g. there are cells in cluster 2 and 4 but not in cluster 1 and 3
    # to avoid that we have to rename the clusters
    if numClusters > 1
        numClusters, clusterTensor = removeEmptyClusters!(clusterTensor, numClusters)
    end
    return numClusters, clusterTensor
end


#-------------------------------------------------------------------------------------------------
# function to analyze the density of the found clusters
function AnalyzeClusters(clusterTensor, countTensor, numClusters::Int, resolution::Int,
                            totalCounts::Int)
    numOfCells = length(countTensor)
    clusterDensities = zeros(numClusters)
    clusterSizes = zeros(Int, numClusters)
    for cluster in 1:numClusters
        cellCounter = 0
        # first sum values of the cluster cells
        for i in eachindex(clusterTensor)
            if clusterTensor[i] == cluster
                clusterDensities[cluster] += countTensor[i]
                cellCounter += 1
            end
        end
        clusterSizes[cluster] = clusterDensities[cluster]
        # now calculate the cluster density in counts per area
        clusterDensities[cluster] = clusterSizes[cluster] / (cellCounter / numOfCells)
        # normalize the density to 1
        clusterDensities[cluster] = clusterDensities[cluster] / totalCounts
        # if a cluster contains all points its volume al actually the real volume
        # spanned by the data. By definition the density of the cluster is then 1.0
        if clusterSizes[cluster] == totalCounts
            clusterDensities[cluster] = 1.0
        end
    end
    return clusterDensities, clusterSizes
end


#-------------------------------------------------------------------------------------------------
# structure for the clustering result
@kwdef struct IteridenseResult
    clusterTensor
    countTensor
    numOfClusters
    finalResolution
    assignments
    clusterDensities
    clusterSizes
end


#-------------------------------------------------------------------------------------------------
# function to perform the Iteridense algorithm loop
function IteridenseLoop(dataMatrix, minClusterSize::Int, density, stopResolution::Int,
                        minClusters::Int, minClusterDensity, noDiagonals,
                        useDensity, useClusters, useFixedResolution,
                        resolution::Int, maxResolution::Int, minMatrix, maxMatrix,
                        totalCounts::Int, ::Val{dimensions}) where dimensions
    # initializations
    countTensor = Array{Any}(undef)
    clusterTensor = Array{Any}(undef)
    clusterDensities = Array{Any}(undef)
    clusterSizes = Array{Any}(undef)
    numClusters::Int = 0
    achievedDensity = 0.0
    # the main loop
    while resolution < maxResolution
        # explicitly free memory of no longer used tensors
        countTensor = nothing
        clusterTensor = nothing
        countTensor = CreateCountTensor(dataMatrix, resolution, minMatrix, maxMatrix,
                                        Val(dimensions))
        numClusters, clusterTensor = InternalClustering(countTensor, resolution, noDiagonals,
                                                        Val(dimensions))
        # clusters can only be analyzed if there is at least one
        if numClusters == 0
            resolution += 1
            continue
        end
        clusterDensities, clusterSizes = AnalyzeClusters(clusterTensor, countTensor, numClusters,
                                                            resolution, totalCounts)
        # Remove clusters smaller than minClusterSize or with density < minClusterDensity:
        # First set these cluster numbers to zero, then remove the empty clusters and eventually
        # re-evaluate the remaining clusters.
        for cluster in 1:numClusters
            if clusterSizes[cluster] < minClusterSize ||
                clusterDensities[cluster] < minClusterDensity
                mergeRenumberClusters!(clusterTensor, currentNumber= cluster,
                                        newNumber= 0)
            end
        end
        numClusters, clusterTensor = removeEmptyClusters!(clusterTensor, numClusters)
        if numClusters == 0
            resolution += 1
            continue
        end
        clusterDensities, clusterSizes = AnalyzeClusters(clusterTensor, countTensor, numClusters,
                                                            resolution, totalCounts)
        achievedDensity = minimum(clusterDensities)
        # break the loop according to the settings
        if (useDensity && (achievedDensity > density)) ||
            (useClusters && (numClusters ≥ minClusters)) ||
            useFixedResolution ||
            resolution == stopResolution
            break
        end
        # increase resolution for next iteration
        resolution += 1
    end

    # if the previous while loop was ended because resolution ≥ maxResolution
    # the resolution is too high and has to be reset
    if resolution ≥ maxResolution || resolution > stopResolution
        resolution = size(clusterTensor, 1)
    end

    assignments = zeros(Int, totalCounts)
    
    return IteridenseResult(clusterTensor= clusterTensor, countTensor= countTensor,
                            numOfClusters= numClusters, finalResolution= resolution,
                            assignments= assignments, clusterDensities= clusterDensities,
                            clusterSizes= clusterSizes)
end


#-------------------------------------------------------------------------------------------------
# function to assign data points to clusters
function AssignPoints(dataMatrix, clusterTensor, resolution::Int, minMatrix, maxMatrix,
                        totalCounts::Int, ::Val{dimensions}) where dimensions
    # at first calculate the cell size for every dimension
    sizeMatrix = zeros(dimensions)
    for i in 1:dimensions
        sizeMatrix[i] = (maxMatrix[i] - minMatrix[i]) / resolution
    end
    # now assign every data point
    assignments = zeros(Int, totalCounts)
    smallValue = 1e-6
    for k in axes(dataMatrix, 1)
        # Julia swaps in matrices x and y, thus reverse to use the coordinate system of the plot
        # due to precision issues, we subtract a small value from the values
        coordMatrix = ntuple(i -> trunc(Int, (dataMatrix[k, dimensions-i+1] - smallValue -
                                                minMatrix[dimensions-i+1]) /
                                            sizeMatrix[dimensions-i+1]) + 1, Val(dimensions))
        assignments[k] = clusterTensor[coordMatrix...]
    end
    return assignments
end


#-------------------------------------------------------------------------------------------------
# main function
function Clustering(dataMatrix; minClusterSize::Int= 3, startResolution::Int= 2,
                    density= 1.1, stopResolution::Int= -1, minClusters::Int= 1, useDensity= true,
                    minClusterDensity= 1.0, noDiagonals= false, useClusters= false)

    if startResolution == stopResolution
        useFixedResolution = true
    else
        useFixedResolution = false
    end
    if (!useDensity && !useClusters && !useFixedResolution)
        error("No information given on how to stop the clustering process")
    end
    # useClusters works as a toggle, if on, the density is not used
    if useClusters
        useDensity= false
    end
    # assure to have sensible inputs
    if minClusterSize < 2
        minClusterSize = 2
    end
    if startResolution < 2
        startResolution = 2
    end
    if density < 1
        density = 1.0
    end
    if stopResolution > -1 && stopResolution < startResolution
        stopResolution = startResolution
    end
    if minClusters < 1
        minClusters = 1
    end
    if minClusterDensity < 1
        minClusterDensity = 1.0
    end
    # minClusterDensity must not be greater than the density
    if minClusterDensity > density
        minClusterDensity = density
    end
    # at first count data points and determine the dimensions
    totalCounts::Int = size(dataMatrix, 1)
    dimensions::Int = size(dataMatrix, 2)
    # dataMatrix is a matrix in which every column contains the data of a dimension
    # therefore store the min/max of every dimension in vectors
    minMatrix= zeros(dimensions)
    maxMatrix= zeros(dimensions)
    for i in 1:dimensions
        minMatrix[i] = minimum(dataMatrix[:, i])
        maxMatrix[i] = maximum(dataMatrix[:, i])
    end

    # minClusterSize cannot be greater than totalCounts - 1
    if minClusterSize ≥ totalCounts
        minClusterSize = totalCounts - 1
    end
    # initializations
    resolution::Int = startResolution
    clusterDensities = Array{Any}(undef)
    clusterSizes = Array{Any}(undef)
    # assure that maximal resolution is used as limit
    maxResolution = totalCounts
    if stopResolution > -1
        if stopResolution > maxResolution
            stopResolution = maxResolution
        end
        maxResolution = stopResolution + 1
    end
    if useFixedResolution
        if startResolution > maxResolution
            startResolution = maxResolution - 1
        end
        resolution = startResolution
        maxResolution = startResolution + 1
    end
    if resolution > maxResolution
        @warn("Given resolution is greater than MaxResolution\n\
                The resolution was reset to MaxResolution")
    end
    # the main clustering loop
    LoopResult = IteridenseLoop(dataMatrix, minClusterSize, density, stopResolution,
                        minClusters, minClusterDensity, noDiagonals, useDensity, useClusters,
                        useFixedResolution, resolution, maxResolution, minMatrix, maxMatrix,
                        totalCounts, Val(dimensions))
    #=
    The clustering could always lead to artifacts, that e.g. a point belonging to a cluster
    appears as single point at the corner of a cell and is therefore not detected as part
    of a cluster. Therefore we run a single run with a higher resolution. Points that were before
    not part of a cluster but now are, are eventually assigned to that cluster. This can only be
    done if the number of clusters did not change.
    =#

    # if no cluster was found, return a warning and an empty result and set the assignment
    # to cluster 0
    if LoopResult.numOfClusters == 0
        printstyled("Information: "; bold=true, color=:blue)
        println("The clustering detected no clusters")
        return IteridenseResult(clusterTensor= [], countTensor= [], numOfClusters= 0,
                                finalResolution= LoopResult.finalResolution,
                                assignments= zeros(Int, totalCounts),
                                clusterDensities= [], clusterSizes= [])
    else
        # run a single loop with a higher fixed resolution
        secondResolution = LoopResult.finalResolution + 1
        LoopResultSecond = IteridenseLoop(dataMatrix, minClusterSize, density, stopResolution,
                                minClusters, minClusterDensity, noDiagonals, useDensity,
                                useClusters, true, secondResolution, maxResolution, minMatrix,
                                maxMatrix, totalCounts, Val(dimensions))
    end

    # assign the data points according to the first clustering result
    assignments = AssignPoints(dataMatrix, LoopResult.clusterTensor,
                                LoopResult.finalResolution, minMatrix, maxMatrix,
                                totalCounts, Val(dimensions))

    intermediateResult = IteridenseResult(clusterTensor= LoopResult.clusterTensor,
                                            countTensor= LoopResult.countTensor,
                                            numOfClusters= LoopResult.numOfClusters,
                                            finalResolution= LoopResult.finalResolution,
                                            assignments= assignments,
                                            clusterDensities= LoopResult.clusterDensities,
                                            clusterSizes= LoopResult.clusterSizes)

    # if the number of clusters did change between the 2 runs, return the assignment according
    # to the first clustering result
    if LoopResult.numOfClusters != LoopResultSecond.numOfClusters || useFixedResolution
        return intermediateResult
    else
        assignmentsSecond = AssignPoints(dataMatrix, LoopResultSecond.clusterTensor,
                                            LoopResultSecond.finalResolution,
                                            minMatrix, maxMatrix, totalCounts, Val(dimensions))
    end

    #=
    The merging logic is the following:
    * if a point is in different non-zero clusters, we cannot decide and then have to
      use the LoopResult as final result
    * if a point is in cluster zero in only one of both assignments, change it to the
      non-zero value
    =#
    # First item:
    for i in eachindex(assignments)
        if assignments[i] != assignmentsSecond[i] &&
            (assignments[i] > 0 && assignmentsSecond[i] > 0)
            return intermediateResult
            break
        end
    end
    # For the second item, we subtract both assignment vectors, set negative values in the
    # result to zero and then add this to assignmentsSecond.
    assignmentsResult = max.(assignments .- assignmentsSecond, 0)
    assignmentsResult = assignmentsResult .+ assignmentsSecond

    # to update clusterSizes we don't need to update and evaluate the clusterTensor
    # but count in assignmentsResult
    # we have to count the zero clusters too
    clusterCounts = zeros(Int, LoopResult.numOfClusters + 1)
    for num in assignmentsResult
        #println("num: $(num)")
        #println("num: $(num)")
        clusterCounts[num+1] += 1
    end
    # cut off the zero clusters
    clusterCounts = clusterCounts[2:end]
    clusterSizes = reshape(clusterCounts, :)
    # For the clusterDensities it is not straigh-forward since there is no clear resolution
    # and tensors to be taken. We therefore simply take the mean of the 2.
    clusterDensities = (LoopResult.clusterDensities .+
                        LoopResultSecond.clusterDensities) ./ 2

    # for the final result we output the tensors and resolution of the first result
    return IteridenseResult(clusterTensor= LoopResult.clusterTensor,
                            countTensor= LoopResult.countTensor,
                            numOfClusters= LoopResult.numOfClusters,
                            finalResolution= LoopResult.finalResolution,
                            assignments= assignmentsResult,
                            clusterDensities= clusterDensities,
                            clusterSizes= clusterSizes)
end


#-------------------------------------------------------------------------------------------------
# function to plot heatmaps for custom resolutions
function PlotIteridenseHeatmap(dataMatrix, resolution::Int)
    @eval using Plots
    # getting interactive plots
    plotlyjs()
    # heatmaps can only be created for 1 or 2 dimensions
    dimensions= size(dataMatrix, 2)
    if size(dataMatrix, 2) > 2
        error("The count map can only be visualized for 1- or 2-dimensional data.\
               \n\tBut the input data has $dimensions dimensions.")
    end
    # calculate min/max and the ranges
    minMatrix= zeros(dimensions)
    maxMatrix= zeros(dimensions)
    ranges= Vector{Any}(undef, dimensions)
    for i in 1:dimensions
        minMatrix[i] = minimum(dataMatrix[:, i])
        maxMatrix[i] = maximum(dataMatrix[:, i])
        ranges[i] = Vector(maxMatrix[i]/(2*resolution):maxMatrix[i]/resolution:maxMatrix[i])
    end
    # create the count matrix for the given resolution
    countMatrix = CreateCountTensor(dataMatrix, resolution, minMatrix, maxMatrix, Val(dimensions))
    # plot the countMatrix as heatmap
    if dimensions == 2
        return Plots.heatmap(ranges[1], ranges[2], countMatrix, aspect_ratio= 1, color= cgrad(:heat))
    else
        countVector = reshape(countMatrix, 1, :)
        return Plots.heatmap(countVector, yticks=[], color= cgrad(:heat))
    end
end


end # end of module
