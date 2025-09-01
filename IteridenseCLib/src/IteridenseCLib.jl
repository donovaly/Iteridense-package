# License is LGPL-3.0-or-later (https://spdx.org/licenses/LGPL-3.0-or-later.html)
#
# author: Uwe Stöhr
"""
The `Iteridense` package provides functions used for the Iteridense clustering algorithm.
"""
module IteridenseCLib

using Base.Libc, Clustering

export  IteridenseClustering, IteridenseFree, GarbageCollection, FreeMemoryInBytes,
        DBSCANClustering, DBSCANFree,
        KMeansClustering, KMeansFree

const smallValue = 1e-6 # for 64bit, for 32bit one needs 1e-5

#-------------------------------------------------------------------------------------------------
# function to merge and to renumber clusters
function renumberClusters!(clusterTensor; currentNumber::Int64, newNumber::Int64)
    if currentNumber == newNumber
        return
    end
    for k in eachindex(clusterTensor)
        if clusterTensor[k] == currentNumber
            clusterTensor[k] = newNumber
        end
    end
end


#-------------------------------------------------------------------------------------------------
# helper functions to manage cluster merges
# its purpose is to track the cluster number when it is changed by cluster merges
function findRootNumber(clusterNumber::Int64, clusterParent::Vector{Int64})
    # if the cluster is its own parent, it is the root
    if clusterParent[clusterNumber] == clusterNumber
        return clusterNumber
    end
    # using path compression to find the root number
    clusterParent[clusterNumber] = findRootNumber(clusterParent[clusterNumber], clusterParent)
    return clusterParent[clusterNumber]
end


#-------------------------------------------------------------------------------------------------
# function to merge two clusters
function mergeClusters(clusterNumber1::Int64, clusterNumber2::Int64, clusterParent::Vector{Int64})
    rootCell1 = findRootNumber(clusterNumber1, clusterParent)
    rootCell2 = findRootNumber(clusterNumber2, clusterParent)

    if rootCell1 != rootCell2
        # merge cluster with the larger root into the one with smaller root
        clusterParent[max(rootCell1, rootCell2)] = min(rootCell1, rootCell2)
    end
    return findRootNumber(rootCell1, clusterParent) # root of the merged cluster
end


#-------------------------------------------------------------------------------------------------
# function to get valid neighbors for a given index and dimension
function getNeighbors!(maxIdxRanges, dim::Int64, CheckIdxTuple, neighborIndices,
                        noDiagonals::Bool, ::Val{dimensions}) where dimensions
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
        if neighborIdx in maxIdxRanges[dim]
            # there are further dimensions to be checked, thus call getNeighbors! recursively
            if dim > 1
                CheckIdxTupleNew = ntuple(i -> i == dim ? neighborIdx : CheckIdxTuple[i],
                                            Val(dimensions))
                getNeighbors!(maxIdxRanges, dim-1, CheckIdxTupleNew, neighborIndices, noDiagonals,
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
function checkNeighbors(clusterTensor, currentIdx, nextClusterNumber::Int64, maxIdxRanges, dimOrder,
                            noDiagonals::Bool, clusterParent::Vector{Int64},
                            ::Val{dimensions}) where dimensions
    # create vector in which the indices of all neighboring cells will be stored
    neighborIndices::Vector{NTuple{dimensions, Int64}} = NTuple{dimensions, Int64}[]

    # check neighbors dimension by dimension in reverse order
    for dim in dimOrder
        # go with the index one down in the current dimension
        checkDimIdx::Int = currentIdx[dim] - 1
        # if checkDimIdx is valid, check the neighbors that share the same dimension index
        if checkDimIdx in maxIdxRanges[dim]
            # if we are at the last index, there is only one neighbor
            if dim == 1
                idxTuple = ntuple(i -> i == dim ? checkDimIdx : currentIdx[i], Val(dimensions))
                push!(neighborIndices, idxTuple)
            else
                # pass the new index to the function to determine neighbors
                CheckIdxTuple = ntuple(i -> i == dim ? checkDimIdx : currentIdx[i],
                                        Val(dimensions))
                # fill neighborIndices
                getNeighbors!(maxIdxRanges, dim-1, CheckIdxTuple,
                                                neighborIndices, noDiagonals, Val(dimensions))
            end
        end
    end

    # the neighbors can be in different clusters, in this case we need to merge them
    # for this, first store the cluster numbers
    rawClusterNums = getClusterNumbers(clusterTensor, neighborIndices)

    # Instead of performing the cluster merges step by step by changing the tensor for every
    # only save the information about the cell numbers. This way we track the number history of
    # a cluster and can eventually perform a single step through the tensor to update the cluster
    # number. To track the number history we use a Union-Find structure.

    # store the cluster numbers of valid neighbor cells
    # We don't use a vector but a set because then we would later need a call of unique!()
    # push! on a set automatically adds the element if it is not already present, ensuring
    # uniqueness without extra work. As we have positive integers we can use even a BitSet
    # (It turned out that unique() is a bottleneck in Julia and should be avoided if possible.)
    neighborClusterNumbers = BitSet()
    for currentClusterNumbers in rawClusterNums
        # cluster zero means unclustered
        if currentClusterNumbers > 0
            # find the root (initial number) of the current cluster in the Union-Find structure
            # the root is the lowest cluster number of a cluster that is now part of the cluster
            # (for example when cluster 12 is merged to number 5, and cluster 5 is its own root
            #  then clusterParent[12] becomes 5)
            rootOfCurrentCluster = findRootNumber(currentClusterNumbers, clusterParent)
            push!(neighborClusterNumbers, rootOfCurrentCluster)
        end
    end

    currentAssignedCluster = 0
    if !isempty(neighborClusterNumbers)
        # find the smallest cluster number among neighbors to assign it to current cell
        currentAssignedCluster = minimum(neighborClusterNumbers)
        # perform the merge
        for clusterNum in neighborClusterNumbers
            if clusterNum != currentAssignedCluster
                mergeClusters(clusterNum, currentAssignedCluster, clusterParent)
            end
        end
    end

    # if at least one neighbor is in a cluster, add the current cell to it
    if currentAssignedCluster > 0
        clusterTensor[currentIdx...] = currentAssignedCluster
    else
        # otherwise start a new cluster
        clusterTensor[currentIdx...] = nextClusterNumber
        nextClusterNumber += 1
    end
    return nextClusterNumber
end


#-------------------------------------------------------------------------------------------------
# function to reduce the number of cells per dimension
function reduceVectorEntries(aVector::AbstractVector{Int})
    # at first we create a count vector of how often a cell number occurs. Then every cell with
    # zero counts is removed if its previous cell has also zero. This is because we must keep a
    # single zero cell to separate the clusters. As last step the assignments are updated
    # according to the new indices.

    # safe guard
    if isempty(aVector)
        return Vector{Int64}(), 1
    end

    # create count map
    # it turned out that using a BitSet is about 10 % faster then using
    # observedNumbers = sort(unique(aVector))
    observedNumbersBitSet = BitSet()
    for numberValue in aVector
        push!(observedNumbersBitSet, numberValue)
    end

    # find gaps and determine which numbers to keep
    # in fact we slice out every number whose predecessor iz a zero and the predecessor of
    # that zero is also a zero
    numbersToMapBitSet = BitSet()
    for num in observedNumbersBitSet
        # keep the observed number itself
        push!(numbersToMapBitSet, num) 
        # If num - 1 exists and is not an observed number, it means num - 1 represents a gap
        # before num. We keep this predecessor to ensure cluster separation.
        if num > 1 && !(num - 1 in observedNumbersBitSet)
            push!(numbersToMapBitSet, num - 1)
        end
    end

    # set the new indices
    maxValue = maximum(aVector)
    newIndices = Dict{Int64, Int64}()
    currentIndex = 1
    for k in 1:maxValue
        if k in numbersToMapBitSet
            newIndices[k] = currentIndex
            currentIndex += 1
        end
    end
    # get number of tensor entries for the current dimension
    reducedLength = currentIndex - 1

    # apply re-indexing to the vector
    newVector = Vector{Int64}(undef, length(aVector))
    for i in eachindex(aVector)
        newVector[i] = newIndices[aVector[i]]
    end
    return newVector, reducedLength
end


#-------------------------------------------------------------------------------------------------
# function assign every data point to a cell in a tensor
function cellAssignments(dataMatrix, inverseSizeVector, offsetVector, numData::Int64,
                            ::Val{dimensions}) where dimensions

    # for every dimension we create a tuple assigning every point to the cell number
    cellAssigns = Matrix{Int64}(undef, numData, dimensions)
    countTensorDims = zeros(Int64, dimensions)
    for dim in 1:dimensions
        for point in 1:numData
            cellAssigns[point, dim] = trunc(Int64, (dataMatrix[point, dim] - offsetVector[dim]) *
                                               inverseSizeVector[dim]) + 1
        end
        # now omit cells with value zero whose predecessor has also value zero
        cellAssigns[:, dim], countTensorDims[dim] = reduceVectorEntries(view(cellAssigns, :, dim))
    end
    return cellAssigns, countTensorDims
end


#-------------------------------------------------------------------------------------------------
# function determine the number of points within the cells
function CreateCountTensor(dataMatrix, resolution::Int64, numData::Int64,
                            minVector::Vector{Float64}, maxVector::Vector{Float64},
                            omitEmptyCells::Bool, ::Val{dimensions}) where dimensions

    # tuple to later reverse the dimension order
    reverseDims = ntuple(i -> dimensions-i+1, Val(dimensions))
    # calculate the cell size for every dimension
    sizeVector = (maxVector .- minVector) ./ resolution
    # take the inverse to later be able to multiply
    inverseSizeVector = 1.0 ./ sizeVector
    # add a small value to minVector for numerical reasons, see below
    offsetVector = minVector .+ smallValue

    # we have 2 ways to create the countTensor:
    # A: a symmetric tensor in which there are resolution entries for every dimension
    #    this is for low dimensions the fastest
    # B: an asymmetric tensor in which there are only as many entries per dimension as necessary
    #    this has memory advantages because cells containing zeroes have no information value and
    #    can be omitted. But it is computationally costly to determine what cells can be removed.
    if omitEmptyCells # way B
        # get for every point the assignments to the cell number in the future tensor
        cellAssigns, countTensorDims = cellAssignments(dataMatrix, inverseSizeVector,
                                                        offsetVector, numData, Val(dimensions))
        # create the countTensor with rank of dimensions, but every dimension has now not
        # resolution entries, but only as many as really necessary
        # Julia swaps in matrices x and y, thus reverse to use the coordinate system of the plot
        countTensor = zeros(Int32, reverse(countTensorDims)...)

        # precompute reversal for indices to avoid to recreate tuple in the following for loop
        for point in 1:numData
            idx = CartesianIndex(ntuple(i -> cellAssigns[point, reverseDims[i]],
                                    Val(Int(dimensions))))
            countTensor[idx] += 1
        end
    else # way A
        # create a symmetric tensor
        countTensor = zeros(Int32, ntuple(i -> resolution, Val(dimensions)))

        # create a matrix with coordinates in our grid
        # every of its columns will get the coordinate values for the particular data point
        # the countTensor cell at the coordinates of the data point is increased
        for point in 1:numData
            # Julia swaps in matrices x and y, thus reverse to use the coordinate system
            # of the plot
            # due to precision issues, we subtract a small value from the values
            idx = CartesianIndex(ntuple(i -> trunc(Int64, (dataMatrix[point, reverseDims[i]] -
                                    offsetVector[reverseDims[i]]) *
                                    inverseSizeVector[reverseDims[i]]) + 1, Val(dimensions)))
            countTensor[idx] += 1
        end
    end
    return countTensor
end


#-------------------------------------------------------------------------------------------------
# function to remove empty clusters
function removeEmptyClusters!(clusterTensor, currentMaxClusterNumber::Int64)
    # count occurrences of each cluster
    clusterCounts = zeros(Int, currentMaxClusterNumber)
    for val in clusterTensor
        if val > 0 && val <= currentMaxClusterNumber # Only count valid cluster IDs
            clusterCounts[val] += 1
        end
    end

    # create a map from old cluster numbers to new ones
    newClusterNumberMap = zeros(Int, currentMaxClusterNumber)
    newCurrentClusterNumber = 1
    for i in 1:currentMaxClusterNumber
        if clusterCounts[i] > 0
            newClusterNumberMap[i] = newCurrentClusterNumber
            newCurrentClusterNumber += 1
        end
    end
    finalNumClusters = newCurrentClusterNumber - 1

    # apply the remapping in one run over clusterTensor
    for k in eachindex(clusterTensor)
        if clusterTensor[k] > 0
            clusterTensor[k] = newClusterNumberMap[clusterTensor[k]]
        end
    end

    return finalNumClusters, clusterTensor
end


#-------------------------------------------------------------------------------------------------
# function that actually performs the clustering
function InternalClustering(countTensor, resolution::Int64, noDiagonals::Bool,
                            ::Val{dimensions}) where dimensions
    # create a tensor to store later the information about the clusters
    clusterTensor = similar(countTensor)
    fill!(clusterTensor, 0)
 
    # clusterParent stores the parent in a Union-Find structure
     maxPossibleClusters = length(clusterTensor) + 1
    # initialize each cluster as its own parent
    clusterParent = collect(1:maxPossibleClusters)

    # nextClusterNumber tracks the next available cluster number
    nextClusterNumber = 1
    # to later check the maximal index range of the clusterTensor
    maxIdxRanges = ntuple(i -> 1:size(countTensor, i), Val(dimensions))
    # to later check neighbors dimension by dimension in reverse order
    dimOrder = reverse(1:dimensions)
    # check neighbors for all cells of the countTensor
    for idx in eachindex(countTensor)
        # only if a cell has more than one data point it can be part of a cluster
        if countTensor[idx] > 1
            # convert linear index to Cartesian index (as a tuple)
            indices = Tuple(CartesianIndices(countTensor)[idx])
            # clusterParent will be modified by checkNeighbors() through mergeClusters()
            nextClusterNumber = checkNeighbors(clusterTensor, indices, nextClusterNumber,
                                                maxIdxRanges, dimOrder, noDiagonals,
                                                clusterParent, Val(dimensions))
        end
    end

    # store the highest assigned cluster number and resize clusterParent to that value
    maxClusterNumber = nextClusterNumber - 1
    resize!(clusterParent, maxClusterNumber)

    # re-map all entries in clusterTensor to their final, canonical cluster number
    # this applies all merges that were recorded in clusterParent
    for k in eachindex(clusterTensor)
        if clusterTensor[k] > 0
            clusterTensor[k] = findRootNumber(Int(clusterTensor[k]), clusterParent)
        end
    end

    # due to cluster merges in the clustering process we might end up with non-sequent cluster
    # numbering where e.g. there are cells in cluster 2 and 4 but not in cluster 1 and 3
    # to avoid that we have to rename/remove the clusters
    if maxClusterNumber > 0
        numClusters, clusterTensor = removeEmptyClusters!(clusterTensor, maxClusterNumber)
    else
        numClusters = 0 # No clusters found
    end
    return numClusters, clusterTensor
end


#-------------------------------------------------------------------------------------------------
# function to analyze the density of the found clusters
function AnalyzeClusters(clusterTensor, countTensor, numClusters::Int64, resolution::Int64,
                            numData::Int64, ::Val{dimensions}) where dimensions
    clusterDensities = zeros(Float64, numClusters)
    clusterSizes = zeros(Float64, numClusters)
    cellCounters = zeros(Int64, numClusters)

    # the normalization factor γ
    γ = if dimensions > 2
            Float64(dimensions / (2*resolution^(dimensions-2)))
        else
            1.0
        end

    # calculate sizes for all clusters
    for i in eachindex(clusterTensor)
        currentCluster = clusterTensor[i]
        if currentCluster > 0 && currentCluster <= numClusters
            clusterSizes[currentCluster] += countTensor[i]
            cellCounters[currentCluster] += 1
        end
    end

    # calculate densities and normalize
    # the tensors can have different sizes but we take the maximal possible cell size
    maxNumOfCells = Float64(resolution^dimensions)
    # calculate the density of the dataset
    datasetDensity = numData / maxNumOfCells
    for cluster in 1:numClusters
        if cellCounters[cluster] > 0 # safe guard to avoid division by zero
            # calculate the cluster density in counts per volume
            clusterDensities[cluster] = clusterSizes[cluster] / cellCounters[cluster]
            # normalization
            clusterDensities[cluster] = (clusterDensities[cluster] / datasetDensity) * γ
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
function IteridenseLoop(dataMatrix,
                        density::Float64,
                        minClusters::Int64,
                        minClusterSize::Int64,
                        resolution::Int64,
                        maxResolution::Int64,
                        stopResolution::Int64,
                        minClusterDensity::Float64,
                        useDensity::Bool,
                        useClusters::Bool,
                        noDiagonals::Bool,
                        omitEmptyCells::Bool,
                        useFixedResolution::Bool,
                        minVector::Vector{Float64}, maxVector::Vector{Float64},
                        numData::Int64,
                        ::Val{dimensions}) where dimensions
    # initializations
    countTensor::Union{Nothing, Array{Int32}} = nothing
    clusterTensor::Union{Nothing, Array{Int32}} = nothing
    clusterDensities::Union{Nothing, Vector{Float64}} = nothing
    clusterSizes::Union{Nothing, Vector{Int64}} = nothing
    assignments = zeros(Int64, numData)
    numClusters::Int64 = 0
    initialResolution = resolution
    achievedDensity = 0.0
    # the main loop
    while resolution < maxResolution
        # countTensor will have the rank of dimensions. This means for resolution= 4 and
        # dimension= 16 it has 4^16 * 32 bit, thus 17 GB.
        # We must therefore test how much RAM is available stop the clustering if necessary.
        # get the available RAM
        availableRAM = Int64(Sys.free_memory()) # in bytes
        # we have 2 tensors (countTensor and clusterTensor), therefore * 2
        # we use Float32, which has a size of 4 bytes
        necessaryRAM = resolution^dimensions * 4 * 2
        # We break if 95% of the availableRAM is less than necessaryRAM because we need some RAM
        # for Julia itself and other tasks the OS might perform.
        if 0.95*availableRAM < necessaryRAM
            printstyled("\nImportant Warning: "; bold= true, color= :magenta)
            printstyled("Not enough available RAM!\n\
                    \nFor the $(dimensions) dimensions there is currently only enough RAM\
                    \navailable for a resolution of \
                    $(Int64(trunc((0.475*availableRAM / 4)^(1/dimensions))))."; color= :magenta)
            if resolution-1 ≥ initialResolution
                printstyled("\nThe clustering was therefore stopped at resolution \
                            $(resolution-1).\n"; color= :magenta)
                resolution -= 1
            else
                printstyled("\nThe clustering was therefore not performed.\n";
                    color= :magenta)
                resolution = 1
                # free memory
                countTensor = nothing
                clusterTensor = nothing
            end
            if isnothing(clusterDensities)
                clusterDensities = Float64[]
            end
            if isnothing(clusterSizes)
                clusterSizes = Int[]
            end
            return IteridenseResult(clusterTensor= clusterTensor, countTensor= countTensor,
                                    numOfClusters= numClusters, finalResolution= resolution,
                                    assignments= assignments, clusterDensities= clusterDensities,
                                    clusterSizes= clusterSizes)
        end
        countTensor = CreateCountTensor(dataMatrix, resolution, numData, minVector, maxVector,
                                        omitEmptyCells, Val(dimensions))
        numClusters, clusterTensor = InternalClustering(countTensor, resolution, noDiagonals,
                                                        Val(dimensions))
        # clusters can only be analyzed if there is at least one
        if numClusters == 0
            resolution += 1
            continue
        end
        clusterDensities, clusterSizes = AnalyzeClusters(clusterTensor, countTensor, numClusters,
                                                        resolution, numData, Val(dimensions))
        
        # remove clusters smaller than minClusterSize or with density < minClusterDensity
        # One might think that first collect the clusters to be deleted then perform a single
        # run over the clusterTensor in which these cluster numbers are set to zero is a good
        # strategy but it turned out that even for 30 clusters to be deleted this approach is
        # way faster:
        for cluster in 1:numClusters
            if clusterSizes[cluster] < minClusterSize ||
                clusterDensities[cluster] < minClusterDensity
                renumberClusters!(clusterTensor, currentNumber= cluster,
                                    newNumber= 0)
            end
        end

        # due the cluster removal we will end up with non-sequent cluster
        # numbering where e.g. there are cells in cluster 2 and 4 but not in cluster 1 and 3
        # to avoid that we have to rename/remove the clusters
        numClusters, clusterTensor = removeEmptyClusters!(clusterTensor, numClusters)
        # clusters can only be analyzed if there is at least one
        if numClusters == 0
            resolution += 1
            continue
        end
        # since we merged clusters, analyze them again
        clusterDensities, clusterSizes = AnalyzeClusters(clusterTensor, countTensor, numClusters,
                                                        resolution, numData, Val(dimensions))
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

    # if no clustering was performed we want the output "Any[]"
    if isnothing(countTensor)
        countTensor = Array{Any}(undef)
    end
    if isnothing(clusterTensor)
        clusterTensor = Array{Any}(undef)
    end

    # if the previous while loop was ended because resolution ≥ maxResolution
    # the resolution is too high and has to be reset
    if resolution ≥ maxResolution || resolution > stopResolution
        resolution = size(clusterTensor, 1)
    end

    return IteridenseResult(clusterTensor= clusterTensor, countTensor= countTensor,
                            numOfClusters= numClusters, finalResolution= resolution,
                            assignments= assignments, clusterDensities= clusterDensities,
                            clusterSizes= clusterSizes)
end


#-------------------------------------------------------------------------------------------------
# function to assign data points to clusters
function AssignPoints(dataMatrix, clusterTensor, resolution::Int64, minVector::Vector{Float64},
                        maxVector::Vector{Float64}, numData::Int64, omitEmptyCells::Bool,
                        ::Val{dimensions}) where dimensions

    assignments = zeros(Int64, numData)
    # tuple to later reverse the dimension order
    reverseDims = ntuple(i -> dimensions-i+1, dimensions)
    # calculate the cell size for every dimension
    sizeVector = (maxVector .- minVector) ./ resolution
    # take the inverse to later be able to multiply
    inverseSizeVector = 1.0 ./ sizeVector
    # add a small value to minVector for numerical reasons, see below
    offsetVector = minVector .+ smallValue

    # depending on omitEmptyCells we have 2 different assignments
    if omitEmptyCells
        # get point assignments
        cellAssigns, countTensorDims = cellAssignments(dataMatrix, inverseSizeVector,
                                                        offsetVector, numData, Val(dimensions))
        # assign every data point
        for point in 1:size(dataMatrix, 1)
            idx = CartesianIndex(ntuple(i -> cellAssigns[point, reverseDims[i]], Val(dimensions)))
            assignments[point] = clusterTensor[idx]
        end
    else
        # assign every data point
        for point in 1:numData
            # Julia swaps in matrices x and y, thus reverse to use the coordinate system of the
            # plot.
            # Due to precision issues, we don't just subtract minVector[reverseDims[i] but
            # also a small value.
            idx = CartesianIndex(ntuple(i -> trunc(Int64, (dataMatrix[point, reverseDims[i]] -
                                    offsetVector[reverseDims[i]]) *
                                    inverseSizeVector[reverseDims[i]]) + 1, Val(dimensions)))
            assignments[point] = clusterTensor[idx]
        end
    end
    return assignments
end


#-------------------------------------------------------------------------------------------------
# for C we must fix the dimensions to a maximal value
# 256 dimensions should be sufficient
const MAX_DIMENSIONS = 256

# C-compatible tensor struct
struct CTensor
    data::Ptr{Cvoid}          # pointer to data buffer
    ndims::Clonglong          # number of dimensions
    dims::NTuple{MAX_DIMENSIONS, Csize_t} # sizes of each dimension, padded with zeros
end

# C-compatible struct to be used for a vector (1D tensor)
struct CArray
    data::Ptr{Cvoid}       # pointer to data buffer
    length::Csize_t        # number of elements 
end


#-------------------------------------------------------------------------------------------------
# C-compatible IteridenseResult struct
struct IteridenseResultC
    clusterTensor::CTensor
    countTensor::CTensor
    numOfClusters::Clonglong
    finalResolution::Clonglong
    assignments::CArray
    clusterDensities::CArray
    clusterSizes::CArray
end


#-------------------------------------------------------------------------------------------------
# main function
function PerformClustering(dataMatrix;
                            density::Float64= 1.1,
                            minClusters::Int64= 1,
                            minClusterSize::Int64= 3,
                            startResolution::Int64= 2,
                            stopResolution::Int64= 64,
                            minClusterDensity::Float64= 0.0,
                            useDensity::Bool= true,
                            useClusters::Bool= false,
                            noDiagonals::Bool= false,
                            omitEmptyCells::Bool= false )

    # count data points and determine the dimensions
    numData::Int64 = size(dataMatrix, 1)
    dimensions::Int64 = size(dataMatrix, 2)
    # dataMatrix is a matrix in which every column contains the data of a dimension
    # therefore store the min/max of every dimension in vectors
    minVector = Float64.([minimum(col) for col in eachcol(dataMatrix)])
    maxVector = Float64.([maximum(col) for col in eachcol(dataMatrix)])

    # assure to have sensible inputs
    if minClusterSize < 2
        minClusterSize = 2
    end
    if startResolution < 2
        startResolution = 2
    end
    if density < 0.0
        density = 0.0
    end
    # minClusterSize cannot be greater than numData - 1
    if minClusterSize ≥ numData
        minClusterSize = numData - 1
    end
    # numData is maximal possible resolution
    if startResolution > numData
        startResolution = numData
    end
    if stopResolution > numData
        stopResolution = numData
    end
    if stopResolution < startResolution
        stopResolution = startResolution
    end
    if minClusters < 1
        minClusters = 1
    end
    if minClusterDensity < 0.0
        minClusterDensity = 0.0
    end
    # minClusterDensity must not be greater than the density
    if minClusterDensity > density
        minClusterDensity = density
    end
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

    # initializations
    resolution::Int64 = startResolution
    clusterDensities::Union{Nothing, Vector{Float64}} = nothing
    clusterSizes::Union{Nothing, Vector{Int64}} = nothing
    # assure that maximal resolution is used as limit
    maxResolution = numData
    if stopResolution > maxResolution
        stopResolution = maxResolution
    end
    maxResolution = stopResolution + 1
    if useFixedResolution
        if startResolution > maxResolution
            startResolution = maxResolution - 1
        end
        resolution = startResolution
        maxResolution = startResolution + 1
    end

    # the main clustering loop
    LoopResult = IteridenseLoop(dataMatrix,
                                density,
                                minClusters,
                                minClusterSize,
                                resolution,
                                maxResolution,
                                stopResolution,
                                minClusterDensity,
                                useDensity,
                                useClusters,
                                noDiagonals,
                                omitEmptyCells,
                                useFixedResolution,
                                minVector, maxVector,
                                numData,
                                Val(dimensions) )
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
        if LoopResult.finalResolution > 1 # if there was a clustering
            printstyled("\nInformation: "; bold= true, color= :blue)
            println("The clustering detected no clusters")
        end
        # if no clustering was performed there is no countTensor
        if LoopResult.countTensor === nothing
            countTensorResult= Int32[]
        else
            countTensorResult= LoopResult.countTensor
        end
        noClusterResult = IteridenseResultC(ArrayToCTensor(Int32[], Int32),
                                            ArrayToCTensor(countTensorResult, Int32),
                                            Clonglong(0),
                                            Clonglong(LoopResult.finalResolution),
                                            ArrayToCArray(zeros(Int64, numData), Int64),
                                            ArrayToCArray(Float64[], Float64),
                                            ArrayToCArray(Int64[], Int64) )
        # trigger a garbage collection
        GC.gc()
        return noClusterResult
    else
        # run a single loop with a higher fixed resolution
        secondResolution = LoopResult.finalResolution + 1
        LoopResultSecond = IteridenseLoop(dataMatrix,
                                            density,
                                            minClusters,
                                            minClusterSize,
                                            secondResolution, # new resolution
                                            maxResolution,
                                            stopResolution,
                                            minClusterDensity,
                                            useDensity,
                                            useClusters,
                                            noDiagonals,
                                            omitEmptyCells,
                                            true,             # use fixed resolution
                                            minVector, maxVector,
                                            numData,
                                            Val(dimensions) )
    end

    # assign the data points according to the first clustering result
    assignments = AssignPoints(dataMatrix, LoopResult.clusterTensor,
                                LoopResult.finalResolution, minVector, maxVector,
                                numData, omitEmptyCells, Val(dimensions))

    intermediateResult = IteridenseResultC(ArrayToCTensor(LoopResult.clusterTensor, Int32),
                                ArrayToCTensor(LoopResult.countTensor, Int32),
                                Clonglong(LoopResult.numOfClusters),
                                Clonglong(LoopResult.finalResolution),
                                ArrayToCArray(assignments, Int64),
                                ArrayToCArray(LoopResult.clusterDensities, Float64),
                                ArrayToCArray(LoopResult.clusterSizes, Int64) )

    # if the number of clusters did change between the 2 runs, return the assignment according
    # to the first clustering result
    if LoopResult.numOfClusters != LoopResultSecond.numOfClusters || useFixedResolution
        # trigger a garbage collection
        GC.gc()
        return intermediateResult
    else
        assignmentsSecond = AssignPoints(dataMatrix, LoopResultSecond.clusterTensor,
                                            LoopResultSecond.finalResolution,
                                            minVector, maxVector, numData,
                                            omitEmptyCells, Val(dimensions))
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
        end
    end
    # For the second item, we subtract both assignment vectors, set negative values in the
    # result to zero and then add this to assignmentsSecond.
    assignmentsResult = max.(assignments .- assignmentsSecond, 0)
    assignmentsResult = assignmentsResult .+ assignmentsSecond

    # to update clusterSizes we don't need to update and evaluate the clusterTensor
    # but count in assignmentsResult
    # we have to count the zero clusters too
    clusterCounts = zeros(Int64, LoopResult.numOfClusters + 1)
    for num in assignmentsResult
        clusterCounts[num+1] += 1
    end
    # cut off the zero clusters
    clusterCounts = clusterCounts[2:end]
    clusterSizes = reshape(clusterCounts, :)
    # For the clusterDensities it is not straight-forward since there are different resolutions.
    # We therefore simply take the mean of the 2.
    clusterDensities = (LoopResult.clusterDensities .+
                        LoopResultSecond.clusterDensities) ./ 2

    # for the final result we output the tensors and resolution of the first result
    result = IteridenseResultC(ArrayToCTensor(LoopResult.clusterTensor, Int32),
                                ArrayToCTensor(LoopResult.countTensor, Int32),
                                Clonglong(LoopResult.numOfClusters),
                                Clonglong(LoopResult.finalResolution),
                                ArrayToCArray(assignmentsResult, Int64),
                                ArrayToCArray(clusterDensities, Float64),
                                ArrayToCArray(clusterSizes, Int64) )
    # trigger a garbage collection
    GC.gc()
    return result
end


#-------------------------------------------------------------------------------------------------
# helper to allocate and copy a Julia array to an allocated C buffer
# as it will be called more often, it is worth to @inline it
@inline function AllocateAndCopy(anArray::AbstractArray{dataType}) where dataType
    arrayLength = length(anArray)
    buffer = Libc.malloc(arrayLength * sizeof(dataType))
    if buffer == C_NULL
        error("allocation failed!")
    end
    unsafe_copyto!(Ptr{dataType}(buffer), pointer(anArray), arrayLength)
    return buffer
end

# convert Julia array to CTensor struct with allocated data buffer
function ArrayToCTensor(anArray, ::Type{dataType}) where dataType
    # first convert the array explicitly to the give data type 
    convertedArray = convert(Array{dataType}, anArray)
    numDimensions = ndims(convertedArray)
    # pad with zeros if numDimensions < MAX_DIMENSIONS
    dimensions = ntuple(i -> i <= numDimensions ? size(convertedArray, i) : 0, MAX_DIMENSIONS)
    dataPointer = AllocateAndCopy(convertedArray)
    return CTensor(dataPointer, Clonglong(numDimensions), dimensions)
end

# convert Julia array to CArray struct with allocated data buffer
function ArrayToCArray(aVector, ::Type{dataType}) where dataType
    convertedVector = convert(Vector{dataType}, aVector)
    dataPointer = AllocateAndCopy(convertedVector)
    return CArray(dataPointer, Csize_t(length(convertedVector)))
end


#-------------------------------------------------------------------------------------------------
# the C wrapper function for Clustering()
Base.@ccallable function IteridenseClustering(
    dataMatrix::Ptr{Float64},
    nrows::Clonglong,
    ncols::Clonglong,
    density::Cdouble,
    minClusters::Clonglong,
    minClusterSize::Clonglong,
    startResolution::Clonglong,
    stopResolution::Clonglong,
    minClusterDensity::Cdouble,
    useDensity::Clonglong,      # bool as int (0 or 1)
    useClusters::Clonglong,
    noDiagonals::Clonglong,
    omitEmptyCells::Clonglong )::Ptr{IteridenseResultC}
    
    # allocate memory for the uninitialized struct
    resultPointer = Ptr{IteridenseResultC}(Libc.malloc(sizeof(IteridenseResultC)))
    if resultPointer == C_NULL
        return C_NULL
    end
    # wrap the raw pointer into a Julia Array without copying the data
    # Julia arrays are column-major, so shape is (nrows, ncols)
    # NOTE: own= false is crucial since Julia must not own the memory to assure that is is not
    #  free'd by Julia's garbage collector. The owner is the C-caller.
    data = unsafe_wrap(Array, dataMatrix, (nrows, ncols); own= false)

    # perform the clustering
    result = PerformClustering(data;
        density = Float64(density),
        minClusters = Int64(minClusters),
        minClusterSize = Int64(minClusterSize),
        startResolution = Int64(startResolution),
        stopResolution = Int64(stopResolution),
        minClusterDensity = Float64(minClusterDensity),
        useDensity = useDensity != 0,
        useClusters = useClusters != 0,
        noDiagonals = noDiagonals != 0,
        omitEmptyCells = omitEmptyCells != 0
    )

    # write result into allocated memory
    unsafe_store!(resultPointer, result)

    return resultPointer
end


#-------------------------------------------------------------------------------------------------
# function to free the memory allocated by IteridenseClustering
Base.@ccallable function IteridenseFree(resultPointer::Ptr{IteridenseResultC})::Cint
    if resultPointer == C_NULL
        return -1
    end
    # read the struct to get pointers
    result = unsafe_load(resultPointer)
    # free tensor data buffer
    if result.clusterTensor.data != C_NULL
        Libc.free(result.clusterTensor.data)
    end
    if result.countTensor.data != C_NULL
        Libc.free(result.countTensor.data)
    end
    # free array data buffer
    if result.assignments.data != C_NULL
        Libc.free(result.assignments.data)
    end
    if result.clusterDensities.data != C_NULL
        Libc.free(result.clusterDensities.data)
    end
    if result.clusterSizes.data != C_NULL
        Libc.free(result.clusterSizes.data)
    end
    Libc.free(resultPointer)
    return 0
end

#-------------------------------------------------------------------------------------------------
# function to call Julia's garbage collector
Base.@ccallable function GarbageCollection()::Cint
    GC.gc()
    return 0
end


#-------------------------------------------------------------------------------------------------
# function to get size of currently available memory
Base.@ccallable function FreeMemoryInBytes()::Culonglong
    return Int(Sys.free_memory())
end


#-------------------------------------------------------------------------------------------------
# C-compatible DBSCANResult struct
struct DBSCANResultC
    numOfClusters::Clonglong
    assignments::CArray
    clusterSizes::CArray
end


#-------------------------------------------------------------------------------------------------
# the C wrapper function for Clustering.dbscan
Base.@ccallable function DBSCANClustering(
    dataMatrix::Ptr{Float64},
    nrows::Clonglong,
    ncols::Clonglong,
    radius::Cdouble,
    minNeighbors::Clonglong,
    minClusterSize::Clonglong,
    )::Ptr{DBSCANResultC}
    
    # allocate memory for the uninitialized struct
    resultPointer = Ptr{DBSCANResultC}(Libc.malloc(sizeof(DBSCANResultC)))
    if resultPointer == C_NULL
        return C_NULL
    end
    # wrap the raw pointer into a Julia Array without copying the data
    # Julia arrays are column-major, so shape is (nrows, ncols)
    # NOTE: own= false is crucial since Julia must not own the memory to assure that is is not
    #  free'd by Julia's garbage collector. The owner is the C-caller.
    data = unsafe_wrap(Array, dataMatrix, (nrows, ncols); own= false)

    # perform the clustering
    result = Clustering.dbscan(data',
                                radius,
                                min_neighbors= minNeighbors,
                                min_cluster_size= minClusterSize)
    assign = zeros(Int64, 0)
    assign = Clustering.assignments(result)
    clusterCounts = zeros(Int64, 0)
    clusterCounts = Clustering.counts(result)
    numOfClusters::Int64 = length(clusterCounts)

    output = DBSCANResultC(Clonglong(numOfClusters),
                            ArrayToCArray(assign, Int64),
                            ArrayToCArray(clusterCounts, Int64) )

    # write output into allocated memory
    unsafe_store!(resultPointer, output)

    return resultPointer
end


#-------------------------------------------------------------------------------------------------
# function to free the memory allocated by DBSCANClustering
Base.@ccallable function DBSCANFree(resultPointer::Ptr{DBSCANResultC})::Cint
    if resultPointer == C_NULL
        return -1
    end
    # read the struct to get pointers
    result = unsafe_load(resultPointer)
    # free array data buffer
    if result.assignments.data != C_NULL
        Libc.free(result.assignments.data)
    end
    if result.clusterSizes.data != C_NULL
        Libc.free(result.clusterSizes.data)
    end
    Libc.free(resultPointer)
    return 0
end


#-------------------------------------------------------------------------------------------------
# C-compatible DBSCANResult struct
struct KMeansResultC
    numOfClusters::Clonglong
    assignments::CArray
    clusterSizes::CArray
    clusterCenters::CTensor
end


#-------------------------------------------------------------------------------------------------
# the C wrapper function for Clustering.kmeans
Base.@ccallable function KMeansClustering(
    dataMatrix::Ptr{Float64},
    nrows::Clonglong,
    ncols::Clonglong,
    numOfClusters::Clonglong,
    maxIter::Clonglong,
    tolerance::Cdouble,
    )::Ptr{KMeansResultC}
    
    # allocate memory for the uninitialized struct
    resultPointer = Ptr{KMeansResultC}(Libc.malloc(sizeof(KMeansResultC)))
    if resultPointer == C_NULL
        return C_NULL
    end
    # wrap the raw pointer into a Julia Array without copying the data
    # Julia arrays are column-major, so shape is (nrows, ncols)
    # NOTE: own= false is crucial since Julia must not own the memory to assure that is is not
    #  free'd by Julia's garbage collector. The owner is the C-caller.
    data = unsafe_wrap(Array, dataMatrix, (nrows, ncols); own= false)

    # perform the clustering
    result = Clustering.kmeans(data',
                                numOfClusters,
                                maxiter= maxIter,
                                tol= tolerance,
                                # display is not exported as we don't need process messages
                                display= :none
                                )
    assign = zeros(Int64, 0)
    clusterCenters = zeros(Float64, 0)
    assign = Clustering.assignments(result)
    clusterCounts = zeros(Int64, 0)
    clusterCounts = Clustering.counts(result)
    clusterCenters = result.centers' # the output 2 x numClusters, thus transpose
    numOfClusters::Int64 = length(clusterCounts)

    output = KMeansResultC(Clonglong(numOfClusters),
                            ArrayToCArray(assign, Int64),
                            ArrayToCArray(clusterCounts, Int64),
                            ArrayToCTensor(clusterCenters, Float64) )

    # write output into allocated memory
    unsafe_store!(resultPointer, output)

    return resultPointer
end


#-------------------------------------------------------------------------------------------------
# function to free the memory allocated by KMeansClustering
Base.@ccallable function KMeansFree(resultPointer::Ptr{KMeansResultC})::Cint
    if resultPointer == C_NULL
        return -1
    end
    # read the struct to get pointers
    result = unsafe_load(resultPointer)
    # free array data buffer
    if result.assignments.data != C_NULL
        Libc.free(result.assignments.data)
    end
    if result.clusterSizes.data != C_NULL
        Libc.free(result.clusterSizes.data)
    end
    Libc.free(resultPointer)
    return 0
end


end # end of module
