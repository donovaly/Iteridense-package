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


#-------------------------------------------------------------------------------------------------
# function to merge and to renumber clusters
function mergeRenumberClusters!(clusterTensor; currentNumber::Int64, newNumber::Int64)
    for k in eachindex(clusterTensor)
        if clusterTensor[k] == currentNumber
            clusterTensor[k] = newNumber
        end
    end
end


#-------------------------------------------------------------------------------------------------
# function to get valid neighbors for a given index and dimension
function getNeighbors!(maxIdxRange, dim::Int64, CheckIdxTuple, neighborIndices, noDiagonals,
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
            # there are further dimensions to be checked, thus call getNeighbors! recursively
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
function checkNeighbors(clusterTensor, currentIdx, numClusters::Int64, maxIdxRange, dimOrder,
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
                CheckIdxTuple = ntuple(i -> i == dim ? checkDimIdx : currentIdx[i],
                                        Val(dimensions))
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
        numClusters = Int64(maximum(clusterTensor)) + 1
        clusterTensor[currentIdx...] = numClusters
    end
    # assure we return the currently highest cluster number
    numClusters = Int64(maximum(clusterTensor))

    return numClusters, clusterTensor
end


#-------------------------------------------------------------------------------------------------
# function determine the number of points within the cells
function CreateCountTensor(dataMatrix, resolution::Int64, minMatrix::Vector, maxMatrix::Vector,
                            ::Val{dimensions}) where dimensions
    if !(length(minMatrix) == length(maxMatrix) == size(dataMatrix, 2) == dimensions)
        error("Dimensions mismatch")
    end
    # at first calculate the cell size for every dimension
    sizeMatrix = ntuple(i -> (maxMatrix[i] - minMatrix[i]) / resolution, Val(dimensions))
    # we need now a tensor in that has resolution entries for every dimension
    countTensor = zeros(Int32, ntuple(i -> resolution, Val(dimensions)))

    # create a matrix with coordinates in our grid
    # every of its columns will get the coordinate values for the particular data point
    # the countTensor is increased at the coordinates of the data point
    smallValue = 1e-6
    for k in axes(dataMatrix, 1)
        # Julia swaps in matrices x and y, thus reverse to use the coordinate system of the plot
        # due to precision issues, we subtract a small value from the values
        coordMatrix = ntuple(i -> trunc(Int32, (dataMatrix[k, dimensions-i+1] - smallValue -
                                                minMatrix[dimensions-i+1]) /
                                            sizeMatrix[dimensions-i+1]) + 1, Val(dimensions))
        countTensor[coordMatrix...] += 1
    end
    return countTensor
end


#-------------------------------------------------------------------------------------------------
# function to remove empty clusters
function removeEmptyClusters!(clusterTensor, numClusters::Int64)
    # evaluate first every cluster
    isEmpty = zeros(Int64, numClusters)
    for n in 1:numClusters
        if count(i -> (i == n), clusterTensor) == 0
            isEmpty[n] = 1
        end
    end
    # now delete the empty clusters
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
function InternalClustering(countTensor, resolution::Int64, noDiagonals,
                            ::Val{dimensions}) where dimensions
    # first create a tensor to store later the information about the clusters
    clusterTensor = zeros(Int32, ntuple(i -> resolution, Val(dimensions)))

    numClusters = 0
    # to later check the maximal index range of the clusterTensor
    maxIdxRange = 1:resolution
    # to later check neighbors dimension by dimension in reverse order
    dimOrder = reverse(1:dimensions)
    # check neighbors for all cells of the countTensor
    iterRange = Iterators.product(ntuple(_ -> 1:resolution, Val(dimensions))...)
    for indices in iterRange
        # only if a cell has more than one data point it can be part of a cluster
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
function AnalyzeClusters(clusterTensor, countTensor, numClusters::Int64, resolution::Int64,
                            totalCounts::Int64, ::Val{dimensions}) where dimensions
    numOfCells = length(countTensor)
    clusterDensities = zeros(Float64, numClusters)
    clusterSizes = zeros(Int64, numClusters)
    # the normalization factor γ
    if dimensions > 2
        γ = dimensions / (2*resolution^(dimensions-2))
    else
        γ = 1.0
    end
    for cluster in 1:numClusters
        cellCounter = 0
        # first sum values of the cluster cells
        for i in eachindex(clusterTensor)
            if clusterTensor[i] == cluster
                # we count using clusterDensities and not clusterSizes
                # because countTensor is 32 bit and making clusterSizes 32 bit makes it
                # complicated for the C library and leaving clusterSizes with 64 bit would slow
                # down the loop significantly
                clusterDensities[cluster] += countTensor[i]
                cellCounter += 1
            end
        end
        clusterSizes[cluster] = clusterDensities[cluster]
        # now calculate the cluster density in counts per volume
        clusterDensities[cluster] = clusterSizes[cluster] / cellCounter
        # normalize the density
        clusterDensities[cluster] = clusterDensities[cluster] / (totalCounts / numOfCells) * γ
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
                        useDensity,
                        useClusters,
                        noDiagonals,
                        useFixedResolution,
                        minMatrix, maxMatrix,
                        totalCounts::Int64,
                        ::Val{dimensions}) where dimensions
    # initializations
    countTensor = Array{Any}(undef)
    clusterTensor = Array{Any}(undef)
    clusterDensities = Array{Any}(undef)
    clusterSizes = Array{Any}(undef)
    assignments = zeros(Int64, totalCounts)
    numClusters::Int64 = 0
    initialResolution = resolution
    achievedDensity = 0.0
    # the main loop
    while resolution < maxResolution
        # countTensor will have the rank of dimensions. This means for resolution= 4 and
        # dimension= 16 it has 4^16 * 32 bit, thus 17 GB.
        # We must therefore test how much RAM is available stop the clustering if necessary.
        # get the available RAM
        availableRAM = Int(Sys.free_memory()) # in bytes
        # we have 2 tensors (countTensor and clusterTensor), therefore * 2, in byte
        # we use Float32, which has a size of 4 bytes
        necessaryRAM = resolution^dimensions * 4 * 2
        # We break if 95% of the availableRAM is less than necessaryRAM because we need some RAM
        # for Julia itself and other tasks the OS might perform.
        if 0.95*availableRAM < necessaryRAM
            printstyled("\nImportant Warning: "; bold= true, color= :magenta)
            printstyled("Not enough available RAM!\n\
                    \nFor the $(dimensions) dimensions there is currently only enough RAM\
                    \navailable for a resolution of \
                    $(Int(trunc((0.475*availableRAM / 4)^(1/dimensions))))."; color= :magenta)
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
            return IteridenseResult(clusterTensor= clusterTensor, countTensor= countTensor,
                                    numOfClusters= numClusters, finalResolution= resolution,
                                    assignments= assignments, clusterDensities= clusterDensities,
                                    clusterSizes= clusterSizes)
        end
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
                                                        resolution, totalCounts, Val(dimensions))
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
        # since we merged clusters, analyze them again
        clusterDensities, clusterSizes = AnalyzeClusters(clusterTensor, countTensor, numClusters,
                                                        resolution, totalCounts, Val(dimensions))
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

    return IteridenseResult(clusterTensor= clusterTensor, countTensor= countTensor,
                                numOfClusters= numClusters, finalResolution= resolution,
                                assignments= assignments, clusterDensities= clusterDensities,
                                clusterSizes= clusterSizes)
end


#-------------------------------------------------------------------------------------------------
# function to assign data points to clusters
function AssignPoints(dataMatrix, clusterTensor, resolution::Int64, minMatrix, maxMatrix,
                        totalCounts::Int64, ::Val{dimensions}) where dimensions
    # at first calculate the cell size for every dimension
    sizeMatrix = zeros(Float64, dimensions)
    for i in 1:dimensions
        sizeMatrix[i] = (maxMatrix[i] - minMatrix[i]) / resolution
    end
    # now assign every data point
    assignments = zeros(Int64, totalCounts)
    smallValue = 1e-6
    for k in axes(dataMatrix, 1)
        # Julia swaps in matrices x and y, thus reverse to use the coordinate system of the plot
        # due to precision issues, we subtract a small value from the values
        coordMatrix = ntuple(i -> trunc(Int64, (dataMatrix[k, dimensions-i+1] - smallValue -
                                                minMatrix[dimensions-i+1]) /
                                            sizeMatrix[dimensions-i+1]) + 1, Val(dimensions))
        assignments[k] = clusterTensor[coordMatrix...]
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
                            density= 1.1,
                            minClusters::Int64= 1,
                            minClusterSize::Int64= 3,
                            startResolution::Int64= 2,
                            stopResolution::Int64= 64,
                            minClusterDensity::Float64= 0.0,
                            useDensity= true,
                            useClusters= false,
                            noDiagonals= false )::IteridenseResultC

    # at first count data points and determine the dimensions
    totalCounts::Int64 = size(dataMatrix, 1)
    dimensions::Int64 = size(dataMatrix, 2)
    # dataMatrix is a matrix in which every column contains the data of a dimension
    # therefore store the min/max of every dimension in vectors
    minMatrix = zeros(Float64, dimensions)
    maxMatrix = zeros(Float64, dimensions)
    for i in 1:dimensions
        minMatrix[i] = minimum(dataMatrix[:, i])
        maxMatrix[i] = maximum(dataMatrix[:, i])
    end

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
    # minClusterSize cannot be greater than totalCounts - 1
    if minClusterSize ≥ totalCounts
        minClusterSize = totalCounts - 1
    end
    # totalCounts is maximal possible resolution
    if startResolution > totalCounts
        startResolution = totalCounts
    end
    if stopResolution > totalCounts
        stopResolution = totalCounts
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
    clusterDensities = Array{Any}(undef)
    clusterSizes = Array{Any}(undef)
    # assure that maximal resolution is used as limit
    maxResolution = totalCounts
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
                                useFixedResolution,
                                minMatrix, maxMatrix,
                                totalCounts,
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
        if typeof(LoopResult.countTensor) == Nothing
            countTensorResult= Int32[]
        else
            countTensorResult= LoopResult.countTensor
        end
        noClusterResult = IteridenseResultC(ArrayToCTensor(Int32[], Int32),
                                            ArrayToCTensor(countTensorResult, Int32),
                                            Clonglong(0),
                                            Clonglong(LoopResult.finalResolution),
                                            ArrayToCArray(zeros(Int64, totalCounts), Int64),
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
                                            true,             # use fixed resolution
                                            minMatrix, maxMatrix,
                                            totalCounts,
                                            Val(dimensions) )
    end

    # assign the data points according to the first clustering result
    assignments = AssignPoints(dataMatrix, LoopResult.clusterTensor,
                                LoopResult.finalResolution, minMatrix, maxMatrix,
                                totalCounts, Val(dimensions))

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
    noDiagonals::Clonglong )::Ptr{IteridenseResultC}
    
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
        noDiagonals = noDiagonals != 0
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
