#include(joinpath(@__DIR__, "IteridenseCLib.jl"))
#using .IteridenseCLib
using IteridenseCLib

function TestIteridenseClustering()
    # a 3x2 Float64 matrix as test
    dataMatrix = Float64.([[19, 23, 57] [42, 39, 34]])

    resultPointer = IteridenseClustering(
        pointer(dataMatrix), 3, 2,
        1.1, # density
        1,   # minClusters
        3,   # minClusterSize
        2,   # startResolution
        -1,  # stopResolution
        1.0, # minClusterDensity
        1,   # useDensity (true)
        0,   # useClusters (false)
        0    # noDiagonals (false)
    )
    if resultPointer == C_NULL
        error("Failed to allocate IteridenseResultC")
    end

    # load the struct
    result = unsafe_load(resultPointer)

    println("numOfClusters: ", result.numOfClusters)
    println("finalResolution: ", result.finalResolution)

    # inspect clusterTensor
    clusterTensorDims = Tuple(result.clusterTensor.dims[1:result.clusterTensor.ndims])
    clusterTensorArray = unsafe_wrap(Array, Ptr{Int32}(result.clusterTensor.data), clusterTensorDims)
    println("clusterTensor dimensions: ", map(Int, clusterTensorDims))
    #println("clusterTensor complete: ", clusterTensorArray)

    # inspect countTensor
    countTensorDims = Tuple(result.countTensor.dims[1:result.countTensor.ndims])
    countTensorArray = unsafe_wrap(Array, Ptr{Int32}(result.countTensor.data), countTensorDims)
    #println("countTensor dimensions: ", map(Int32, countTensorDims))
    println("countTensor complete: ", countTensorArray)

    # inspect assignments
    assignments = unsafe_wrap(Array, Ptr{Int64}(result.assignments.data),
                                result.assignments.length)
    println("assignments: ", assignments)

    # inspect clusterDensities
    clusterDensities = unsafe_wrap(Array, Ptr{Float64}(result.clusterDensities.data),
                                    result.clusterDensities.length)
    println("clusterDensities: ", clusterDensities)

    # inspect clusterSizes
    clusterSizes = unsafe_wrap(Array, Ptr{Int64}(result.clusterSizes.data),
                                result.clusterSizes.length)
    println("clusterSizes: ", clusterSizes)

    # free allocated memory
    IteridenseFree(resultPointer)

    # perform garbage collection
    GarbageCollection()
end

# run multiple times to trigger precompilation and test stability
for i in 1:5
    TestIteridenseClustering()
end


function TestDBSCNClustering()
    # a 3x2 Float64 matrix as test
    dataMatrix = Float64.([[19, 23, 57] [42, 39, 34]])

    resultPointer = DBSCANClustering(
        pointer(dataMatrix), 3, 2,
        8.0, # radius
        1,   # minNeighbors
        2    # minClusterSize
    )
    if resultPointer == C_NULL
        error("Failed to allocate DBSCANResultC")
    end

    # load the struct
    result = unsafe_load(resultPointer)

    println("numOfClusters: ", result.numOfClusters)

    # inspect assignments
    assignments = unsafe_wrap(Array, Ptr{Int64}(result.assignments.data),
                                result.assignments.length)
    println("assignments: ", assignments)

    # inspect clusterSizes
    clusterSizes = unsafe_wrap(Array, Ptr{Int64}(result.clusterSizes.data),
                                result.clusterSizes.length)
    println("clusterSizes: ", clusterSizes)

    # free allocated memory
    DBSCANFree(resultPointer)
end

# run multiple times to trigger precompilation and test stability
for i in 1:5
    TestDBSCNClustering()
end


function TestKMeansClustering()
    # a 3x2 Float64 matrix as test
    dataMatrix = Float64.([[19, 23, 57] [42, 39, 34]])

    resultPointer = KMeansClustering(
        pointer(dataMatrix), 3, 2,
        2,   # numOfClusters
        1,   # maxIter
        1e-4 # tolerance
     )
    if resultPointer == C_NULL
        error("Failed to allocate KMeansClustering")
    end

    # load the struct
    result = unsafe_load(resultPointer)

    println("numOfClusters: ", result.numOfClusters)

    # inspect assignments
    assignments = unsafe_wrap(Array, Ptr{Int64}(result.assignments.data),
                                result.assignments.length)
    println("assignments: ", assignments)

    # inspect clusterSizes
    clusterSizes = unsafe_wrap(Array, Ptr{Int64}(result.clusterSizes.data),
                                result.clusterSizes.length)
    println("clusterSizes: ", clusterSizes)

    # inspect clusterCenters
    clusterCentersDims = Tuple(result.clusterCenters.dims[1:result.clusterCenters.ndims])
    clusterCentersArray = unsafe_wrap(Array, Ptr{Float64}(result.clusterCenters.data), clusterCentersDims)
    #println("clusterCenters dimensions: ", map(Float64, clusterCentersDims))
    println("clusterCenters complete: ", clusterCentersArray)

    # free allocated memory
    KMeansFree(resultPointer)
end

# run multiple times to trigger precompilation and test stability
for i in 1:5
    TestKMeansClustering()
end
