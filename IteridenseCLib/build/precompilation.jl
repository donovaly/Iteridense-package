#include(joinpath(@__DIR__, "IteridenseCLib.jl"))
#using .IteridenseCLib
using IteridenseCLib

function TestIteridenseClustering()
    # a 3x2 Float64 matrix as test
    dataMatrix = Float64.([[19, 23, 57] [42, 39, 34]])

    resultPointer = IteridenseClustering(
        pointer(dataMatrix), 3, 2,
        3,    # minClusterSize
        2,    # startResolution
        1.1,  # density
        -1,   # stopResolution
        1,    # minClusters
        1.0,  # minClusterDensity
        0,    # noDiagonals (false)
        1,    # useDensity (true)
        0    # useClusters (false)
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
    clusterTensorArray = unsafe_wrap(Array, Ptr{Int64}(result.clusterTensor.data), clusterTensorDims)
    println("clusterTensor dimensions: ", map(Int, clusterTensorDims))
    #println("clusterTensor complete: ", clusterTensorArray)

    # inspect countTensor
    countTensorDims = Tuple(result.countTensor.dims[1:result.countTensor.ndims])
    countTensorArray = unsafe_wrap(Array, Ptr{Int64}(result.countTensor.data), countTensorDims)
    #println("countTensor dimensions: ", map(Int64, countTensorDims))
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
end

# run multiple times to trigger precompilation and test stability
for i in 1:5
    TestIteridenseClustering()
end
