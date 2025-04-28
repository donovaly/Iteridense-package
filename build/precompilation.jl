include(joinpath(@__DIR__, "Iteridense-C-library.jl"))
using .IteridenseCLib

function TestIteridenseClustering()
    # a 3x2 Float64 matrix as test
    dataMatrix = Float64.([[19, 23, 57] [42, 39, 34]])

    resultPointer = IteridenseClustering(
        pointer(dataMatrix), 3, 2,
        3,    # minClusterSize
        2,    # startResolution
        1.1,  # density
        2,    # fixedResolution
        -1,   # stopResolution
        1,    # minClusters
        1.0,  # minClusterDensity
        0,    # noDiagonals (false)
        1,    # useDensity (true)
        0,    # useClusters (false)
        0     # useFixedResolution (false)
    )

    if resultPointer == C_NULL
        error("Failed to allocate IteridenseResultC")
    end

    # Load the struct by value
    result = unsafe_load(resultPointer)

    #println("numOfClusters: ", result.numOfClusters)
    #println("finalResolution: ", result.finalResolution)

    # Inspect clusterTensor
    clusterTensorDims = Tuple(result.clusterTensor.dims[1:result.clusterTensor.ndims])
    clusterTensorArray = unsafe_wrap(Array, Ptr{Float64}(result.clusterTensor.data), clusterTensorDims)
    #println("clusterTensor dimensions: ", map(Int, clusterTensorDims))
    #println("clusterTensor data[1, 1, 1]: ", clusterTensorArray[1, 1, 1])
    #println("clusterTensor complete: ", clusterTensorArray)

    # Inspect assignments
    assignmentsDims = Tuple(result.assignments.dims[1:result.assignments.ndims])
    assignmentsArray = unsafe_wrap(Array, Ptr{Int32}(result.assignments.data), assignmentsDims)
    #println("assignments dimensions: ", map(Int, assignmentsDims))
    #println("assignments data: ", assignmentsArray)

    # Free allocated memory
    IteridenseFree(resultPointer)

    return clusterTensorArray
end

# Run multiple times to trigger precompilation and test stability
tensor = nothing
for i in 1:5
    global tensor = TestIteridenseClustering()
end

println("complete tensor: $tensor")
