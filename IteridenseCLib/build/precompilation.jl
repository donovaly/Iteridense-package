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

    # load the struct
    result = unsafe_load(resultPointer)

    println("numOfClusters: ", result.numOfClusters)
    println("finalResolution: ", result.finalResolution)

    # Inspect countTensor
    countTensorDims = Tuple(result.countTensor.dims[1:result.countTensor.ndims])
    countTensorArray = unsafe_wrap(Array, Ptr{Float64}(result.countTensor.data), countTensorDims)
    #println("countTensor dimensions: ", map(Int, countTensorDims))
    #println("countTensor data[1, 1, 1]: ", countTensorArray[1, 1, 1])
    #println("countTensor complete: ", countTensorArray)

    # inspect assignments
    assignmentsDims = Tuple(result.assignments.dims[1:result.assignments.ndims])
    assignmentsArray = unsafe_wrap(Array, Ptr{Int32}(result.assignments.data), assignmentsDims)
    println("assignments dimensions: ", map(Int, assignmentsDims))
    println("assignments data: ", assignmentsArray)

    # free allocated memory
    IteridenseFree(resultPointer)

    return countTensorArray
end

# Run multiple times to trigger precompilation and test stability
tensor = nothing
for i in 1:5
    global tensor = TestIteridenseClustering()
end

println("countTensor: $tensor")
