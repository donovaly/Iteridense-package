using IteridenseCLib

function TestIteridenseClustering()
    pointer = CreateTensorStruct()
    if pointer == C_NULL
        error("Failed to allocate IteridenseResultC")
    end

    # load the struct
    result = unsafe_load(pointer)

    #println("numOfClusters: ", result.numOfClusters)
    #println("finalResolution: ", result.finalResolution)

    # inspect clusterTensor
    clusterTensorDims = Tuple(result.clusterTensor.dims[1:result.clusterTensor.ndims])
    clusterTensorArray = unsafe_wrap(Array, Ptr{Float64}(result.clusterTensor.data),
                                        clusterTensorDims)
    #println("clusterTensor dimensions: ", map(Int, clusterTensorDims))
    #println("clusterTensor data[1, 1, 1]: ", clusterTensorArray[1, 1, 1])
    #println("clusterTensor complete: ", clusterTensorArray)
    tensor = clusterTensorArray

    # inspect assignments
    assignmentsDims = Tuple(result.assignments.dims[1:result.assignments.ndims])
    assignmentsArray = unsafe_wrap(Array, Ptr{Int32}(result.assignments.data), assignmentsDims)
    #println("assignments dimensions: ", map(Int, assignmentsDims))
    #println("assignments data: ", assignmentsArray)

    # free allocated memory
    IteridenseFree(pointer)

    return tensor
end

tensor = []
for i in 1:5
    global tensor = TestIteridenseClustering()
end

println("complete tensor: $tensor")
