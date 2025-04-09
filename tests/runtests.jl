using Test
using CSV, DataFrames
#using Iteridense

packagePath = dirname(@__DIR__)
include(joinpath(packagePath, "src\\Iteridense.jl"))
using .Iteridense: IteridenseCluster, PlotIteridenseHeatmap

# test in 1D
println("testing 1D: Ph.D. publications")
function Phd1D()
    filePath = joinpath(packagePath, "examples\\datasets\\PhdPubs.csv")
    if isfile(filePath)
        data = CSV.read(filePath, DataFrame; delim= ',')
    else
        return error("dataset not found")
    end
    dataMatrix = Matrix(data)
    results = Iteridense.Clustering(dataMatrix[:, end], minClusterSize= 6,
                                useClusters= true, minClusters= 3)
    return [results.numOfClusters, results.finalResolution, results.clusterSizes]
end

# tests in 2D
println("testing 2D: NoisyMoons")
function NoisyMoons()
    filePath = joinpath(packagePath, "examples\\datasets\\NoisyMoons.csv")
    if isfile(filePath)
        data = CSV.read(filePath, DataFrame; delim= ',')
    else
        return error("dataset not found")
    end
    dataMatrix = Matrix(data)
    results = Iteridense.Clustering(dataMatrix, density= 2.2)
    return [results.numOfClusters, results.finalResolution, results.clusterSizes]
end

println("testing 2D: NoisyMoons")
function NoisyRandom()
    filePath = joinpath(packagePath, "examples\\datasets\\NoisyRandom.csv")
    if isfile(filePath)
        data = CSV.read(filePath, DataFrame; delim= ',')
    else
        return error("dataset not found")
    end
    dataMatrix = Matrix(data)
    results = Iteridense.Clustering(dataMatrix, density= 3.0, minClusterSize= 10)
    return [results.numOfClusters, results.finalResolution, results.clusterSizes]
end

# test heatmap
function NoisyMoonsHeatmap()
    filePath = joinpath(packagePath, "examples\\datasets\\NoisyMoons.csv")
    if isfile(filePath)
        data = CSV.read(filePath, DataFrame; delim= ',')
    else
        return error("dataset not found")
    end
    dataMatrix = Matrix(data)
    results = Iteridense.Clustering(dataMatrix, density= 2.2)
    return [results.numOfClusters, results.finalResolution, results.clusterSizes]
end

# test in 3D
println("testing 3D: Pluton")
function Pluton()
    filePath = joinpath(packagePath, "examples\\datasets\\Pluton.csv")
    if isfile(filePath)
        data = CSV.read(filePath, DataFrame; delim= '\t')
    else
        return error("dataset not found")
    end
    dataMatrix = Matrix(data)
    results = Iteridense.Clustering(dataMatrix[:, 2:end], density= 27, minClusterSize= 4);
    return [results.numOfClusters, results.finalResolution, results.clusterSizes]
end

# test in 6D
println("testing 6D: Ph.D. publications")
function PhdPubs()
    filePath = joinpath(packagePath, "examples\\datasets\\PhdPubs.csv")
    if isfile(filePath)
        data = CSV.read(filePath, DataFrame; delim= ',')
    else
        return error("dataset not found")
    end
    dataMatrix = Matrix(data)
    # cut off first column
    completeMatrix = dataMatrix[:, 2:end]
    results = Iteridense.Clustering(completeMatrix, minClusterSize= 20, useClusters= true, minClusters= 5);
    return [results.numOfClusters, results.finalResolution, results.clusterSizes]
end

# perform the tests
@testset "1D" begin
    @test Phd1D()[1] == 3
    @test Phd1D()[2] == 88
    @test Phd1D()[3] == [536  216  88]
end

@testset "2D" begin
    @test NoisyMoons()[1] == 2
    @test NoisyMoons()[2] == 12
    @test NoisyMoons()[3] == [750  749]
    @test NoisyRandom()[1] == 7
    @test NoisyRandom()[2] == 48
    @test NoisyRandom()[3] == [12  17  12  10  13  14  15]
end

@testset "3D" begin
    @test Pluton()[1] == 4
    @test Pluton()[2] == 7
    @test Pluton()[3] == [5  9  16  5]
end

@testset "6D" begin
    @test PhdPubs()[1] == 5
    @test PhdPubs()[2] == 5
    @test PhdPubs()[3] == [104  262  89  189  188]
end
