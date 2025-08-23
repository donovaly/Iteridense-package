#ifndef ITERIDENSE_H
#define ITERIDENSE_H

#include <stddef.h>  // for size_t
#include <stdint.h>  // for int32_t, int64_t
#include <stdbool.h> // for bool

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_DIMENSIONS 256

// C-compatible tensor struct
typedef struct {
    void* data;                  // pointer to data buffer
    int64_t ndims;               // number of dimensions
    size_t dims[MAX_DIMENSIONS]; // sizes of each dimension, padded with zeros
} CTensor;

// C-compatible array (1D tensor) struct
typedef struct {
    void* data;                  // pointer to array data buffer
    size_t length;               // length of array
} CArray;

// C-compatible IteridenseResult struct
typedef struct {
    CTensor clusterTensor;    // of type int32_t
    CTensor countTensor;      // of type int32_t
    int64_t numOfClusters;
    int64_t finalResolution;
    CArray assignments;      // of type int64_t
    CArray clusterDensities; // of type double
    CArray clusterSizes;     // of type int64_t
} IteridenseResultC;

// C-compatible DBSCANResult struct
typedef struct {
    int64_t numOfClusters;
    CArray assignments;      // of type int64_t
    CArray clusterSizes;     // of type int64_t
} DBSCANResultC;

// C-compatible KMeansResultC struct
typedef struct {
    int64_t numOfClusters;
    CArray assignments;      // of type int64_t
    CArray clusterSizes;     // of type int64_t
    CTensor clusterCenters;  // of type double
} KMeansResultC;

/**
 * Perform Iteridense clustering on a data matrix.
 * 
 * @param dataMatrix Pointer to double array (column-major) of size nrows x ncols
 * @param nrows Number of rows in dataMatrix
 * @param ncols Number of columns in dataMatrix
 * @param density Density parameter
 * @param minClusters Minimum number of clusters
 * @param minClusterSize Minimum cluster size
 * @param startResolution Starting resolution
 * @param stopResolution Stop resolution
 * @param minClusterDensity Minimum cluster density
 * @param useClusters Boolean flag (0 or 1)
 * @param useDensity Boolean flag (0 or 1)
 * @param noDiagonals Boolean flag (0 or 1)
 * 
 * @return Pointer to an IteridenseResultC struct allocated on the heap
 *         Must be freed by calling IteridenseFree()
 */
IteridenseResultC* IteridenseClustering(
    const double* dataMatrix,
    size_t nrows,
    size_t ncols,
    double density,
    int64_t minClusters,
    int64_t minClusterSize,
    int64_t startResolution,
    int64_t stopResolution,
    double minClusterDensity,
    int64_t useDensity,
    int64_t useClusters,
    int64_t noDiagonals
);

/**
 * Free the memory allocated by IteridenseClustering
 * 
 * @param pointer Pointer returned by IteridenseClustering
 * @return 0 on success, -1 if pointer is NULL
 */
int IteridenseFree(IteridenseResultC* pointer);

/**
 * Call Julia's garbage collection
 */
int GarbageCollection();

/**
 * Get size of currently available memory
 */
uint64_t FreeMemoryInBytes();

/**
 * Perform DBSCAN clustering on a data matrix
 * 
 * @param dataMatrix Pointer to double array (column-major) of size nrows x ncols
 * @param nrows Number of rows in dataMatrix
 * @param ncols Number of columns in dataMatrix
 * @param radius Neighborhood radius; points within this distance are considered neighbors
 * @param minNeighbors Minimal number of neighbors required to assign a point to a cluster "core"
 * @param minClusterSize Minimum cluster size
 * 
 * @return Pointer to an DBSCANResultC struct allocated on the heap
 *         Must be freed by calling DBSCANFree()
 */
DBSCANResultC* DBSCANClustering(
    const double* dataMatrix,
    size_t nrows,
    size_t ncols,
    double radius,
    int64_t minNeighbors,
    int64_t minClusterSize
);

/**
 * Free the memory allocated by DBSCANClustering
 * 
 * @param pointer Pointer returned by DBSCANClustering
 * @return 0 on success, -1 if pointer is NULL
 */
int DBSCANFree(DBSCANResultC* pointer);

/**
 * Perform K-means clustering on a data matrix
 * 
 * @param dataMatrix Pointer to double array (column-major) of size nrows x ncols
 * @param nrows Number of rows in dataMatrix
 * @param ncols Number of columns in dataMatrix
 * @param numOfClusters Number of desired clusters
 * @param maxIter Maximal iterations to find a solution
 * @param tolerance Maximal tolerance of changes between two iterations to decide if a
 *                  solution converged
 * 
 * @return Pointer to an KMeansResultC struct allocated on the heap
 *         Must be freed by calling DBSCANFree()
 */
KMeansResultC* KMeansClustering(
    const double* dataMatrix,
    size_t nrows,
    size_t ncols,
    int64_t numOfClusters,
    int64_t maxIter,
    double tolerance
);

/**
 * Free the memory allocated by KMeansClustering
 * 
 * @param pointer Pointer returned by KMeansClustering
 * @return 0 on success, -1 if pointer is NULL
 */
int KMeansFree(KMeansResultC* pointer);

#ifdef __cplusplus
}
#endif

#endif // ITERIDENSE_H
