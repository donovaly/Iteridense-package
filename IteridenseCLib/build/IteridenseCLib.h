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
    CTensor clusterTensor;    // of type int64_t
    CTensor countTensor;      // of type int64_t
    int64_t numOfClusters;
    int64_t finalResolution;
    CArray assignments;      // of type int64_t
    CArray clusterDensities; // of type double
    CArray clusterSizes;     // of type int64_t
} IteridenseResultC;

/**
 * Perform clustering on a data matrix.
 * 
 * @param dataMatrix Pointer to double array (column-major) of size nrows x ncols.
 * @param nrows Number of rows in dataMatrix.
 * @param ncols Number of columns in dataMatrix.
 * @param minClusterSize Minimum cluster size.
 * @param startResolution Starting resolution.
 * @param density Density parameter.
 * @param stopResolution Stop resolution.
 * @param minClusters Minimum number of clusters.
 * @param minClusterDensity Minimum cluster density.
 * @param noDiagonals Boolean flag (0 or 1).
 * @param useDensity Boolean flag (0 or 1).
 * @param useClusters Boolean flag (0 or 1).
 * 
 * @return Pointer to an IteridenseResultC struct allocated on the heap.
 *         Must be freed by calling IteridenseFree().
 */
IteridenseResultC* IteridenseClustering(
    const double* dataMatrix,
    size_t nrows,
    size_t ncols,
    int64_t minClusterSize,
    int64_t startResolution,
    double density,
    int64_t stopResolution,
    int64_t minClusters,
    double minClusterDensity,
    int64_t noDiagonals,
    int64_t useDensity,
    int64_t useClusters
);

/**
 * Free the memory allocated by IteridenseClustering.
 * 
 * @param pointer Pointer returned by IteridenseClustering.
 * @return 0 on success, -1 if pointer is NULL.
 */
int IteridenseFree(IteridenseResultC* pointer);

#ifdef __cplusplus
}
#endif

#endif // ITERIDENSE_H
