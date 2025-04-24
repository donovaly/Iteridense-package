#ifndef ITERIDENSE_H
#define ITERIDENSE_H

#include <stddef.h>  // for size_t

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_DIMS 32

// C-compatible tensor struct
typedef struct {
    void* data;               // pointer to tensor data buffer
    int ndims;                // number of dimensions
    size_t dims[MAX_DIMS];    // sizes of each dimension (padded with zeros)
} CTensor;

// C-compatible IteridenseResult struct
typedef struct {
    CTensor clusterTensor;
    CTensor countTensor;
    int numOfClusters;
    int finalResolution;
    CTensor assignments;
    CTensor clusterDensities;
    CTensor clusterSizes;
} IteridenseResultC;

/**
 * allocates and computes the IteridenseResult struct
 * returns a pointer to a heap-allocated IteridenseResultC struct
 * The caller is responsible for freeing the returned pointer by calling IteridenseFree().
 *
 * returns NULL on allocation failure
 */
IteridenseResultC* IteridenseClustering(void);

/**
 * free memory allocated by IteridenseClustering
 *
 * @param pointer pointer returned by IteridenseClustering
 * @return 0 on success, -1 if pointer is NULL.
 */
int IteridenseFree(IteridenseResultC* pointer);

#ifdef __cplusplus
}
#endif

#endif // ITERIDENSE_H
