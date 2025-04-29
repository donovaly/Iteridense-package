#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include "CompilationResult\include\julia_init.h"
#include "CompilationResult\include\IteridenseCLib.h"

// Helper to print clusterTensor data (assuming 3D tensor)
void print_clusterTensor(const CTensor* tensor) {
    printf("clusterTensor ndims: %d\n", tensor->ndims);
    printf("clusterTensor dims: ");
    for (int i = 0; i < tensor->ndims; i++) {
        printf("%zu ", tensor->dims[i]);
    }
    printf("\n");

    // For demonstration, assume data is double and print first 5 elements
    double* data = (double*)tensor->data;
    size_t total_elements = 1;
    for (int i = 0; i < tensor->ndims; i++) {
        total_elements *= tensor->dims[i];
    }

    printf("clusterTensor data (first 5 elements): ");
    for (size_t i = 0; i < (total_elements < 5 ? total_elements : 5); i++) {
        printf("%f ", data[i]);
    }
    printf("\n");
}

int main(int argc, char *argv[]) {

    init_julia(argc, argv);

    // Julia matrix: dataMatrix = [[19, 23, 57] [42, 39, 34]]
    double dataMatrix[6] = {19, 23, 57, 42, 39, 34};
    size_t nrows = 3;
    size_t ncols = 2;

    // Call IteridenseClustering
    IteridenseResultC* result = IteridenseClustering(
        dataMatrix,
        nrows,
        ncols,
        3,      // minClusterSize
        2,      // startResolution
        1.1,    // density
        2,      // fixedResolution
        -1,     // stopResolution
        1,      // minClusters
        1.0,    // minClusterDensity
        0,      // noDiagonals (false)
        1,      // useDensity (true)
        0,      // useClusters (false)
        0       // useFixedResolution (false)
    );

    if (result == NULL) {
        fprintf(stderr, "IteridenseClustering failed: returned NULL\n");
        return EXIT_FAILURE;
    }

    printf("numOfClusters: %d\n", result->numOfClusters);
    printf("finalResolution: %d\n", result->finalResolution);

    print_clusterTensor(&result->clusterTensor);

    // You can similarly inspect other tensors if needed...

    // Free the allocated result
    int free_status = IteridenseFree(result);
    if (free_status != 0) {
        fprintf(stderr, "IteridenseFree failed\n");
        return EXIT_FAILURE;
    }

    printf("Test completed successfully.\n");
    shutdown_julia(0);
    return EXIT_SUCCESS;
}
