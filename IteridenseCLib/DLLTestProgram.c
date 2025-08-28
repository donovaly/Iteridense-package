#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include "CompilationResult\include\julia_init.h"
#include "CompilationResult\include\IteridenseCLib.h"

// Helper to print clusterTensor data (assuming 3D tensor)
typedef enum {
    TYPE_INT32,
    TYPE_DOUBLE
} DataType;

void printTensor(const CTensor* tensor, const char* name, DataType type) {
    printf("%s ndims: %d\n", name, tensor->ndims);
    printf("%s dims: ", name);
    size_t total_elements = 1;
    for (int i = 0; i < tensor->ndims; i++) {
        printf("%zu ", tensor->dims[i]);
        total_elements *= tensor->dims[i];
    }
    printf("\n");

    printf("%s data: ", name);
    if (type == TYPE_INT32) {
        int32_t* data = (int32_t*)tensor->data;
        for (size_t i = 0; i < total_elements; i++) {
            printf("%ld ", data[i]);
        }
    } else if (type == TYPE_DOUBLE) {
        double* data = (double*)tensor->data;
        for (size_t i = 0; i < total_elements; i++) {
            printf("%f ", data[i]);
        }
    } else {
        printf("Unsupported data type");
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
    IteridenseResultC* resultIteridense = IteridenseClustering(
        dataMatrix,
        nrows,
        ncols,
        1.1,    // density
        1,      // minClusters
        3,      // minClusterSize
        2,      // startResolution
        -1,     // stopResolution
        1.0,    // minClusterDensity
        1,      // useDensity (true)
        0,      // useClusters (false)
        0,      // noDiagonals (false)
        0       // omitEmptyCells (false)
    );

    if (resultIteridense == NULL) {
        fprintf(stderr, "IteridenseClustering failed: returned NULL\n");
        return EXIT_FAILURE;
    }

    printf("numOfClusters: %d\n", resultIteridense->numOfClusters);
    printf("finalResolution: %d\n", resultIteridense->finalResolution);
    printTensor(&resultIteridense->clusterTensor, "clusterTensor", TYPE_INT32);
    printTensor(&resultIteridense->countTensor, "countTensor", TYPE_INT32);

    // Free the allocated resultIteridense
    int free_statusIteridense = IteridenseFree(resultIteridense);
    if (free_statusIteridense != 0) {
        fprintf(stderr, "IteridenseFree failed\n");
        return EXIT_FAILURE;
    }

    // call garbage collection
    GarbageCollection();

    // get size of currently available memory
    printf("size of currently available memory: %d\n", FreeMemoryInBytes());

    // Call DBSCANClustering
    DBSCANResultC* resultDBSCAN = DBSCANClustering(
        dataMatrix,
        nrows,
        ncols,
        8.0,    // radius
        1,      // minNeighbors
        1       // minClusterSize
    );

    if (resultDBSCAN == NULL) {
        fprintf(stderr, "DBSCANClustering failed: returned NULL\n");
        return EXIT_FAILURE;
    }

    printf("DBSCAN numOfClusters: %d\n", resultDBSCAN->numOfClusters);

    // Free the allocated resultDBSCAN
    int free_statusDBSCAN = DBSCANFree(resultDBSCAN);
    if (free_statusDBSCAN != 0) {
        fprintf(stderr, "DBSCANFree failed\n");
        return EXIT_FAILURE;
    }

    // Call KMeansClustering
    KMeansResultC* resultKMeans = KMeansClustering(
        dataMatrix,
        nrows,
        ncols,
        2,    // numOfClusters
        1,    // maxIter
        1e-4  // tolerance
    );

    if (resultKMeans == NULL) {
        fprintf(stderr, "KMeansClustering failed: returned NULL\n");
        return EXIT_FAILURE;
    }

    printf("K-means numOfClusters: %d\n", resultKMeans->numOfClusters);
    printTensor(&resultKMeans->clusterCenters, "clusterCenters", TYPE_DOUBLE);

    // Free the allocated resultKMeans
    int free_statusKMeans = KMeansFree(resultKMeans);
    if (free_statusKMeans != 0) {
        fprintf(stderr, "KMeansFree failed\n");
        return EXIT_FAILURE;
    }

    printf("Test completed successfully.\n");
    shutdown_julia(0);
    return EXIT_SUCCESS;
}
