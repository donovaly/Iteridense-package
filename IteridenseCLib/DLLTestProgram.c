#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include "CompilationResult\include\julia_init.h"
#include "CompilationResult\include\IteridenseCLib.h"

// Helper to print clusterTensor data (assuming 3D tensor)
void printTensorInt(const CTensor* tensor, const char* name) {
    printf("%s ndims: %d\n", name, tensor->ndims);
    printf("%s dims: ", name);
    for (int i = 0; i < tensor->ndims; i++) {
        printf("%zu ", tensor->dims[i]);
    }
    printf("\n");

    int64_t* data = (int64_t*)tensor->data;
    size_t total_elements = 1;
    for (int i = 0; i < tensor->ndims; i++) {
        total_elements *= tensor->dims[i];
    }

    printf("%s data: ", name);
    for (size_t i = 0; i < total_elements; i++) {
        printf("%d ", data[i]);
    }
    printf("\n");
}

void printTensorDouble(const CTensor* tensor, const char* name) {
    printf("%s ndims: %d\n", name, tensor->ndims);
    printf("%s dims: ", name);
    for (int i = 0; i < tensor->ndims; i++) {
        printf("%zu ", tensor->dims[i]);
    }
    printf("\n");

    double* data = (double*)tensor->data;
    size_t total_elements = 1;
    for (int i = 0; i < tensor->ndims; i++) {
        total_elements *= tensor->dims[i];
    }

    printf("%s data: ", name);
    for (size_t i = 0; i < total_elements; i++) {
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
    IteridenseResultC* resultIteridense = IteridenseClustering(
        dataMatrix,
        nrows,
        ncols,
        3,      // minClusterSize
        2,      // startResolution
        1.1,    // density
        -1,     // stopResolution
        1,      // minClusters
        1.0,    // minClusterDensity
        0,      // noDiagonals (false)
        1,      // useDensity (true)
        0      // useClusters (false)
    );

    if (resultIteridense == NULL) {
        fprintf(stderr, "IteridenseClustering failed: returned NULL\n");
        return EXIT_FAILURE;
    }

    printf("numOfClusters: %d\n", resultIteridense->numOfClusters);
    printf("finalResolution: %d\n", resultIteridense->finalResolution);
    printTensorInt(&resultIteridense->clusterTensor, "clusterTensor");
    printTensorInt(&resultIteridense->countTensor, "countTensor");

    // Free the allocated resultIteridense
    int free_statusIteridense = IteridenseFree(resultIteridense);
    if (free_statusIteridense != 0) {
        fprintf(stderr, "IteridenseFree failed\n");
        return EXIT_FAILURE;
    }

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
    printTensorDouble(&resultKMeans->clusterCenters, "clusterCenters");

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
