# The IteridenseClustering standalone program

## Usage

1. Download the latest release [from GitHub](https://github.com/donovaly/Iteridense-package/tree/main)
2. Extract the ZIP and there in the folder *bin* start the *IteridenseClustering.exe* (no installation or admin permissions are required)
3. In the program click the button to open a CSV data file
4. Perform the clustering as you wish
5. Save the result either the plot or as CSV. The CSV is hereby the input file plus an additional colum with the assignments to the clusters.

### Requirements for CSV data format

* The CSV data file must have exactly one header line
* Every column represents a data dimension
* The CSV can have any column delimiter character (IteridenseClustering detects it automatically) 

## Compilation

1. Install the **[Lazarus IDE](https://en.wikipedia.org/wiki/Lazarus_(software))**
2. Get the source code [from GitHub](https://github.com/donovaly/Iteridense-package/tree/main)
3. Open the file *IteridenseClustering.lpi* in Lazarus.
3. Build the Lazarus project or run it.
