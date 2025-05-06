# Julia and Lazarus package for the Iteridense clustering algorithm

# Description

This repository provides two things:

* a package for the programming Julia
* a standalone, fully featured program *IteridenseClustering* written in Lazarus

Both allow you to cluster any data using the clustering algorithms *Iteridense*, *DBSCAN and *k-Means*.

For a brief description and discussion of the Iteridense clustering algorithm, see [this PDF](https://codeberg.org/Soloof/Iteridense/raw/branch/main/Paper/Iteridense-clustering.pdf).

# How to do use the Julia reference implementation

1. Install the scientific programming language [Julia](https://en.wikipedia.org/wiki/Julia_(programming_language))
2. Install the program [VS Code](https://en.wikipedia.org/wiki/Visual_Studio_Code) (under Linux's AUR it is called *Code - OSS*)
3. In VS Code, install the Extension *'Julia'*
4. In VS Code go to the menu File→Preferences→Settings and search there for *'Julia: num Threads'*. Then click in the search result at
   *'Edit in settings.json'* and change the line
 
    "julia.NumThreads": 1,
   
   to
   
    "julia.NumThreads": "auto",

   and press *Ctrl+S* to save the change. Eventually restart VS Code.
5. Open the script file ***Iteridense-Clustering.jl*** with VS Code and there trust the source when you are asked for this.
   Then step through the file by executing the difference code pieces by setting the cursor into the code and pressing *Ctrl+Return*.
   Note that you must execute all commands from top to bottom of the script as every command depends on the ones above. On the first
   execution of the script you must uncomment 1 line at the top of the script to install packages to Julia.
6. As described in *Iteridense-Clustering.jl* you can easily cluster any data. It only has to be available as a CSV file.

# How to do use the Julia reference implementation

* Install the **[Lazarus IDE](https://en.wikipedia.org/wiki/Lazarus_(software))**
* Open the file *IteridenseClustering.lpi* in Lazarus.
* Build the Lazarus project or run it.

## Structure of the repository

The folder **[src](https://github.com/donovaly/Iteridense-package/tree/main/src)** contains the Julia module *[Iteridense.jl](https://github.com/donovaly/Iteridense-package/blob/main/src/Iteridense.jl)*.

The folder **[IteridenseCLib](https://github.com/donovaly/Iteridense-package/tree/main/IteridenseCLib)** contains the Julia module in a version that can be compiled as a C-library. That library can be used e.g. as DLL for any program that can read C-compatible libraries.
The release ZIP file contains a precompiled version as DLL for Windows (filename IteridenseCLib.dll). 

The folder **[IteridenseClustering](https://github.com/donovaly/Iteridense-package/tree/main/IteridenseCLib/IteridenseClustering)** contains the program *IteridenseClustering*. This program uses the IteridenseCLib library. The release ZIP contains a compiled version for Windows as executable.
I effect *IteridenseClustering* is the Iteridense Julia package plus a graphical uiser interface (GUI).

The folder **[examples](https://github.com/donovaly/Iteridense-package/tree/main/examples)** contains the example Julia script *[Iteridense-Clustering.jl](https://github.com/donovaly/Iteridense-package/blob/main/examples/Iteridense-Clustering.jl* that uses the Iteridense Julia package. It also contains some *[datasets](https://github.com/donovaly/Iteridense-package/tree/main/examples/datasets)* as CSV to test and play with the clustering.
