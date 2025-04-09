# Iteridense - a simple iterative grid- and density-based clustering algorithm

# Description

This repository provides two things:

* A brief description and discussion of the Iteridense clustering algorithm, see [this PDF](https://codeberg.org/donovaly/Iteridense/raw/branch/main/Paper/Iteridense-clustering.pdf).
* A reference implementation of the algorithm to use it directly on data of your choice.

# How to do use the reference implementation

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

## Structure of the repository

The folder **[Paper](https://codeberg.org/donovaly/Iteridense/src/branch/main/Paper)** contains the PDF and its source code file of the paper describing the algorithm. The folder **[Paper/clipart](https://codeberg.org/donovaly/Iteridense/src/branch/main/Paper/clipart)** contains all images used in the paper.

The folder **[Reference Implementation](https://codeberg.org/donovaly/Iteridense/src/branch/main/Reference%20Implementation)** contains the Julia code of the algorithm. There the file *IteridenseLibrary.jl* is the actual algorithm. It can be used as code library for any Julia program. The file *Iteridense-Clustering.jl* explains the algorithm with examples. It uses *IteridenseLibrary.jl*.

The folder **[Reference Implementation/datasets](https://codeberg.org/donovaly/Iteridense/src/branch/main/Reference%20Implementation/datasets)** contains all data used as example in *Iteridense-Clustering.jl*.