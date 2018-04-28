# pipecleaner 0.0.2

* Exports new `split_pipeline` utility function to break expressions of 
    pipelines up into lists.

* Adds `data` parameter to `debug_pipeline` to enable the option to insert the 
    data piped in, making navigating nested calls easier. Restructured add-ins 
    to allow insertion or piping.

* Switches to rlang backend instead of text manipulations, adding a [light] 
    dependency.

* Adds a `NEWS.md` file to track changes to the package.

# pipecleaner 0.0.1

* Adds initial framework for `debug_pipeline` and RStudio add-in.
