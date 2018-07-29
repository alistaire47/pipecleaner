# pipecleaner 0.0.2.9000

* Adds `burst_pipes` function and a pair of RStudio add-ins to remove pipes 
    from a pipeline and replace them with assignment.

* Rebuilds `data = "insert"` behavior of `debug_pipeline` so dots are replaced 
    with piped-in data properly. Formulas are ignored; nested pipelines are 
    still not supported.

* Removes `data` parameter of `debug_pipeline` and accompanying add-in, as the 
    data insertion behavior is now robust (and more debuggable) thanks to 
    recursive substitution.

* Enables `debug_pipeline` and `split_pipeline` to work with `=` assignment (in 
    addition to `<-`) at the start of the pipeline.

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
