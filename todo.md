Before release
- update function documentation
- finish vignette
- add readme

After release
- remove violating rows in databases formed with approximate dependencies
- create a better intermediate language for constructing relations etc.
- vectorisation of DFD
- make reduce() return a "partial_database" type that can't be used in rejoin(), or at least do some losslessness/consistency checks on completion
be used in rejoin(), or at least check for consistency somehow
- possibly add attribute types to database schemes, as this is the norm for real databases
- add error checking for decompose for norm_deps not matching data.frame
- store lossless rejoin plan in database and database scheme
- cross_intersection still converts to full bitset instead of shorted logical vectors
- add lookup maps for quicker subsetting (c.f. Tane)
- add option for introducing artificial keys
- powerset creation is still slow
- move relation matrix setdiff into relation matrix function
- add combining of dependency lists, schemes, etc.
