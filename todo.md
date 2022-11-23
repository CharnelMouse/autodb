Before release
- export reduce(), possible after making it return a "partial_database" type that can't be used in rejoin()
- update function documentation
- finish vignette

After release
- possibly add attribute types to database schemes, as this is the norm for real databases
- add error checking for decompose for norm_deps not matching data.frame
- store lossless rejoin plan in database and database scheme
- cross_intersection still converts to full bitset instead of shorted logical vectors
- add lookup maps for quicker subsetting (c.f. Tane)
- add option for introducing artificial keys
- powerset creation is still slow
- move relation matrix setdiff into relation matrix function
- add combining of dependency lists, schemes, etc.
