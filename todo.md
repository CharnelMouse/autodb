- add removing of avoidable attributes (ERASE, Maier section 6.6)
- add error checking for decompose for norm_deps not matching data.frame
- add partitions
- add option for introducing artificial keys
- powerset creation is still slow
- look for avoidable attributes (LTK normal form)
- replace checking for original key with adding addition FD before other
normalising steps, see end of Maier section 6.5. (Might need to implement
Erase first, to stop it adding unnecessary additional keys.)
- move relation matrix setdiff into relation matrix function
