This is a re-submission.
The previous submission failure are as addressed below.

> Pls only use the CRAN template for the BSD_3_clause, i.e. only fields
> YEAR:
> COPYRIGHT HOLDER:
> ORGANIZATION:

This has been fixed.

> Please single quote software names in both Title and Description fields
> of the DESCRIPTION file, e.g., 'Python'

This has been fixed.

> Please reduce the test timings by using
>   - small toy data only
>   - few iterations
>   - or by running less important tests only conditionally if some
> environment variable is set that you only define on your machine?
> 
> Overall checktime of the whole package should stay < 10 minutes even on
> Windows.

This has been fixed:
- The performance of some package functions was improved;
- Most of the test time is spent on property tests, which check many
  randomly-generated test cases. These were set to sample less test cases on
  CRAN.
