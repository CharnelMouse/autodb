This is a re-submission.
The previous submission failure is as addressed below.

> Please reduce the test timings by using
>   - small toy data only
>   - few iterations
>   - or by running less important tests only conditionally if some
> environment variable is set that you only define on your machine?
> 
> Overall checktime of the whole package should stay < 10 minutes even on
> Windows.

This has been fixed:
- The property tests have been set to sample even less test cases on CRAN than
  before.
