This is a re-submission.
The previous submission failed on pre-checks, as addressed below.

> Possibly misspelled words in DESCRIPTION:
>   Alteryx's (15:26)
>   AutoNormalize (15:36)
>   Graphviz (19:3)

No changes. These are all names for things, and are spelled correctly.

> The Title field should be in title case. Current version is:
>   'Automatic database normalisation for data frames'
> In title case that is:
>   'Automatic Database Normalisation for Data Frames'

This has been fixed.

> The Description field contains
>   (https://github.com/alteryx/autonormalize), with various changes and
> Please enclose URLs in angle brackets (<...>).

This has been fixed.

> LaTeX errors when creating PDF version.
> This typically indicates Rd problems.

This has been fixed. The issue was with some help files containing the phrase

> \code{\link[base:names]{`names<-`}},

before Markdown conversion. Removing the backticks around names<- fixed the
issue.
