# Nudge meta-analysis data

Data used for a meta-analysis on the effectiveness of nudges, i.e.
choice architecture interventions.

## Usage

``` r
nudge
```

## Format

A data frame with 447 effect size measurements and 25 columns:

1.  publication_id, integer ID number for the publication. Note that two
    publications were erroneously assigned the same ID number, so this
    is not a unique publication identifier.

2.  study_id, integer ID number for the study.

3.  es_id, integer ID number for the effect size measured.

4.  reference, publication citation in "Author(s) (year)" format. Due to
    two publications being assigned the same reference, this is also not
    a unique publication identifier.

5.  title, title of the publication. Due to the error in assigning
    publication ID numbers, this is the unique publication identifier
    within the data set.

6.  year, year of the publication.

7.  location, geographical location of the intervention. This is given
    as a factor, rather than an integer, using the information provided
    in the codebook.

8.  domain, factor giving the intervention's behavioural domain.

9.  intervention_category, factor giving the intervention's category,
    based on the taxonomy in Münscher et al. (2016).

10. intervention_technique, factor giving the intervention's technique,
    based on the taxonomy in Münscher et al. (2016).

11. type_experiment, factor giving the type of experiment, as defined by
    Harrison and List (2004).

12. population, factor giving the intervention's target population. This
    is given as a factor, rather than an integer, using the information
    provided in the codebook.

13. n_study, sample size of the overall study.

14. n_comparison, combined sample size of the control and the
    intervention for the measured effect size.

15. n_control, sample size of the control condition for the measured
    effect size.

16. n_intervention, sample size of the intervention condition for the
    measured effect size.

17. binary_outcome, logical for whether the outcome scale is binary or
    continuous.

18. mean_control, mean of outcome for the control condition.

19. sd_control, SD of outcome for the control condition.

20. mean_intervention, mean of outcome for the intervention condition.

21. sd_intervention, SD of outcome for the intervention condition.

22. cohens_d, extracted effect size of intervention.

23. variance_d, variance of extracted effect size.

24. approximation, logical for whether effect size extraction involved
    approximation.

25. wansink, logical for whether the study was (co-)authored by Brian
    Wansink. This was added on revision, because, a few years before
    publication, Wansink had many papers retracted or corrected, due to
    various questionable practices, resulting in Wansink being
    determined to have committed scientific misconduct. This column was
    added to check whether the findings were robust to the exclusion of
    non-retracted studies by the Cornell Food and Brand Laboratory, of
    which Wansink was the director.

## Source

<https://osf.io/fywae/>

## References

Mertens S., Herberz M., Hahnel U. J. J., Brosch T. (2022) The
effectiveness of nudging: A meta-analysis of choice architecture
interventions across behavioral domains. *Proc. Natl. Acad. Sci.
U.S.A.*, **4**, 119(1).
