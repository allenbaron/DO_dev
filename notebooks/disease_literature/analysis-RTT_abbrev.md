Seems to be used frequently for Rett syndrome (DOID:1206), but should it be added?

**GOAL:** Determine if RTT is worth adding as a synonym of Rett syndrome.

*It will be if it is not used for other diseases and Rett syndrome appears to be a sizable proportion of instances identified in the published literature.*

# Approach

In this case, I’ll identify all open access publications with “RTT” in them and then try to extract the full-length equivalent from each publication to estimate the prevalence and determine how this abbreviation is used.

*The output will be saved to a file to avoid need of repeat API call
(until the next desired updated).*

The number of publication hits was 4,521.

The full text of Open Access publications will also be downloaded for this analysis, excluding retractions and preprints.


# Evaluating Usage

Extracting all these values from the full text of the sample publications and all the titles (in a case-insensitive manner).

The number of publications with and without matches in their titles or full text (or what approximates for full text) are as follows:
anal_df |>
    dplyr::summarize(match = any(!is.na(.data$match)), .by = c("id", "type")) |>
    dplyr::count(.data$type, .data$match) |>
    dplyr::mutate(title = .data$type == "title") |>
    dplyr::mutate(pct = round(.data$n / sum(.data$n) * 100, 2), .by = "title") |>
    dplyr::arrange(.data$title, .data$match, .data$type) |>
    dplyr::select(-"title")
```

    ## # A tibble: 8 × 4
    ##   type            match     n   pct
    ##   <chr>           <lgl> <int> <dbl>
    ## 1 ft-abstract?    FALSE   160  3.58
    ## 2 ft-available    FALSE  2466 55.2 
    ## 3 ft-inaccessible FALSE    11  0.25
    ## 4 ft-none         FALSE    12  0.27
    ## 5 ft-abstract?    TRUE      9  0.2 
    ## 6 ft-available    TRUE   1812 40.5 
    ## 7 title           FALSE  4464 99.9 
    ## 8 title           TRUE      6  0.13

The breakdown of terms identified is as follows:

    ## # A tibble: 340 × 3
    ##    std_match                             n   pct
    ##    <chr>                             <int> <dbl>
    ##  1 "rett syndrome "                    782 36   
    ##  2 "round trip time "                  424 19.5 
    ##  3 "reverse transcriptase template "    59  2.72
    ##  4 "reverse transcription template "    57  2.62
    ##  5 "rett syndromerett syndrome "        51  2.35
    ##  6 "radiation therapist "               32  1.47
    ##  7 "referral to treatment "             27  1.24
    ##  8 "rt template "                       21  0.97
    ##  9 "radiation therapists "              20  0.92
    ## 10 "root to tip "                       19  0.87
    ## # ℹ 330 more rows

# Conclusion

Though Rett syndrome represents about 1/3 of the uses for RTT, it is the major meaning for RTT in the disease realm. It seems reasonable to add RTT as a synonym for Rett syndrome (this will use the acronym annotation, since DO does not use an annotation for abbreviation, even though it’s not exactly an acronym).
