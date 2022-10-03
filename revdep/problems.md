# arrow

<details>

* Version: 9.0.0.2
* GitHub: https://github.com/apache/arrow
* Source code: https://github.com/cran/arrow
* Date/Publication: 2022-10-02 02:40:05 UTC
* Number of recursive dependencies: 72

Run `cloud_details(, "arrow")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      • https://issues.apache.org/jira/browse/ARROW-7653 (1)
      • packageVersion("lubridate") > "1.8" is not TRUE (1)
      • pyarrow not available for testing (1)
      • tolower(Sys.info()[["sysname"]]) != "windows" is TRUE (1)
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error (test-dplyr-funcs-string.R:145:3): paste, paste0, and str_c ───────────
      <dplyr:::mutate_error/rlang_error/error/condition>
      Error in `transmute(., str_c(x, y, sep = NA_character_))`: Problem while computing `..1 = str_c(x, y, sep = NA_character_)`.
      Caused by error in `str_c()`:
      ! `sep` must be a single string, not a character `NA`.
      
      [ FAIL 1 | WARN 2 | SKIP 66 | PASS 8173 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 113.7Mb
      sub-directories of 1Mb or more:
        R       4.1Mb
        libs  108.4Mb
    ```

*   checking Rd cross-references ... NOTE
    ```
    Package unavailable to check Rd xrefs: ‘readr’
    ```

# autostats

<details>

* Version: 0.3.1
* GitHub: https://github.com/Harrison4192/autostats
* Source code: https://github.com/cran/autostats
* Date/Publication: 2022-08-19 13:10:06 UTC
* Number of recursive dependencies: 233

Run `cloud_details(, "autostats")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘autostats-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: f_modify_formula
    > ### Title: Modify Formula
    > ### Aliases: f_modify_formula
    > 
    > ### ** Examples
    > 
    > 
    ...
    +   f_modify_formula(
    + rhs_remove = c("Petal.Width", "Sepal.Length"),
    + rhs_add = "Custom_Variable"
    + )
    Error in `stringr::str_c()`:
    ! `..1` must be a vector, not a symbol.
    Backtrace:
     1. f %>% ...
     7. stringr::str_c(lhs, " ~ ", ., collapse = "")
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘autostats.Rmd’ using rmarkdown
    --- finished re-building ‘autostats.Rmd’
    
    --- re-building ‘tidyModels.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
    ...
    Quitting from lines 104-109 (tidyModels.Rmd) 
    Error: processing vignette 'tidyModels.Rmd' failed with diagnostics:
    `..1` must be a vector, not a symbol.
    --- failed re-building ‘tidyModels.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘tidyModels.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# bdpar

<details>

* Version: 3.0.3
* GitHub: https://github.com/miferreiro/bdpar
* Source code: https://github.com/cran/bdpar
* Date/Publication: 2022-08-22 13:30:05 UTC
* Number of recursive dependencies: 74

Run `cloud_details(, "bdpar")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘test_all.R’
    Running the tests in ‘tests/test_all.R’ failed.
    Last 13 lines of output:
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(pipe$removeUserName(data), " ") at test_FindUserNamePipe.R:346:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. └─pipe$removeUserName(data)
       5.   └─stringr::str_replace_all(...)
       6.     ├─stringr:::type(pattern)
       7.     └─stringr:::type.default(pattern)
       8.       └─cli::cli_abort("`pattern` must be a string", call = error_call)
       9.         └─rlang::abort(...)
      
      [ FAIL 20 | WARN 0 | SKIP 5 | PASS 513 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2666 marked UTF-8 strings
    ```

# crispRdesignR

<details>

* Version: 1.1.6
* GitHub: NA
* Source code: https://github.com/cran/crispRdesignR
* Date/Publication: 2021-01-11 07:00:02 UTC
* Number of recursive dependencies: 77

Run `cloud_details(, "crispRdesignR")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘crispRdesignR-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: getofftargetdata
    > ### Title: Off Target Data Frame Creation
    > ### Aliases: getofftargetdata
    > 
    > ### ** Examples
    > 
    > 
    ...
    Backtrace:
        ▆
     1. ├─crispRdesignR::sgRNA_design(testseq, usergenome, gtfname, calloffs = FALSE)
     2. │ └─stringr::str_replace_all(revsetPAM, "N", ".")
     3. │   └─stringr:::check_lengths(string, pattern, replacement)
     4. │     └─vctrs::vec_size_common(...)
     5. └─vctrs:::stop_scalar_type(...)
     6.   └─vctrs:::stop_vctrs(...)
     7.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘DT’ ‘gbm’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# flair

<details>

* Version: 0.0.2
* GitHub: https://github.com/kbodwin/flair
* Source code: https://github.com/cran/flair
* Date/Publication: 2020-04-23 18:00:05 UTC
* Number of recursive dependencies: 84

Run `cloud_details(, "flair")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘flair-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: decorate_code
    > ### Title: Creates an object of the class 'with_flair'
    > ### Aliases: decorate_code
    > 
    > ### ** Examples
    > 
    > 
    ...
      8. │       └─flair:::flair_rx.default(...)
      9. │         └─... %>% unlist()
     10. ├─base::unlist(.)
     11. ├─stringr::str_extract_all(., "(\\<[^\\<\\>]*\\>)|((?<=\\>|^)([^\\<]|(\\<(?=(\\-|\\<))))*(?=\\<|$))")
     12. │ └─stringr:::check_lengths(string, pattern)
     13. │   └─vctrs::vec_size_common(...)
     14. └─vctrs:::stop_scalar_type(`<fn>`(`<source>`), "string", `<env>`)
     15.   └─vctrs:::stop_vctrs(...)
     16.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. ├─flair:::flair_lines.with_flair(test_wf, c(2:4))
        3. │ └─... %>% map2(1:sum(where_sources), ~ 0:.x + .y)
        4. ├─purrr::map2(., 1:sum(where_sources), ~0:.x + .y)
        5. ├─purrr::map(., ~str_count(.x, "\\n|(<br>)"))
        6. │ └─flair (local) .f(.x[[i]], ...)
        7. │   └─stringr::str_count(.x, "\\n|(<br>)")
        8. │     └─stringr:::check_lengths(string, pattern)
        9. │       └─vctrs::vec_size_common(...)
       10. └─vctrs:::stop_scalar_type(`<fn>`(`<source>`), "string", `<env>`)
       11.   └─vctrs:::stop_vctrs(...)
       12.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 5 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘how_to_flair.Rmd’ using rmarkdown
    
    Attaching package: 'dplyr'
    
    The following objects are masked from 'package:stats':
    
        filter, lag
    
    ...
    Quitting from lines 32-43 (how_to_flair.Rmd) 
    Error: processing vignette 'how_to_flair.Rmd' failed with diagnostics:
    `string` must be a vector, not a <source> object.
    --- failed re-building ‘how_to_flair.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘how_to_flair.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespaces in Imports field not imported from:
      ‘dplyr’ ‘evaluate’ ‘ggplot2’
      All declared Imports should be used.
    ```

*   checking LazyData ... NOTE
    ```
      'LazyData' is specified without a 'data' directory
    ```

# gginnards

<details>

* Version: 0.1.0-1
* GitHub: https://github.com/aphalo/gginnards
* Source code: https://github.com/cran/gginnards
* Date/Publication: 2021-07-30 11:40:02 UTC
* Number of recursive dependencies: 66

Run `cloud_details(, "gginnards")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘gginnards-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: drop_vars
    > ### Title: Explore and manipulate the embedded data.
    > ### Aliases: drop_vars mapped_vars data_vars data_attributes
    > 
    > ### ** Examples
    > 
    > library(ggplot2)
    ...
      2. │ └─... %>% unlist()
      3. ├─base::unlist(.)
      4. ├─stringr::str_split(., pattern = stringr::boundary("word"))
      5. │ └─stringr:::check_lengths(string, pattern)
      6. │   └─vctrs::vec_size_common(...)
      7. └─vctrs::stop_incompatible_size(...)
      8.   └─vctrs:::stop_incompatible(...)
      9.     └─vctrs:::stop_vctrs(...)
     10.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘user-guide-1.Rmd’ using rmarkdown
    --- finished re-building ‘user-guide-1.Rmd’
    
    --- re-building ‘user-guide-2.Rmd’ using rmarkdown
    Quitting from lines 316-321 (user-guide-2.Rmd) 
    Error: processing vignette 'user-guide-2.Rmd' failed with diagnostics:
    Can't recycle `string` (size 2) to match `pattern` (size 0).
    --- failed re-building ‘user-guide-2.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘user-guide-2.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# glmmPen

<details>

* Version: 1.5.1.10
* GitHub: NA
* Source code: https://github.com/cran/glmmPen
* Date/Publication: 2022-04-29 12:30:07 UTC
* Number of recursive dependencies: 90

Run `cloud_details(, "glmmPen")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. │   ├─testthat (local) .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. ├─glmmPen::glmmPen(...)
        7. │ └─glmmPen:::fD_adj(out = fD_out0)
        8. │   └─stringr::str_c(rep(cnms, each = d), ":", levels(group))
        9. │     └─vctrs::vec_size_common(!!!dots)
       10. └─vctrs::stop_incompatible_size(...)
       11.   └─vctrs:::stop_incompatible(...)
       12.     └─vctrs:::stop_vctrs(...)
       13.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 4 | WARN 0 | SKIP 0 | PASS 6 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is 103.2Mb
      sub-directories of 1Mb or more:
        libs  102.2Mb
    ```

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘rstantools’
      All declared Imports should be used.
    ```

# gmgm

<details>

* Version: 1.1.2
* GitHub: NA
* Source code: https://github.com/cran/gmgm
* Date/Publication: 2022-09-08 20:32:55 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "gmgm")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Error in `"the defined structure has cycles" %>% stop()`: the defined structure has cycles
      Backtrace:
          ▆
       1. ├─testthat::expect_equal(...) at test-struct_learn.R:304:2
       2. │ └─testthat::quasi_label(enquo(object), label, arg = "object")
       3. │   └─rlang::eval_bare(expr, quo_get_env(quo))
       4. ├─gmgm::struct_learn(...)
       5. │ └─list_gmm_score$gmm %>% do.call("gmbn", .)
       6. ├─base::do.call("gmbn", .)
       7. └─gmgm::gmbn(A = `<gmm>`, B = `<gmm>`, C = `<gmm>`)
       8.   └─"the defined structure has cycles" %>% stop()
      
      [ FAIL 1 | WARN 0 | SKIP 0 | PASS 415 ]
      Error: Test failures
      Execution halted
    ```

# hockeyR

<details>

* Version: 1.2.0
* GitHub: https://github.com/danmorse314/hockeyR
* Source code: https://github.com/cran/hockeyR
* Date/Publication: 2022-09-18 17:46:06 UTC
* Number of recursive dependencies: 107

Run `cloud_details(, "hockeyR")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘hockey-ref-scrapers.Rmd’ using rmarkdown
    Quitting from lines 94-136 (hockey-ref-scrapers.Rmd) 
    Error: processing vignette 'hockey-ref-scrapers.Rmd' failed with diagnostics:
    `string` must be a vector, not a <xml_nodeset> object.
    --- failed re-building ‘hockey-ref-scrapers.Rmd’
    
    --- re-building ‘hockeyR.Rmd’ using rmarkdown
    --- finished re-building ‘hockeyR.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘hockey-ref-scrapers.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# strex

<details>

* Version: 1.4.3
* GitHub: https://github.com/rorynolan/strex
* Source code: https://github.com/cran/strex
* Date/Publication: 2022-07-24 21:40:02 UTC
* Number of recursive dependencies: 65

Run `cloud_details(, "strex")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘spelling.R’
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─testthat::expect_error(...) at test-trim.R:36:2
        2. │ └─testthat:::quasi_capture(...)
        3. │   ├─testthat (local) .capture(...)
        4. │   │ └─base::withCallingHandlers(...)
        5. │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        6. └─strex::str_trim_anything("x", boundary("word"))
        7.   └─strex:::verify_string_pattern(string, pattern, boundary_allowed = FALSE)
        8.     └─checkmate::assert_character(pattern, min.len = 1)
        9.       └─checkmate::makeAssertion(x, res, .var.name, add)
       10.         └─checkmate:::mstop(...)
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 285 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘alphordering-numbers.Rmd’ using rmarkdown
    --- finished re-building ‘alphordering-numbers.Rmd’
    
    --- re-building ‘argument-matching.Rmd’ using rmarkdown
    --- finished re-building ‘argument-matching.Rmd’
    
    --- re-building ‘before-and-after.Rmd’ using rmarkdown
    --- finished re-building ‘before-and-after.Rmd’
    
    ...
      Failed to tidy R code in chunk 'regex'. Reason:
    Error in loadNamespace(x) : there is no package called 'styler'
    
    --- finished re-building ‘numbers-in-strings.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘important-miscellany.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# TiPS

<details>

* Version: 1.2.1
* GitHub: NA
* Source code: https://github.com/cran/TiPS
* Date/Publication: 2022-08-26 19:34:46 UTC
* Number of recursive dependencies: 54

Run `cloud_details(, "TiPS")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        3. ├─TiPS::build_simulator(reactions)
        4. │ └─TiPS:::simulator_generator(reactions, functions)
        5. │   ├─base::unique(...)
        6. │   ├─base::unlist(stringr::str_extract_all(unlist(reactions), stringr::boundary("word")))
        7. │   └─stringr::str_extract_all(unlist(reactions), stringr::boundary("word"))
        8. │     └─stringr:::check_lengths(string, pattern)
        9. │       └─vctrs::vec_size_common(...)
       10. └─vctrs::stop_incompatible_size(...)
       11.   └─vctrs:::stop_incompatible(...)
       12.     └─vctrs:::stop_vctrs(...)
       13.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 2 | WARN 0 | SKIP 0 | PASS 0 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘TiPS.Rmd’ using rmarkdown
    Building the simulator ...
    Quitting from lines 90-91 (TiPS.Rmd) 
    Error: processing vignette 'TiPS.Rmd' failed with diagnostics:
    Can't recycle `string` (size 2) to match `pattern` (size 0).
    --- failed re-building ‘TiPS.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘TiPS.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        doc    1.2Mb
        libs   5.8Mb
    ```

# wordpredictor

<details>

* Version: 0.0.3
* GitHub: https://github.com/pakjiddat/word-predictor
* Source code: https://github.com/cran/wordpredictor
* Date/Publication: 2022-01-04 14:30:02 UTC
* Number of recursive dependencies: 79

Run `cloud_details(, "wordpredictor")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘wordpredictor-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: DataCleaner
    > ### Title: Provides data cleaning functionality
    > ### Aliases: DataCleaner
    > 
    > ### ** Examples
    > 
    > 
    ...
      3. │   └─wordpredictor (local) process(lines)
      4. │     └─self$clean_lines(lines)
      5. │       └─stringr::str_count(l, pattern = boundary("word"))
      6. │         └─stringr:::check_lengths(string, pattern)
      7. │           └─vctrs::vec_size_common(...)
      8. └─vctrs::stop_incompatible_size(...)
      9.   └─vctrs:::stop_incompatible(...)
     10.     └─vctrs:::stop_vctrs(...)
     11.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
       3. │   └─stringr:::check_lengths(string, pattern)
       4. │     └─vctrs::vec_size_common(...)
       5. └─vctrs::stop_incompatible_size(...)
       6.   └─vctrs:::stop_incompatible(...)
       7.     └─vctrs:::stop_vctrs(...)
       8.       └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 3 | WARN 0 | SKIP 0 | PASS 117 ]
      Error: Test failures
      In addition: Warning messages:
      1: In for (i in seq_len(n)) { :
        closing unused connection 4 (/tmp/RtmpBKOHrE/sample.txt)
      2: In for (i in seq_along(nms)) { :
        closing unused connection 5 (/tmp/RtmpBKOHrE/test.txt)
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘features.Rmd’ using rmarkdown
    Quitting from lines 157-185 (features.Rmd) 
    Error: processing vignette 'features.Rmd' failed with diagnostics:
    Can't recycle `string` (size 726) to match `pattern` (size 0).
    --- failed re-building ‘features.Rmd’
    
    --- re-building ‘overview.Rmd’ using rmarkdown
    Quitting from lines 115-146 (overview.Rmd) 
    Error: processing vignette 'overview.Rmd' failed with diagnostics:
    Can't recycle `string` (size 72) to match `pattern` (size 0).
    --- failed re-building ‘overview.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘features.Rmd’ ‘overview.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

# xpose

<details>

* Version: 0.4.13
* GitHub: https://github.com/UUPharmacometrics/xpose
* Source code: https://github.com/cran/xpose
* Date/Publication: 2021-06-30 08:00:02 UTC
* Number of recursive dependencies: 108

Run `cloud_details(, "xpose")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘xpose-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: amt_vs_idv
    > ### Title: Compartment kinetics
    > ### Aliases: amt_vs_idv
    > 
    > ### ** Examples
    > 
    > amt_vs_idv(xpdb_ex_pk, nrow = 2, ncol = 1)
    ...
        ▆
     1. ├─base (local) `<fn>`(x)
     2. ├─xpose:::print.xpose_plot(x)
     3. │ └─xpose::append_suffix(x$xpose, x$labels$title, "title")
     4. │   └─stringr::str_c(string, xpdb$xp_theme[stringr::str_c(type, "_suffix")], sep = "")
     5. │     └─vctrs::vec_size_common(!!!dots)
     6. └─vctrs:::stop_scalar_type(`<fn>`(`<uneval>`), "..2", `<env>`)
     7.   └─vctrs:::stop_vctrs(...)
     8.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
        2. │ └─ggplot2::ggsave(...)
        3. │   ├─grid::grid.draw(plot)
        4. │   └─ggplot2:::grid.draw.ggplot(plot)
        5. │     ├─base::print(x)
        6. │     └─xpose:::print.xpose_plot(x)
        7. │       └─xpose::append_suffix(x$xpose, x$labels$title, "title")
        8. │         └─stringr::str_c(string, xpdb$xp_theme[stringr::str_c(type, "_suffix")], sep = "")
        9. │           └─vctrs::vec_size_common(!!!dots)
       10. └─vctrs:::stop_scalar_type(`<fn>`(`<uneval>`), "..2", `<env>`)
       11.   └─vctrs:::stop_vctrs(...)
       12.     └─rlang::abort(message, class = c(class, "vctrs_error"), ..., call = vctrs_error_call(call))
      
      [ FAIL 3 | WARN 0 | SKIP 7 | PASS 517 ]
      Error: Test failures
      Execution halted
    ```

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘access_xpdb_data.Rmd’ using rmarkdown
    Returning data from run001.ext, $prob no.1, subprob no.0, method foce
    Returning parameter estimates from $prob no.1, subprob no.0, method foce
    --- finished re-building ‘access_xpdb_data.Rmd’
    
    --- re-building ‘customize_plots.Rmd’ using rmarkdown
    Using data from $prob no.1
    Filtering data by EVID == 0
    Using data from $prob no.1
    ...
    Error: processing vignette 'vpc.Rmd' failed with diagnostics:
    `..2` must be a vector, not a <uneval> object.
    --- failed re-building ‘vpc.Rmd’
    
    SUMMARY: processing the following files failed:
      ‘customize_plots.Rmd’ ‘introduction.Rmd’ ‘multiple_pages.Rmd’
      ‘vpc.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

