# sssUtils

Utilities for the Self-Sufficiency Standard (SSS) workflow, including:

-   Path helpers (`sss_data_path()`, `sss_code_path()`)
-   Environment checks
-   Configuration loading for modules and states
-   Shared helper functions used across the SSS codebase

------------------------------------------------------------------------

## Installation

`sssUtils` is **not** on CRAN. Install it from GitHub:

``` r
install.packages("remotes")   # if needed
remotes::install_github("Center-for-Women-s-Welfare/sssUtils")
```

### Windows users

Windows requires **Rtools** to build packages from source:\
<https://cran.r-project.org/bin/windows/Rtools/>

**Basic usage**

``` r
`library(sssUtils)`

# `Check environment variables`

`sss_check_environment()`

# `Build data/code paths`

`sss_data_path("data", "2026", "raw") sss_code_path("config", "2026")`
```

For full SSS setup instructions (Google Drive, `.Renviron`, project structure), see the\
**sss_production README** in the main `sss_production` repository.
