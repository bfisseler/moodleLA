
<!-- README.md is generated from README.Rmd. Please edit that file -->

# moodleLA

<!-- badges: start -->

[![Repostatus:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
status](https://www.r-pkg.org/badges/version/moodleLA)](https://CRAN.R-project.org/package=moodleLA)
[![R-CMD-check](https://github.com/bfisseler/moodleLA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bfisseler/moodleLA/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/bfisseler/moodleLA/graph/badge.svg)](https://app.codecov.io/gh/bfisseler/moodleLA)
<!-- badges: end -->

The goal of moodleLA is to facilitate access to and manipulating of
Moodle™ data for learning analytics practitioners. Moodle is a widely
used open source learning management system (LMS). For more information
about Moodle go to [www.moodle.org](https://moodle.org/).

## Credits

This package is developed and maintained by [Dr. Björn
Fisseler](https://www.fernuni-hagen.de/psychologie/fakultaet/dekanat/bjoern-fisseler.shtml)
from University of Hagen (FernUniversität in Hagen).

## Installation

Please note that the package is still under active development and has
not yet been released. There is some initial documentation available,
but more will be added by the end of 2025. So stay tuned and keep in
touch!

To install the latest development version:

``` r
devtools::install("bfisseler/moodleLA")
```

You will also need to install [Microsoft
Presidio](https://github.com/microsoft/presidio) using Docker for
additional detecting and filtering PII. You can (and should) customize
Microsoft Presidio based on your needs and the language used. There will
be more instructions on how to configure Microsoft Presidio for use with
moodleLA at the end of 2025.
