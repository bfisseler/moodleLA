
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

The moodleLA package is designed to streamline the retrieval and
pseudonymization of Moodle™ data for practitioners in the field of
Learning Analytics (LA) and Educational Data Mining (EDM). Moodle is a
widely used open source learning management system (LMS). For more
information about Moodle go to [www.moodle.org](https://moodle.org/).

## Credits

This package is developed and maintained by [Dr. Björn
Fisseler](https://www.fernuni-hagen.de/psychologie/fakultaet/dekanat/bjoern-fisseler.shtml)
from University of Hagen (FernUniversität in Hagen).

## Installation

Please note that the package is under active development and has not yet
been released. It is fully functional and there is some initial
documentation available, but more will be added by the end of 2026. So
stay tuned and keep in touch!

To install the latest development version:

``` r
pak::pak("bfisseler/moodleLA")
```

You will also need to install
[Presidio](https://github.com/data-privacy-stack/presidio) from the Data
Privacy Stack. Presidio is currently transitioning from a
Microsoft-owned project to an independent, community-governed open
source project. The Shiny app integrated in moodleLA supports running
[Presidio using
Docker](https://presidio.dataprivacystack.org/installation/#using-docker)
for additional detecting and filtering PII. You can (and should)
customize Presidio based on your needs and the language used. There will
be more instructions on how to configure Presidio for use with moodleLA
at the end of 2026.
