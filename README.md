
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- use devtools::build_readme() to update -->

# EAVA

Verbal Autopsy Expert Algorithm is a computer-coded verbal autopsy
algorithm (CCVA) which assigns causes of death using responses obtained
from the 2016 World Health Organization Verbal Autopsy Questionnaire.
Historically, causes of death have been assigned by physician coding of
questionnaire responses. As CCVAs become more widely used, physician
time can be reallocated to other priorities in low resources settings.

## Installation

You can install the development version of EAVA from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("emilybrownwilson/EAVA")
```

## Example

This is a basic example which shows you how to use the package:

``` r
# load EAVA package
library(EAVA)

# load embedded example data
data <- as.data.frame(data_public)

# run odk2EAVA() using example data
output <- odk2EAVA(data, id_col  = "comsa_id")

# run codEAVA for neonates and children 1-to-59 months of age
EAVA_neonate <- codEAVA(output, "neonate")
EAVA_child <- codEAVA(output, "child")
```

``` r
# check results
head(EAVA_neonate)
#>      ID       cause
#> 1  2021 Intrapartum
#> 5  3169 Unspecified
#> 9 16965 Unspecified
head(EAVA_child)
#>      ID                   cause
#> 2 17485                 Preterm
#> 3 12102             Unspecified
#> 4  9502      Diarrhea/Dysentery
#> 6   434        Other infections
#> 7  4168                 Malaria
#> 8  5983 Meningitis/Encephalitis
```
