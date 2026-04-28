
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- use devtools::build_readme() to update -->

# EAVA

Verbal Autopsy Expert Algorithm is a computer-coded verbal autopsy
algorithm (CCVA) which assigns causes of death using responses obtained
from the 2016 World Health Organization Verbal Autopsy Questionnaire.
Historically, causes of death have been assigned by physician coding of
questionnaire responses. CCVAs offer several advantages, providing
reliable, reproducible cause of death determination and as well as
enabling physicians to address other priorities in low resources
settings.

## Installation

You can install the development version of EAVA from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("emilybrownwilson/EAVA")
```

## Example

This is a basic example which shows you how to load the packag and run
the functions:

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

# Codebook

The signs and symptoms which EAVA uses to determine causes of death are
presented in the codebook

``` r
# expolore symtptoms and function mapping
data(ccva_codebook)

# View first few mappings
head(ccva_codebook)
#>   who2016_var                                  question openVA_var
#> 1     id10004      Did s(he) die during the wet season?      i004a
#> 2     id10004      Did s(he) die during the dry season?      i004b
#> 3     id10019                              Was he male?      i019a
#> 4     id10019                           Was she female?      i019b
#> 5     id10022 Was s(he) aged 65 years or more at death?      i022a
#> 6     id10022   Was s(he) aged 50 to 64 years at death?      i022b

# Look up a specific openVA variable
subset(ccva_codebook, openVA_var == "i147o")
#>    who2016_var                                               question
#> 74     id10147 During the illness that led to death, did (s)he have a
#>    openVA_var
#> 74      i147o

# Trace a WHO variable to openVA variables
subset(ccva_codebook, who2016_var == "Id10147")
#> [1] who2016_var question    openVA_var 
#> <0 rows> (or 0-length row.names)
```
