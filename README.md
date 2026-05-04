
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- use devtools::build_readme() to update -->

# EAVA

Verbal Autopsy Expert Algorithm (EAVA) is a computer-coded verbal
autopsy algorithm (CCVA) which evaluates signs and symptoms present at
the time of death and reported in the 2016 WHO Verbal Autopsy
Questionnaire. If a decedent meets the diagnostic criteria for more than
one cause, a single cause of death is determined using an age-specific
hierarchy of causes. Historically, causes of death have been assigned by
physician coding of questionnaire responses. EAVA produces timely,
reproducible cause of death assignments, enabling physicians to address
other priorities in low resource settings.

## Installation

You can install the development version of EAVA from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("emilybrownwilson/EAVA")
```

Or load the package after installing from CRAN:

``` r
# load EAVA package
#install.packages("EAVA")
library(EAVA)
```

# Codebook

ccva_codebook maps 2016 WHO VA questionnaire questions and their
corresponding names to openVA variables.

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
#>    who2016_var                                                      question
#> 74     id10147 During the illness that led to death, did (s)he have a fever?
#>    openVA_var
#> 74      i147o

# Trace a WHO variable to openVA variables
subset(ccva_codebook, who2016_var == "id10147")
#>    who2016_var                                                      question
#> 74     id10147 During the illness that led to death, did (s)he have a fever?
#>    openVA_var
#> 74      i147o
```

## EAVA Example

This is a basic example which shows you how to load the EAVA package and
run the functions. odk2EAVA() converts 2016 WHO VA questionnaire
variable names and responses to formatting conventions used for openVA.
codEAVA() assign CoDs by EAVA for neonates and children 1-59 months of
age.

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
