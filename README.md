# utilityR general use and the cleaning process.

The package is devoted to a number of functions used within the standard Reach Ukraine cleaning processes. The package is composed of a number of function 'families' each
dealing with a specific aspect of the data manipulation. Please read vignettes and individual function documentation to discover more about each of the families in greater
detail. you can browse the vignettes of this package by calling `vignette(package='utilityR')`.
The text below presents a comprehensive overview of the cleaning process and how each of the functions are used within their respective frameworks.
The text below follows the structure of the cleaning template presented in the markdown templates of this package.

## Table of Contents
- [Open up the cleaning template](#Open-up-the-cleaning-template)

### Open up the cleaning template

1. Load the utilityR library
2. Create a new file using the utilityR markdown template
3. Create a new Rmarkdown file by clicking 'File'->'New File'->'R Markdown'->'From Template'->'Cleaning template'. This will create a new directory in your local machine that will host all of the cleaning template files as well as folder subdirectories that you'll need for the cleaning.
4. Load the kobo tool into the 'resources' folder. Load the raw data into the 'data\inputs\kobo_export' folder and the audit files into 'data\inputs\audit' folder if you have them.
5. Start the `1_cleaning.R` file.

### Initialization

Prior to running anything fill up the `directory_dictionary` list with the relevant names. Load the API key file and run the `init.R` and `load_Data.R` files. Usually, no inputs from your side are needed for these two bits of script.
The raw data are saved in the `kobo.raw.main` and `kobo.raw.loopx` files that are later duplicated into `raw.main` and `raw.loopx` files. This is convenient for cases where you want to re-run your cleaning procedure from scratch




<!-- badges: start -->

[![R-CMD-check](https://github.com/Nestor-Ch/utilityR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Nestor-Ch/utilityR/actions/workflows/R-CMD-check.yaml) [![codecov](https://codecov.io/gh/Nestor-Ch/utilityR/graph/badge.svg?token=BYYLEDL4XU)](https://codecov.io/gh/Nestor-Ch/utilityR)

<!-- badges: end -->
