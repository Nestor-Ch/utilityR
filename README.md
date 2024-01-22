# utilityR general use and the cleaning process.

The package is devoted to a number of functions used within the standard Reach Ukraine cleaning processes. The package is composed of a number of function 'families' each
dealing with a specific aspect of the data manipulation. Please read vignettes and individual function documentation to discover more about each of the families in greater
detail. you can browse the vignettes of this package by calling `vignette(package='utilityR')`.
The text below presents a comprehensive overview of the cleaning process and how each of the functions are used within their respective frameworks.
The text below follows the structure of the cleaning template presented in the markdown templates of this package.

## Table of Contents
- [Open up the cleaning template](#Open-up-the-cleaning-template)
- [Duplicates and no-consents](#Cleaning-duplicates-and-no-consent-entries)
- [Audit checks and soft duplicates]($Audit-checks-and-soft-duplicates)

### Open up the cleaning template

1. Load the utilityR library
2. Create a new file using the utilityR markdown template
3. Create a new Rmarkdown file by clicking 'File'->'New File'->'R Markdown'->'From Template'->'Cleaning template'. This will create a new directory in your local machine that will host all of the cleaning template files as well as folder subdirectories that you'll need for the cleaning.
4. Load the kobo tool into the 'resources' folder. Load the raw data into the 'data\inputs\kobo_export' folder and the audit files into 'data\inputs\audit' folder if you have them.
5. Start the `1_cleaning.R` file.

### Initialization

Prior to running anything fill up the `directory_dictionary` list with the relevant names. Load the API key file and run the `init.R` and `load_Data.R` files. Usually, no inputs from your side are needed for these two bits of script.

The raw data are saved in the `kobo.raw.main` and `kobo.raw.loopx` files that are later duplicated into `raw.main` and `raw.loopx` files. This is convenient for cases where you want to re-run your cleaning procedure from scratch

If you need to modify your kobo tool in any way, please do so within the `src/sections/tool_modification.R` file. This is reserved for the cases where the tool was changed in the middle of data collection.

### Cleaning duplicates and no-consent entries

All entries marked for deletion are kept in the `deletion.log.new` object - this is our deletion log.

The initial step that the script goes through is the removal of duplicates. The algorithm for finding them in the `raw.main` is standard - finding the duplicate `uuid` entries and dropping them. We cannot do this for the `raw.loopx` objects as their unique identifier `loop_index` works as a row index. We will never be able to find any duplicates by investingating this column. However, each individual loop entry is assigned to some entry from the `raw.main` object through the `uuid` and the `parent_index` columns. Each set of household members should have 1 unique `uuid` and `parent_index`. The script finds those entries that have more than one `parent_index` per a unique `uuid` and marks them as duplicates. 

At the end of this process, those entries are added into the `deletion.log.dupl` object.

***Improtant***

**`deletion.log.new` and `deletion.log.dupl` objects are kept separate until we have added everything we need into the `deletion.log.new` object and deleted all of the needed entries from our dataset. These objecs are merged only prior to writing the deletion.log excel file after Section 3 of the cleaning script**

This is done because most other checks that we run check for the general invalidity of the survey and delete entries if they match the `uuid`. If we were to merge the two objects together, the script would delete all the entries that match the duplicate `uuid` index while ignoring the fact that:
1. While there may be two of these entries in the data, that doesn't mean that it's not a valid entry, we just don't need two of them
2. The deletion of entries by `uuid` within the loop will cause the deletion of all loop entries that match this `uuid`, even if they're not duplicated.

The next bit of the script checks the **No consent** entries in the data and requires the user's input. The user has to define the condition that classifies an entry as a no-consent entry and filter the dataframe by it, thus creating the `no_consents` data.frame object.

The last bit of this script is trying to find the test entries by parsing the enumerator's comments column that you've specified in the `directory_dictionary$enum_comments` element of the list to find entries that say 'test' in Ukrainian, or English.

If you want to add any other checks for general validity of the data, you can add them here and merge the resulting cleaning log files into the `deletion.log.new` object.

### Audit checks and soft duplicates




<!-- badges: start -->

[![R-CMD-check](https://github.com/Nestor-Ch/utilityR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Nestor-Ch/utilityR/actions/workflows/R-CMD-check.yaml) [![codecov](https://codecov.io/gh/Nestor-Ch/utilityR/graph/badge.svg?token=BYYLEDL4XU)](https://codecov.io/gh/Nestor-Ch/utilityR)

<!-- badges: end -->
