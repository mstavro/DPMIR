# Repository for Replication of "Does Police Militarization Increase Repression?"
Replication data for the paper "Does Police Militarization Increase Repression?"

**overhaul of README needed**

### Folder and File Descriptions
#### Main Replication Files
Contains the R script and dataset required to replicate the main results of the paper.
#### CIRI Component Replication Files
Contains the R script and full data required to replicate the component results (ordered logit on torture, killing, disappearances, political imprisonment) in the paper.
#### Robustness Tests
Contains subfolders which house the files required to run robustness tests (ex. PTS substitution for CIRI).
#### Unfiltered Data and Descriptives
Houses the central dataset prior to any filtering/variable selection along with an R script to obtain descriptives.
#### Jamovi Replication File (DEPRECATED)
Provides a .omv file that contains the dataset in jamovi as well as some analyses. Provided for interested parties who would like to work with a graphical interface (similar to IBM SPSS) instead of R code.

### How to Run Replications
A lot of the code is designed to be plug and play in R or R Studio, making use of the file.choose() command instead of setting a working directory and accessing the files directly.
- Navigate to the "Main Replication Files" folder, open the R script, and use the file explorer window to choose the accompanying .csv file

### Files Output by Analysis Code
Note: the analysis code will output several files in .html format. These files will be saved to the working directory. If you would like these files to be saved elsewhere, _be sure to change the working directory before running the code_. The code uses file.choose() instead of setting a working directory outright.

Alternatively, jamovi can be used. jamovi contains both the dataset and analyses in one integrated statistical environment similar to IBM's SPSS or Stata. jamovi can be downloaded <a href="https://www.jamovi.org">here.</a>

### Viewing the Data in jamovi
jamovi provides an easy way for users not familiar with R to review the procedures and detailed information contained within the analyses (i.e., the exact p values of each explanatory and control variable) all within a single window. Additionally, jamovi allows for easy tinkering with the data. The .omv file bundles the data and analyses all in one file.

jamovi analyses are also viewable in R syntax mode. The syntax from jamovi is included in the main replication script.
