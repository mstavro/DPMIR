# Repository for Replication of "Do Not Resist: How Police Militarization Increases Repression"
Replication data for the paper "Do Not Resist: How Police Militarization Increases Repression"

### How to Run Replications
A lot of the code is designed to be plug and play in R or R Studio, making use of the file.choose() command instead of setting a working directory and accessing the files directly.
- The DNR main analysis script uses DNR Data for Main Analysis.csv
- Secondary analysis using Fariss' (2014) latent variable contains the script and data in the same file under code for analyses

### Files Output by Analysis Code
Note: the analysis code will output several files in .html format. These files will be saved to the working directory. If you would like these files to be saved elsewhere, _be sure to change the working directory before running the code_. The code uses file.choose() instead of setting a working directory outright.

Alternatively, jamovi can be used. jamovi contains both the dataset and analyses in one integrated statistical environment similar to IBM's SPSS or Stata. jamovi can be downloaded <a href="https://www.jamovi.org">here.</a>

### Viewing the Data in jamovi
jamovi provides an easy way for users not familiar with R to review the procedures and detailed information contained within the analyses (i.e., the exact p values of each explanatory and control variable) all within a single window. Additionally, jamovi allows for easy tinkering with the data. The .omv file bundles the data and analyses all in one file.

jamovi analyses are also viewable in R syntax mode. The syntax from jamovi is included in the main replication script.

### License Information
Replication files are provided under a GNU General Public License v3.0. This work can be freely shared and modified; any publications of modifications to replication should be done under a GNU GPL v3.0 license.
