# Repository for Replication of "Does Police Militarization Increase Repression?"
Replication data for the paper "Does Police Militarization Increase Repression?"

This document contains instructions on how to replicate the analyses from "Does Police Militarization Increase Repression?" Viewing the commit history also provides a record on how the analyses changed over time (model adjustments, debugging, etc.).

---

### Before You Start
Check where your working directory is set. Running these analyses will output multiple files to the working directory (i.e., HTML files for the regression tables). If you do not want files to suddenly appear in your working directory, create a separate file for the output from these scripts and set that to the working directory.

**These scripts will clear your environment prior to running and automatically install the required packages.** Make sure to save/backup anything you would dislike being lost from your R environment prior to running these scripts.

---

### General Notes on How to Run Replications
A lot of the code is designed to be plug and play in R or R Studio. By the time this repository is made public, the R scripts should be overhauled to pull data files directly from GitHub. All the user needs to do is run the entire script (and, on occasion, press enter to deal with some diagnostic prompts). 

---

### Folder and File Descriptions
#### DPMIR Main Analysis Replication Code.R
The main analysis file. Contains the code needed to replicate the primary tables. It pulls from DPMIR Main Data.csv. It should be plug-and-play; all that is needed is to run the code. When this repository goes public, the code should be updated and will pull the data from GitHub automatically.
#### DPMIR Data Descriptives.R
Provides descriptives from the dataset ("DPMIR Main Data.csv").
#### LICENSE
The code/analyses in this repository are provided under an MIT license. If you are interested in specific terms see this file.
#### Robustness Tests
Contains subfolders which house the files required to run robustness tests (ex. PTS substitution for CIRI). These robustness checks are found both in the main paper and appendix. There may be some additional robustness checks not included in the appendix (ex. ITT or USAID). Some replication subfolders have accompanying data that is different from the main dataset. If this is the case, these scripts should be run with their associated .csv in the subfolder. Comments in the code should indicate which file to use. When this repository goes public, the scripts should contain links to the appropriate datasets which will automatically pull the required data.
#### geocoding.csv
A file used to produce a googleVis map of the countries covered by the dataset. Most users probably will not need this. For those who are interested in seeing the map, remove the comment tags in the main data replication file.
#### Jamovi Replication File 
A .omv file that allows users to view and manipulate data in jamovi. Intended for users who would prefer a GUI-based environment like SPSS opposed to programming interface like R. Note that the user may have to replicate some analyses in jamovi on their own. See the R scripts for the variable names needed to drag-and-drop into the jamovi prompts (ex. police, repress_index, repress_index_lagged, polity2_P4, etc.).

---

### To Do:
* [ ] After repo is made public, use Github links to automatically receive datasets in scripts
