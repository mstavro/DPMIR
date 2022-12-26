# Repository for Replication of "Does Police Militarization Increase Repression?"
Replication data for the paper "Does Police Militarization Increase Repression?"

This document contains instructions on how to replicate the analyses from "Does Police Militarization Increase Repression?"

---

### To Do:
*[ ] After repo is made public, use Github links to automatically receive datasets in scripts

---

### Before You Start
Check where your working directory is set. Running these analyses will output multiple files to the working directory (i.e., HTML files for the regression tables). If you do not want files to suddenly appear in your working directory, create a separate file for the output from these scripts and set that to the working directory.

**These scripts will clear your environment prior to running and automatically install the required packages.** Make sure to save/backup anything you would dislike being lost from your R environment prior to running these scripts.

---

### Folder and File Descriptions
#### Main Replication Files
Contains the R script and dataset required to replicate the main results of the paper.
#### CIRI Component Replication Files
Contains the R script and full data required to replicate the component results (ordered logit on torture, killing, disappearances, political imprisonment) in the paper.
#### Robustness Tests
Contains subfolders which house the files required to run robustness tests (ex. PTS substitution for CIRI).
#### Unfiltered Data and Descriptives
Houses the central dataset prior to any filtering/variable selection along with an R script to obtain descriptives.
#### Jamovi Replication File 
A .omv file that allows users to view and manipulate data in jamovi. Intended for users who would prefer a GUI-based environment like SPSS opposed to programming interface like R. Note that the user may have to replicate some analyses in jamovi on their own. See the R scripts for the variable names needed to drag-and-drop into the jamovi prompts.

---
### How to Run Replications
A lot of the code is designed to be plug and play in R or R Studio. By the time this repository is made public, the R scripts should be overhauled to pull data files directly from GitHub. All the user needs to do is run the entire script (and, on occasion, press enter to deal with some diagnostic prompts). 
