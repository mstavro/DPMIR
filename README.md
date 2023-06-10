# Repository for Replication of "Does Police Militarization Increase Repression?"
Replication data for the paper "Does Police Militarization Increase Repression?" by Martin Stavro [![Orcid ID](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-7527-9582) [![LinkedIn](https://github.com/mstavro/DPMIR/assets/86576037/623ecf46-de3c-4744-a884-3a8febd38720)](https://github.com/mstavro/DPMIR/assets/86576037/a9ce96c4-e38a-46b4-95eb-d21693f3cbf0))](https://www.linkedin.com/in/martinstavro/) and Dr. Ryan Welch.

This document contains instructions on how to replicate the analyses from "Does Police Militarization Increase Repression?" Viewing the commit history also provides a record on how the analyses changed over time (model adjustments, debugging, etc.).


---

## Prerequisites
- R (preferably up to date)
- RStudio (preferably up to date)
- Some free memory

---

## Instructions to Replicate
The code provided is intended to be plug and play. **All you need to do to replicate the analyses in the main paper and appendix is run all the code in "DPMIR Replication Script.R" using CTRL + SHIFT + ENTER.** The R script has been set up to automatically pull the data from the repository by passing the raw content link to the read_csv() function.  

The code is divided into different sections depending on what table/figure is being replicated. In RStudio, you can use these comment dividers for navigation similar to a "table of contents" as seen in the image below.

![image](https://github.com/mstavro/DPMIR/assets/86576037/75ab65ea-5a64-4836-a9ec-288bde6ab168)

**It is highly recommended that you run the code in its entirety in its original order.** This makes it easy to replicate all the data correctly. While the code is divided into sections, _some sections cross-reference objects (regressions, etc.) created in earlier sections, so skipping around may produce errors._

---

## Accessing Replicated Figures/Tables
After running the replication code in its entirely, replicated tables/figures are stored as objects. These objects can be called using this general syntax:
- For tables: **outputT#** (where # is the number of the table. For example, table 1 is outputT1).
- For figures: **outputF#**
- For tables/figures in the appendix: **outputTA#** or **outputFA#**
Note that there is no object for outputF1, as this figure is simply a graphic and was not created in R.

---

## Troubleshooting
### ERROR: SSL Handshake Failed
R failed to fetch the data files from the GitHub repository on its own. This can typically be solved in one of two ways:
1) Try running the script again (this tends to fix it)
2) Manually change the content inside read_csv() to connect to a downloaded version of the GitHub data files on your local machine (an easy way to do this is some_data <- read_csv(file.choose())

---

## Folder and File Descriptions
### Main Folder
- **DPMIR Replication Script.R:** The one-stop shop to replicate everything in the paper.
### Data Folder
- **DPMIR Main Data:** The dataset that most of the paper relies upon - the good stuff.
- **DPMIR US Recoded:** A modification to the main data where the US is recoded to have police militarization (0 -> 1; we address in the paper why it was 0 to begin with).
- **DPMIR US Removed:** A modification to the main data where US cases are removed.
- **Geocoded:** A parsed version of the main data with a geocoded field allowing for a nice visualization with googleVis.
- **License:** The fine print. It's an MIT License. It lets you do with the data what you please, with some stipulations.
