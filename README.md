# shared-mapping
Repo for pulling all info (mail files, holdout files, pricing, circle, etc.) for a shared mailing automatically. This script utilizes redshift and airtable so you will need to have your credentials for both set up within R in order to run the script.

## Set-up

To set up your Airtable API key, go into Airtable and access your account page (top right corner). If there is not already an API key, create one. Once created, copy that key and open up your environment file in R:

```
usethis::edit_r_environ()
```
And then paste in your API key after creating a new variable called "airtable_api_key":
```
airtable_api_key = "YOUR_KEY"
```
If you don't have redshift set up within R refer to this documentation for steps on gettig it set up: https://docs.google.com/document/d/1IUbxUj95HhU4YHh-k0YA7k6HZS-x4JPxyz-9fYRTluk/edit?usp=sharing

##  Dependencies

All the dependencies to run this script are contained within this repo. This script references an Airtable API helper script as well as several files for holdout mappings. It requires the R packages tidyverse, lubridate, and data.table. If needed, install these before running. You should have set up your Airtable and Redshift connections above.

## How to use this repo

This script is contained within the matchbacks-processing folder which should be cloned into the "Work" folder of the matchback you are working on. If you need to run this script outside of that folder structure you will need to update the file paths/references accordingly. Before running this script you should set up a matchback like normal: clone the matchback-processing repo, initialize the project folders, create an R project, and copy in any relevant files from previous matchbacks (e.g., the map file). You are now ready to run the main script in this repo, "airtable_shared_summary".

Open up this file in the R project for the matchback. You can run the code until you print out a list of all shared clients within Airtable. Then, set the client variable to whichever client you are running the matchback, making sure it is exactly how the client is named in Airtable! Once you set the client, you're done. Source the script and everything else should be automated: all mailfiles and holdouts that don't exist already will be pulled and written to the PII/Mailfiles folder and all new mapping information will be added to the mapper.

Double check the outputs to make sure everything looks good and then you should be ready to run the matchback!

### Common Issues

This script is still new, and as such there are some edge cases that could give the script some problems. Some common ones to look out for:
- mailfiles might be deleted due to PII autodelete policies. Make sure that if files need to be redownloaded the filenames stay consistent as to not mess up the mappings
- if you notice a mapping is totally off for a client, this might be caused by an error in Airtable where a client is listed as the wrong partner (they might appear as the "correct" partner but be listed as the wrong partner in the "Combined Client List" field which will cause issues; this usually only occurs if a client is switched from one partner bumber to another within the same envelope). Will need to reach out to the shared team to get this fixed or manually fill out the map for that client
- A/B mappings should be automated based on what is in Airtable, but sometimes the information in Airtable is incorrect and thus will be mapped wrong. If you are unsure, always confirm with the mail team member when and where any A/B splits should be included

If you notice any other issues with the script while running, please let the team know so it can be fixed!
