# youtube-extremism-replication


The rendered markdown file with outputs: `main-text-figures.md` and `appendix-figures.md`. `renv` ensures that everyone is working from the same environment and with the same package versions. Code was run on R 4.0.2.

# Running the code

1. Fork this repository

2. Open the `.Rproj` and the Rmarkdown files (`.Rmd`) in RStudio. 

3. Run `renv::init()` to automatically install the packages declared in that lockfile into your private project library. Then run `renv::restore()` to restore the state of the project at time of analysis from `renv.lock`. (This step may take a while.)

4. In the `.Rmd`s, run the code chunk by chunk or `knit` to re-render the `.md` in RStudio.


# Code and Data

Only the code and final outputs are provided in the repository, as some source data cannot be made public.

`build.R`  
Contains preprocessing of survey and browser extension data, and produces three user-level datasets:  
   1. `activity_yg_cces.rds` : the primary dataset used for user-level analyses, combines survey data (provided by YouGov), 2018 CCES survey data (provided by YouGov), and browser activity counts for participants where they exist subsetting to only users with any browser activity data.  
   2. `browser_history_yg_cces.rds` : merges the same YouGov data except that it contains browser _history_ variables (used in the appendix) rather than _activity_ data (used for main text), and subsets to only users with any browser history data.
   3. `yg_browser_cces_merged.rds` : a merged table containing survey, browser history, and browser activity data for all participants in the YouGov survey. This dataset is used in comparisons between the full sample of 4,000 and the fraction of those for whom we have browser extension data (e.g., in Table S1 and Figure S13).

Also produces several other aggregated datasets used in the analysis:  
   1. `on_platform_referrers_by_channel.csv` : YouTube referrers by channel.  
   2. `aggregated_referrers_by_channel.csv` : all referrers by channel.  
   3. `recommendation_pipeline.tsv`: pipeline of YouTube recommendations and follows.  
   4. `summarize_subscribe_table.csv`: summary of user subscription results.  

Variables in each dataset are renamed from the YouGov survey (see `DART0034_codebook.pdf` for original names). In `activity_yg_cces.rds` and `browser_history_yg_cces.rds`, we restricted the set of participants in the analyses to those for whom we could capture at least one day of activity/browser history data. `yg_browser_cces_merged.rds` contain data for all participants who took the YouGov survey.

We also provide tables for day-level averages by channel type for browser activity data in `day_count_averages.csv` and `day_time_averages.csv`.
