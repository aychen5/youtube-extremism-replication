# youtube-extremism-replication


The rendered markdown file with outputs: `main-text-figures.md` and `appendix-figures.md`. `renv` ensures that everyone is working from the same environment and with the same package versions.

# Running the code

1. Fork this repository

2. Open the `.Rproj` and the Rmarkdown files in RStudio. 

3. Run `renv::init()` to automatically install the packages declared in that lockfile into your private project library. Then run `renv::restore()` to restore the state of the project at time of analysis from `renv.lock`. (This step may take a while.)

4. In the `.Rmd`s, run the code chunk by chunk or `knit` to re-render the `.md`


# Data

`build.R` produces the user-level datasets: `activity_yg_cces.rds`, `browser_history_yg_cces.rds`, and `yg_browser_cces_merged.rds`. Variables from the YouGov survey are renamed (see `DART0034_codebook.pdf` for original names). Additional preprocessing of survey and browser extension data are in this script. The latter portion of the script also contains code to generate referrers (`on_platform_referrers_by_channel.csv`, `aggregated_referrers_by_channel.csv`), recommendations (`recommendation_pipeline.tsv`), and subscriptions (`summarize_subscribe_table.csv`) datasets. Only the code and final outputs are provided in the repository, as some source data cannot be made public.

The primary dataset used for user-level analyses is `activity_yg_cces.rds`, which combines survey data (provided by YouGov), 2018 CCES survey data (provided by YouGov), and browser activity counts for participants where they exist subsetting to only users with any browser activity data. We also restricted the set of participants in the analyses to those for whom we could capture at least one day of activity data. `browser_history_yg_cces.rds` merges the same YouGov data except that it contains browser _history_ variables (used in the appendix) rather than _activity_ data (used for main text), and subsets to only users with any browser history data. `yg_browser_cces_merged.rds` is data for all participants who took the YouGov survey.


We also provide user by day tables (???)

