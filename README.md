# youtube-extremism-replication


The rendered markdown file with outputs: `main-text-figures.md` and `appendix-figures.md`. `renv` ensures that everyone is working from the same environment and with the same package versions.
Code runs under R 4.0.2.

# Running the code

1. Fork this repository.

2. Open the `.Rproj` and the Rmarkdown files in RStudio. 

3. Run `renv::init()` to automatically install the packages declared in that lockfile into your private project library. Then run `renv::restore()` to restore the state of the project at time of analysis from `renv.lock`. (This step may take a while.)

4. In the `.Rmd`s, run the code chunk by chunk or `knit` to re-render the `.md`
