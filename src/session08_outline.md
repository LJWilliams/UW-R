Outline of Session 08 Pipelines
========================================================

> Companion: slides session08_pipelines (Keynote for me, PDF for others) ...

Consider `01_countrySpecificInterceptSlope.R`. This is an example of a true script that could have evolved from the analysis we've been developing interactively. It is meant to be run unattended.

Let's work up to that. Open RStudio. Get yourself into the project you're using for this bootcamp. Copy `01_countrySpecificInterceptSlope.R` into the top-level project directory. Ensure the RStudio file browser is pointed there so you get good feedback as we proceed.

*Clean out the workspace. Restart the R session.*

Consider `01_countrySpecificInterceptSlope.R`. Step-through the script line-by-line in RStudio. Satisfy yourself that all is well.

*Clean out the workspace. Restart the R session. Manually delete the output files `gCoef.txt` and `gCoef.rds`.*

Use RStudio's `Source` button to source the entire file.

*Clean out the workspace. Restart the R session. Manually delete the output files `gCoef.txt` and `gCoef.rds`.*

Notice that RStudio helpfully shows you what actually happening: a call to the function `source()` with your script as the input. You should see something like this:

```
source('~/teaching/UW-R/01_countrySpecificInterceptSlope.R')
```

Later -- when we're assembling a pipeline with multiple scripts, we'll put lines like this in to run one R script from another. But let's stay focused on this single script for now.

Go to the shell! Make sure you're in the directory where this project and script lives! `pwd`, `cd`, and `ls` help you query, change, and list the contents of your working directory.

Now execute the R script from the shell.

```
Rscript 01_countrySpecificInterceptSlope.R > 01_countrySpecificInterceptSlope.Rout
```

Verify that `gCoef.txt` (open it!) and `gCoef.rds` (don't open it!) got written as expected.

Open the file `01_countrySpecificInterceptSlope.Rout`. This is a rather ugly way to preserve the stuff you would have seen if you'd been executing the script interactively. This is better than nothing but there is a better way.

Return to RStudio and return to the script `01_countrySpecificInterceptSlope.R` in the source editor.

*Clean out the workspace. Restart the R session. Manually delete the output files `gCoef.txt` and `gCoef.rds` and `01_countrySpecificInterceptSlope.Rout`.*

__STOP: Do you have the `knitr` package installed?__ If not, do this:

```
install.packages("knitr", dependencies = TRUE)
```

Click on RStudio's `Compile Notebook` button. A nice looking HTML "report" should appear. The file `01_countrySpecificInterceptSlope.html` should appear in the directory. Open it in your browser.

*Clean out the workspace. Restart the R session. Manually delete the output files `gCoef.txt` and `gCoef.rds` and `01_countrySpecificInterceptSlope.Rout`.*

Let's run the script and create the pretty HTML output from the R console, instead of using a mouse.

```
library(knitr)
stitch_rmd("01_countrySpecificInterceptSlope.R")
```

Verify that `gCoef.txt` and `gCoef.rds` got written as expected.

Open the file `01_countrySpecificInterceptSlope.html` in your browser. Sweet!

Now let's upgrade our "execute the R script from the shell" workflow, i.e. leave the pretty HTML behind instead of the grim `*.Rout`-style of output.

*Manually delete the output files `gCoef.txt` and `gCoef.rds` and `01_countrySpecificInterceptSlope.md` and `01_countrySpecificInterceptSlope.html`. Yeah, now we have a nuisance, intermediate file `01_countrySpecificInterceptSlope.md` that's getting created. Be patient.*

Go back to the shell and do this:

```
Rscript -e "knitr::stitch_rmd('01_countrySpecificInterceptSlope.R')"
```

Verify that `gCoef.txt` and `gCoef.rds` and `01_countrySpecificInterceptSlope.html` were created as expected.

*Manually delete the output files `gCoef.txt`, `gCoef.rds`, `01_countrySpecificInterceptSlope.md`, and `01_countrySpecificInterceptSlope.html`.*

Imagine the script `01_countrySpecificInterceptSlope.r` is just the first piece of a larger pipeline. You will want to execute all the scripts in the pipeline *en masse*. Let's initialize One R Script To Rule Them All, i.e. a master script that will call the other scripts.

File > New File > R Script. Insert this code, copied from above, and File > Save as `Makefile.r`:

```
library(knitr)
stitch_rmd("01_countrySpecificInterceptSlope.R")
```

Go back to the shell and do this:

```
Rscript Makefile.r
```

Verify that all the usual files were written as expected.

Let's say we are *really tired* of all this manual tidying we do. For example, let's make sure the intermediate file `01_countrySpecificInterceptSlope.md` is never left behind. Edit `Makefile.r` to look like this:

```
library(knitr)
stitch_rmd("01_countrySpecificInterceptSlope.R")
file.remove("01_countrySpecificInterceptSlope.md")
```

*Manually delete the output files `gCoef.txt`, `gCoef.rds`, `01_countrySpecificInterceptSlope.md`, and `01_countrySpecificInterceptSlope.html`.*

Go back to the shell and do this:

```
Rscript Makefile.r
```

Verify that all the usual files were written as expected __except__ the intermediate file `01_countrySpecificInterceptSlope.md` was NOT left behind.

Are you sick of manually deleting files to "reset" the process and run it again?

That's why people like to automate pipelines. Typically we use a program called `Make` for this. You may not have `Make` installed, so feel free to just watch this last piece.