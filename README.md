<!--<style>
img{padding-bottom:0.1em;}
</style>-->

# shinyElement
[![DOI](https://zenodo.org/badge/doi/10.24433/CO.2852743.v1.svg)](https://doi.org/10.24433/CO.2852743.v1)

<em>For an identical version of this readme that has the repo's contents hidden: [https://MetzgerSK.github.io/shinyElement](https://MetzgerSK.github.io/shinyElement)</em>

[This](https://github.com/MetzgerSK/shinyElement) repository houses 12\* of the 12 Shiny apps from *Using Shiny to Teach Econometric Models* by Shawna K. Metzger (Cambridge University Press).  They are identical to the files in the book's Code Ocean capsule of record [here](https://doi.org/10.24433/CO.2852743.v1).  (\*`LASSO_bchamp_lite` is a stripped-down version of the twelfth app.)  

By being in a GitHub repo, the 12\* apps can be run directly via [Binder](https://mybinder.org) using the links below.  Other than clicking the link and waiting <~60 seconds, there's nothing additional you need to install or do.

:arrow_right: **TL;DR: Click <span style="vertical-align:middle;">![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)</span> to launch an app.**

## The Manuscript
Until the Element's released by Cambridge in late 2020/early 2021, you can access the final submitted manuscript in PDF form [here](https://bit.ly/3kHPk7O).

## The Apps

### Ch. 2 ###
- `whySurv`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/whySurv/)</span> <span style="font-size:0.75em;">:hourglass_flowing_sand:</span><br>
	Examines two of the reasons why OLS can perform poorly when the dependent variable is some length of time (i.e., a duration).
- `mleLogit`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/mleLogit/)</span><br>
	Focuses on the intuition behind maximum likelihood using a logit model.  See `mleLM` description below for more about all the maximum likelihood apps' general properties.

### Ch. 3 ###
- `leastSq`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/leastSq/)</span><br>
	Focuses on the intuition behind the least-squares line.  Students manually minimize the sum of squares for a randomly generated set of data by selecting various slope and intercept values.  The app displays a graph with the proposed best-fit line superimposed over the fake data.  The line updates based on students' slope + intercept selections.
- `mleLM`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/mleLM/)</span><br>
	Focuses on the intuition behind maximum likelihood.  Students manually find the maximum of a linear model's likelihood function for a toy set of data.  Like `leastSq`, `mleLM` displays a graph with the proposed best-fit line and fake data, and this line updates as students select different slope + intercept values.

*Note: After generating a dataset, you can view each app's objective function by clicking the upper-right switch to "Formal," then clicking the :eye: button that appears next to "Current...Value."*

### Ch. 4 ###
- `olsApp`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/olsApp/)</span> <span style="font-size:0.75em;">:hourglass_flowing_sand:</span><br>
	A multi-part app that explores the ramifications of violating the Gauss&ndash;Markov assumptions for OLS estimates, as well as the normally distributed error assumption.  For each violation, the app executes Monte Carlo simulations using parameter values selected by the student.  Among other things, it reports the simulation results, as well as various diagnostic plots for each individual simulation draw.
- `linRegEstms`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/linRegEstms/)</span> <span style="font-size:0.75em;">:hourglass_flowing_sand:</span><br>
	A variant of `olsApp`, focusing on efficiency.  It compares the performance of different estimators in the presence of various classic linear regression assumption violations.

### Ch. 5 ###
- `leastSqLASSO`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/leastSqLASSO/)</span><br>
	A variant of `leastSq`, adapted for LASSO.
- `LASSO_bchamp`: *Code Ocean capsule only (file sizes too big)*<br>
> <em> Uses replication data from Nicholas Beauchamp's ["Predicting and Interpolating State-Level Polls Using Twitter Textual Data"](http://dx.doi.org/10.1111/ajps.12274
) (2017, <span style="font-style:normal;">American Journal of Political Science</span>) to introduce students to LASSO models, allowing them to manually change <span style="font-style:normal;">&lambda;</span>'s value and observe the performance of the resulting predictive model.  Also includes interactive data viewer to examine word usage on a state&ndash;date basis.</em>
- `LASSO_bchamp_lite`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/LASSO_bchamp_lite/)</span><br>
Same as `LASSO_bchamp`, but stripped down to basic functionality to allow the app to be run from Binder.  (No interactive data viewer.)

### Appendix C ###
- `predProbs`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/predProbs/)</span><br>
	The simple app from Appendix C's how-to.  It generates predicted probabilities from a logit model predicting whether a _Titanic_ passenger survived the sinking.
- `predProbs` 2.0&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/predProbs2/)</span><br>
	Same as `predProbs`, but spiffed.
- `predProbs_HMST`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/predProbs_HMST/)</span><br>
	Same app structure as `predProbs`, but modified to allow students to replicate the predicted probability tables from an actual published article.  Target table: Table 3 in Hensel, Mitchell, Sowers, and Thyne's ["Bones of Contention"](http://dx.doi.org/10.1177/0022002707310425
) (2008, <em>Journal of Conflict Resolution</em>). 
- `predProbsMNL`&nbsp;&nbsp;&nbsp;<span style="vertical-align:middle;">[![Launch Rstudio Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/MetzgerSK/shinyElement/major?urlpath=shiny/predProbsMNL/)</span><br>
	Same app structure as `predProbs`, but adapted to multinomial logit.  Generates predicted probabilities for Mexican voters' vote choice in the first round of the 2012 presidential election.

## Notes
- The apps may take a minute or two to launch.  Hang tight.  
	- The Binder page should load in a new window/tab once you click the link.  If it doesn't, click the link again or manually copy/paste the URL into a new window/tab.
	- On the Binder page, as long as `Found built image, launching...` appears under the "Build Logs" header (click 'show' link at header's right to display the log), things are working fine.  
	- <span style="font-size:0.75em;">:hourglass_flowing_sand:</span> = more complex apps. They'll take longer than the others to launch.
	- If there's no text in the log after 15 seconds or so, refresh the page.  Rinse and repeat until the app loads.
	- If the app doesn't load within a minute or two after `server running at https://...` displays (it should auto-redirect), either type that URL manually into your browser or hit refresh. Rinse and repeat until the app loads.
- Every click launches a separate Binder instance, even for the same app.  This means multiple people can use the same app at once without any of the bottlenecking issues endemic to Shiny that stem from R being single-threaded.  
- <a name="fn1_ret"></a>To launch a different app, either come back to the GitHub repo and click the app's Binder link *or* hit :back: twice.<sup><a href="#fn1">1</a></sup> 

## Technical Notes
- All the apps' Docker images have been prebuilt.  
- Binder suggests each Binder'd GitHub repo can support ~100 simultaneous users.  My own rough tests suggest at least 50, under certain circumstances.
- With how Binder's memory allocation works, the more any of the apps get used, the faster any of them will load.
- If you fork any of the apps and make new commits in your forked repo, you'll have to go to the Binder [homepage](https://mybinder.org/) to generate a new link before you can run it on Binder.  When you first access the generated link, Binder will build the Docker image.  That usually takes anywhere from 20&ndash;40 minutes for the apps in their current state, depending.
- Once a Binder is running, you can edit and run the code for *any* of the repo's apps in a RStudio session by changing the end of the URL from <code>shiny/<em>appName</em>?token=...</code> to <code>rstudio/?token=...</code>.  (Others won't be able to see your edits.) <br/> > <ins>Note</ins>: Any changes you make to the code **won't be stored**.  Save local copies of anything you need before closing the window/tab.

## Other Apps
There are additional Shiny apps for regression models usually covered in MLE/GLM courses [here](https://github.com/MetzgerSK/shinyAdvReg).

## License
[MIT](https://choosealicense.com/licenses/mit/)
<br/>
<br/>

----
<a name="fn1">1</a>: You can also technically load another app from the GitHub repo by modifying your current Binder session.  The end of the current app's URL will be <code>shiny/<em>appName</em>?token=...</code>.  Replace <code><em>appName</em></code> in the URL with the other app's name and hit <kbd>Enter</kbd> to load. <span style="font-size:0.75em"><a href="#fn1_ret">↩</a></span>