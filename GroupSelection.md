# Group Selection

One of the things we were most interested in when exploring this method is how changing around parameters of the regression changes the composition of the three main groups (EAC, UGC, UPC).

In this repository, you'll find a the results of an R markdown file called [classifier](./Classifier.md). The code in this file contains loops to run multiple different models and test multiple CIs all at once.

The main layout of the document is as follows:

**1) Data Imputation** 

Across 4 projects, we have 902 total subjects. These are subjects with some reading-related behavioral data; some of them also have structural MRI data. Our goal here is first to use our classifier to create groups based solely on behavioral data and then figure out how many individuals in each group we have MRI data for. Since this is already a process where we drop subjects, we don't want to be dropping people for missing a single behavioral score. Our dataset is somewhat large (depending on your definition) and has many measures; thus, it's good for imputation. 

We are using the findings from [Eckert et al. (2018)](https://www.frontiersin.org/articles/10.3389/fpsyg.2018.00644/full) to inform our imputation methods. They found the most success with the [`missForest` package](https://cran.r-project.org/web/packages/missForest/index.html) (as compared to mean replacement and predictive mean matching using the [`mice` package](https://cran.r-project.org/web/packages/mice/index.html)). 

In this paper they do **explicit multiple imputation**, where 10 imputed datasets are generated and then pooled to for point and variance estimates. However, personal communication with the creator of `missForest`, [Daniel Stekhoven](https://www.sib.swiss/stekhoven-daniel), suggests that this method is not necessary. In his words:

> [...] randomForest provides an implicit multiple imputation by averaging over many decision/regression trees [...] When we use the different imputation methods, missForest was so much better (while at the same time underestimating the standard deviation of the CIs) that my intermediate hypothesis is; we do not need multiple imputation if we have the right data (when the data is right, I have not yet figured out).

So, it seemed like we could just use `missForest` without having to worry about multiple imputation. To check this, we compared explicit multiple imputation (using `mice`) to `missForest`. To see the results of this investigation, check out the LINK HERE document.

**2) Create a decoding composite.**

We use four measures (word reading, nonword decoding, word fluency, and nonword fluency) to create a more stable deocding composite. 

**3) Run model**

We use a typical linear model to predict reading compreension ability. Before running the mode, we make pretty good use of the `caret` package, specifically `preProcess`, to check the normality of our predictors. Skewed predictors were centered, scaled, and transformed.

For more information and documentation on `preProcess`, check out [this](https://topepo.github.io/caret/pre-processing.html) page.

**4) Creating Groups**

In this section, we apply different confidence intervals to the residuals from the main model. The core idea is that the relationship between predicted and actual reading comprehension scores tells us which group they should be in. The groups are as follows.

* UPC: unexpected poor comprehender (pred. RC > actual RC)
* NSC: non-selected
* EAC: expected average comprehender (pred. RC = actual RC)
* UGC: unexpected good comprehender (pred. RC < actual RC)
* EPC: expected poor comprehender (pred. and actual RC are below some cutoff)

Our study is primarily focused on people who have a discrepancy between their reading comprehension ability and their word reading ability. Individuals with a standardized predicted reading score of -1 have low scores on the predictors (likely word reading) as well as reading comprehension. This is definitely an interesting group (see literature on dyslexia and reading disorder!), but not relevant for our current investigation. 

**5) Merge Groups with MRI IDs**

We ran the classifier on our full dataset, since it works best with large data. However, we don't have MRI data for all subjects. Since we want to do a structural MRI analysis, we need to restrict our groups to those with structural MRI data. That happens in this step.

**6) Check group differences and group sizes**
Once we ran the classifier regression model and assigned each subject to a group, we always checked the groups for differences on the measures included in the original regression (e.g., reading comprehension, age, nonverbal IQ, etc.). Ideally, the groups would be equal on all of the predictors and different on reading comprehension. This was not always the case.

We wanted mostly similar group sizes, so we checked the group sizes for each model.


## Confidence Intervals

In this method, we usually use confidence intervals (CIs) to place individuals into groups. To be a UGC, their measured reading comprehension ability has to be above some CI around their predicted reading comprehension. Similarly, UPCs have a measured reading comprehension score below some CI around the predicted reading comprehension. Finally, EACs have a measured score within some CI around the predicted score.  The original papers that have been published using this method use different CIs. to select their groups. For UPCs and UGCs, the CIs vary from 65 to 80%. For EACs, they vary from 15 to 25%. However, these papers rarely provide a rationale for using these different CIs.

The first number is the within-CI for the EACs, and the second is the outside-CI for the UPCs and UGCs.

<img src="./Images/animate.gif"> 

This image shows what hapens when you change around the CIs. Depending on how much you change them, the group sizes change a lot. The CIs that we primarily looked at were 15-60, 15-65, 15-70, and 20-70, since they most closely matched the literature. 

We relied on the group difference analyses to help us pick the best CIs.


## Reading Comprehension Measures

Our lab collects a lot of individal difference measures. With this dataset, we had four different reading comprehension measures. Not all subjects had data for all of the measures, but we used imputation methods to deal with that (for more details on how we imputed, you'd have to ask Clint!). 

Our reading comprehension measures included the Kaufman Test of Educational Achievement (KTEA) Reading Comprehension subtest, the Woodcock-Johnson III (WJ3) Passage Comprehension subtest, the Nelson-Denny (ND) Comprehension subtest, and the Gates-MacGinitie (GM) Comprehension subtest. 

We pretty quickly settled on WJ3 and KTEA -- WJ3 because it had the most complete data and KTEA because it produced the most balanced groups. We also made a reading comprehension composite using WJ3 and KTEA. Interestingly, the groups we found using the different reading comprehension measures were often different, even though all of the measures were ostensibly tapping the same construct (reading comprehension). This is related to research by [Keenan and Meenan (2014)](http://journals.sagepub.com/doi/full/10.1177/0022219412439326) which found that using different reading comprehension tests to define poor comprehenders led to very different groups, with an average overlap of **only 43%**.

## Predictors

Across studies, the specific measures used to predict reading comprehension ability vary. Most include age, nonverbal IQ (matrix reasoning or block design) and decoding (word and/or nonword). However, some include vocabulary (expressive or receptive) or even word fluency.

To explore these predictors, we ran a bunch of models. We had some with expressive vocabulary, some with receptive, and some with a composite of the two. We generally found that including vocabulary was necessary to keep the groups equal on things like decoding and nonverbal IQ.

We also ran some models that included oral comprehension. The rationale here was to go beyond the Simple View of Reading (reading comprehension = decoding + oral comprehension). If individuals showed a discrepancy between their actual reading comprehension ability and a predicted value that included both decoding and oral comprehension, this discrepancy must be due to something that is not accounted for by the Simple View. Including oral comprehension did sometimes lead to well-balanced groups (depending on the CIs). However, for theoretical reasons we decided to use vocabulary and not oral comprehension in future analyses. This model could still be further explored to understand what contributes to comprehension beyond decoding and oral comprehension, though!
