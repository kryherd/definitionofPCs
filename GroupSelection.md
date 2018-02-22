# Group Selection

One of the things we were most interested in when exploring this method is how changing around parameters of the regression changes the composition of the three main groups (EAC, UGC, UPC). We did three main things: change confidence interval, change reading comprehension measure, and change predictors.

In this repository, you'll find a core R script called [GroupSelection](./GroupSelection.R). Since each of the main explorations rely on the same type of script, I'm only uploading this main one. There are a few things we always do. 

**1) Create a decoding composite.**

We use four measures (word reading, nonword decoding, word fluency, and nonword fluency) to create a more stable deocding composite. You can see more about how this composite was created in the [create_composite](./create_composite.R) R script.

**2) Check predictors for normality.**

We make pretty good use of the `caret` package, specifically `preProcess`. After testing our predictors for normality using `dagoTest`, we often centered, scaled, and transformed them.

For more information and documentation on this function, check out [this](https://topepo.github.io/caret/pre-processing.html) page.

**3) Compare group differences.**

Once we ran the classifier regression model and assigned each subject to a group, we always checked the groups for differences on the measures included in the original regression (e.g., reading comprehension, age, nonverbal IQ, etc.). Ideally, the groups would be equal on all of the predictors and different on reading comprehension. This was not always the case.

**4) Check group sizes.**

We wanted mostly similar group sizes, so we checked the group sizes for each model.

**5) Removed subjects with a standardized predicted reading comprehension of -1 or less.**

Our study is primarily focused on people who have a discrepancy between their reading comprehension ability and their word reading ability. Individuals with a standardized predicted reading score of -1 have low scores on the predictors (likely word reading) as well as reading comprehension. This is definitely an interesting group (see literature on dyslexia and reading disorder!), but not relevant for our current investigation. 

Now, onto the things we explored!

## Confidence Intervals

In this method, we usually use confidence intervals (CIs) to place individuals into groups. To be a UGC, their measured reading comprehension ability has to be above some CI around their predicted reading comprehension. Similarly, UPCs have a measured reading comprehension score below some CI around the predicted reading comprehension. Finally, EACs have a measured score within some CI around the predicted score.  The original papers that have been published using this method use different CIs. to select their groups. For UPCs and UGCs, the CIs vary from 65 to 80%. For EACs, they vary from 15 to 25%. However, these papers rarely provide a rationale for using these different CIs.

One key part of my code was used to easily switch between CIs.

```
ci.to.use <- c(.15, .65)
ci.title <- "CIs: 15%, 65%"
```
The first number was the within-CI for the EACs, and the second was the outside-CI for the UPCs and UGCs.

![animated-CI](./Images/animate.gif)

