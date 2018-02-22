# Group Analysis of Structural MRI Data

Besides having a large dataset of behavioral measures, we also have a lot of MRI data. Functional tasks were different across grants, but the structural data was acquired in a similar-enough way to allow us to analyze group differences. Having data from multiple grants gave us a really large dataset to work with and substantial groups (~40 per group). [Previous studies](https://link.springer.com/article/10.1007/s11881-015-0114-y) looking at structural MRI in poor comprehenders were only able to acquire relatively small group sizes (~10-15 per group), which is more in line with the group sizes we would have attained from any of our individual grants alone. *(so, collaborative science is great!)*

This analysis had a few steps.  

1. I used the methods described in [Group Selection](./GroupSelection.md) to create groups that were balanced on predictor measures (nonverbal IQ, age, vocab, decoding) and were relatively similar in size. We often picked a few different CI/predictor/comprehension measure combinations that were all reasonable.  
2. Andy exported cortical thickness and gray matter volume measurements for each subject in many ROIs using [Freesurfer](https://surfer.nmr.mgh.harvard.edu/) (Andy really knows a lot about neuroimaging -- check out his [blog](https://www.andysbrainblog.com/) or [YouTube channel](https://www.youtube.com/channel/UCh9KmApDY_z_Zom3x9xrEQw) if you have a chance).
3. I ran linear models to see if group had a significant effect on thickness or volume, controlling for either mean thickness or mean intracranial volume as well as age.
4. I ran *post-hoc* analyses to break down significant effects of group. 

Running this analysis really got me used to running for loops over lists of data frames in R. I had to run these models for multiple group selection methods over lots of regions of interest (ROIs). I also learned more about how to read in multiple sheets of an Excel document and how to send R output to a text document.

To learn more about this analysis, you can check out the [Cortical ROI analysis](./ROI_analysis_cortical.R) R script. You can also look at a [poster](./) I presented at the Society for the Neurobiology of Language conference in Fall 2017.

 definitionofPCs/SNL_2017_Ryherd.pdf 