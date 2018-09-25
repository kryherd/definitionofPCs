# Progress & to-do

### Data Imputation

- [x] Create master data file
- [x] Email RAs to make sure all the data is exported
- [ ] Use info from RAs to fully fill out dataset
- [ ] Add participant age (at MRI and at beh) to imputation
- [ ] Add project to imputation
- [ ] Add `ktea2.raw` and `gm.rcomp.raw` to imputation.
- [ ] Re-run imputation using `missForest`.
 


Maybe to-dos...  

- [ ] Check to see if our data is MCAR vs. MAR (Hawkins test?)  
- [ ] Re-do imputation method comparison with `micemd` instead of `mice`

### Group Selection

Once data is successfully imputed...

- [ ] Re-run group selection script on newly imputed data
- [ ] Pick best model(s) for ROI analysis

### ROI Analysis

Once model(s) are picked for group selection...

- [ ] Re-run ROI analysis
- [ ] Interpret significant results


### Writing

Once results are found...

- [ ] Write a paper?