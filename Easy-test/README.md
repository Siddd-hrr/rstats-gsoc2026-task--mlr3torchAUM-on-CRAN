## Easy-test

### Prerequisites
-   R (version 4.0 or higher)
-   Rtools (Windows) or build-essential (Linux)

### Result 1
- [Typical_Usage_Vignette](Results/Typical_usage.html)

### STEPS TO PRODUCE RESULT_1 
-  **Step1: fetch and clone the mlr3torchAUM repository:**
```bash 
    git clone https://github.com/tdhock/mlr3torchAUM.git
    cd mlr3torchAUM
```
- **Step2: Install required packages to render a Vignette (note: using R' terminal)**
```bash
    install.packages(c("rmarkdown", "knitr"))
```
- **Step3: Create the directory and render the Typical_Usage Vignette inside it (note: using R' terminal)**
```bash
    dir.create("../Results", showWarnings = FALSE)
    
    rmarkdown::render("vignettes/Typical_usage.Rmd",
    output_format = "html_document",
    output_dir = "../Results"
    )
```

### Result 2
- [Output log Report](Results/easy-test-report.txt)

- [R-Script Code](Results/easy_test2.R)

**Question: Are they consistent? **
-   “Re-running the blog post code produced outputs consistent with the post"

-   Aanlysis/Reason: I see 1 minority sample (R) in every batch with min_samples_per_stratum=1, shuffle=TRUE gives different indices 
    each epoch while shuffle=FALSE keeps them fixed, and fixing seed=1 makes repeated runs produce the same learned weights.
    So, therefore the result matches the blog reults, as they give similar analysis of the log report. 



