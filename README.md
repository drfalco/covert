# Discourse-Linked DPs as Covert Partitives

Experimental judgments and plotting code for Michelangelo Falco and Roberto Zamparelli (2024), “Discourse-Linked DPs as Covert Partitives: Ellipsis and pro-form strategies in Italian and English,” *Isogloss* 10(3), 1–33. [Published article](https://doi.org/10.5565/rev/isogloss.345)

## Data

Each CSV has one row per respondent and one column per condition or questionnaire item. The scores are the original five-point judgments. All files are UTF-8.

| File | Respondents | Rating columns | Contents |
| --- | ---: | ---: | --- |
| [dfe.csv](data/dfe.csv) | 20 | 22 | English determiner phrases, by experimental condition |
| [dfi.csv](data/dfi.csv) | 26 | 30 | Italian determiner phrases, by experimental condition |
| [dfeq.csv](data/dfeq.csv) | 26 | 11 | English quantifier items used in the analysis |
| [dfiq.csv](data/dfiq.csv) | 20 | 2 | Italian quantifier items used in the analysis |
| [english.csv](data/questionnaires/english.csv) | 26 | 48 | All rating items in the English questionnaire export |
| [italian.csv](data/questionnaires/italian.csv) | 20 | 63 | All rating items in the Italian questionnaire export |

The two questionnaire files include the items in the corresponding quantifier CSV. Each questionnaire and its quantifier subset share participant labels so that the same person's responses can be matched. Use each rating only once when combining files. The determiner-phrase tables have their own participant labels. In the condition names, `DL` denotes discourse-linked and `nDL` denotes non-discourse-linked.

The public files use randomly assigned study-local labels and are sorted by those labels. The original submission order, recruitment identifiers, timestamps, consent records and one Italian text-response item have been omitted. All numerical judgments and their within-respondent grouping were retained.

## Run the plotting analysis

Install the R packages once if needed.

```r
install.packages(c("tidyverse", "ggpubr", "rstatix", "viridis", "patchwork"))
```

From the repository root, run the following commands in a terminal.

```sh
cd scripts
Rscript --vanilla mf_plots_final.R
```

The [R script](scripts/mf_plots_final.R) reads the four analysis CSVs and writes six numbered plots and one Italian agreement plot to `figures/regenerated/`. The authors’ archived plots are in [figures/](figures/).

On the software versions below, all seven regenerated images matched a run of the original script on the original data. Five also matched the archived images. The files named `fig5.png` and `agreeita.png` differed from their archived copies. The archived filenames follow the development version of the manuscript, and their numbering can differ from the published captions. The original and public scripts both produced warnings from the pairwise-test plotting layer (`stat_pwc`), so a regenerated plot can omit significance annotations. Those plot layers call t-tests. The paper describes two-sided Wilcoxon tests. The archived script does not implement the paper’s Wilcoxon analysis.

The release was tested with R 4.2.3, tidyverse 2.0.0, ggplot2 3.5.1, ggpubr 0.6.0, rstatix 0.7.2, viridis 0.6.4 and patchwork 1.3.0.
