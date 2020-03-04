
<!-- README.md is generated from README.Rmd. Please edit that file -->
memr
====

Medical records embeddings
--------------------------

The `memr`(Multisource Embeddings for Medical Records) package in R allows for creating embeddings, i.e. vector representations, of medical free-text records written by doctors. It also provides a wide spectrum of tools to data visualization and medical visits' segmentation. These tools aim to develop computer-supported medicine by facilitating medical data analysis and iterpretation. The package can be exploited for many applications like the recommendation prediction, patients' clustering etc. that can aid doctors in their practice.

Installation & Dependences
--------------------------

`memr` is written in R and is based on the following packages:

-   dplyr
-   ggplot2
-   ggrepel
-   Rtsne
-   text2vec

To install `memr`, simply type in an R console (after having installed the `devtools` package, e.g. `install.package('devtools')`):

``` r
devtools::install_git("https://github.com/adamgdobrakowski/memr")
```

Usage
-----

### Example datasets

We show the usage of the package on the example datasets. They are completely artificial, but their structure reflects a structure of the real data collected from Polish health centers. The results of the research on the real data are described in the paper Dobrakowski et al. (2019).

For every visit we can have some information about ICD-10 code of diagnosed disease, ID and specialty of the doctor:

``` r
knitr::kable(visits)
```

| visit\_id | icd10 | doctor\_id | specialties                      |
|:----------|:------|:-----------|:---------------------------------|
| 101       | J32   | 24         | endocrinologist, internist       |
| 102       | Y52   | 26         | endocrinologist                  |
| 103       | X12   | 24         | endocrinologist, internist       |
| 104       | Q29   | 24         | endocrinologist, internist       |
| 105       | U46   | 26         | endocrinologist                  |
| 106       | U50   | 26         | endocrinologist                  |
| 107       | I58   | 25         | cardiologist, internist          |
| 108       | C82   | 26         | endocrinologist                  |
| 109       | P73   | 24         | endocrinologist, internist       |
| 110       | P66   | 26         | endocrinologist                  |
| 111       | U53   | 23         | ophthalmologist, endocrinologist |

For the visits we have also the descriptions of interview with the extracted medical terms:

``` r
knitr::kable(interviews)
```

|     | x                 |
|-----|:------------------|
| 101 | fever, eye        |
| 102 | cough, thyroid    |
| 103 | fever, thyroid    |
| 104 | fever, eye        |
| 105 | cough, thyroid    |
| 106 | cough, thyroid    |
| 107 | cough, thyroid    |
| 108 | rhinitis, eye     |
| 109 | rhinitis, eye     |
| 110 | rhinitis, thyroid |

Descriptions of examinations of patients:

``` r
knitr::kable(examinations)
```

|     | x                                 |
|-----|:----------------------------------|
| 102 | mother, father, cough, eye        |
| 103 | woman, father, rhinitis, thyroid  |
| 104 | woman, father, fever, thyroid     |
| 105 | mother, patient, rhinitis, eye    |
| 106 | man, father, cough, heart         |
| 107 | woman, father, fever, eye         |
| 108 | woman, patient, rhinitis, thyroid |
| 109 | woman, patient, rhinitis, heart   |
| 110 | woman, father, cough, heart       |
| 111 | man, father, rhinitis, eye        |

And descriptions of recommendations prescribed by doctors to the patients:

``` r
knitr::kable(recommendations)
```

|     | x                                    |
|-----|:-------------------------------------|
| 103 | hospital, endocrinologist            |
| 104 | hospital, sleep, internist           |
| 105 | hospital, internist                  |
| 106 | sleep, ophthalmologist               |
| 107 | hospital, treatment, ophthalmologist |
| 108 | treatment, ophthalmologist           |
| 109 | treatment, hospital, endocrinologist |
| 110 | hospital, sleep, cardiologist        |

Each medical term has one or more categories:

``` r
knitr::kable(terms_categories)
```

| term            | category       |
|:----------------|:---------------|
| man             | person         |
| woman           | person         |
| mother          | person         |
| father          | person         |
| patient         | person         |
| cough           | disease        |
| rhinitis        | disease        |
| fever           | disease        |
| sleep           | recommendation |
| healthy eating  | recommendation |
| hospital        | recommendation |
| treatment       | recommendation |
| internist       | specialty      |
| cardiologist    | specialty      |
| ophthalmologist | specialty      |
| endocrinologist | specialty      |
| heart           | anatomic       |
| eye             | anatomic       |
| thyroid         | anatomic       |

### Medical terms embeddings

Firstly we can compute embeddings:

``` r
embedding_size <- 3

interview_term_vectors <- embed_terms(merged_terms = interviews, embedding_size = embedding_size,
                                       term_count_min = 1L)
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 1, expected cost 0.1679
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 2, expected cost 0.1195
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 3, expected cost 0.0929
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 4, expected cost 0.0753
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 5, expected cost 0.0627
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 6, expected cost 0.0532
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 7, expected cost 0.0458
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 8, expected cost 0.0398
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 9, expected cost 0.0350
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 10, expected cost 0.0309
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 11, expected cost 0.0275
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 12, expected cost 0.0246
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 13, expected cost 0.0221
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 14, expected cost 0.0199
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 15, expected cost 0.0179
examination_term_vectors <- embed_terms(merged_terms = examinations, embedding_size = embedding_size,
                                         term_count_min = 1L)
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 1, expected cost 0.0596
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 2, expected cost 0.0338
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 3, expected cost 0.0242
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 4, expected cost 0.0194
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 5, expected cost 0.0165
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 6, expected cost 0.0147
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 7, expected cost 0.0133
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 8, expected cost 0.0123
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 9, expected cost 0.0115
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 10, expected cost 0.0108
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 11, expected cost 0.0102
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 12, expected cost 0.0097
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 13, expected cost 0.0093
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 14, expected cost 0.0089
#> INFO [2020-02-08 11:32:48] 2020-02-08 11:32:48 - epoch 15, expected cost 0.0086

knitr::kable(interview_term_vectors[1:5, ])
```

|          |            |            |            |
|:---------|-----------:|-----------:|-----------:|
| rhinitis |  -0.4591842|   0.0773042|   0.3169519|
| fever    |  -0.3166999|   0.4445915|   0.2889484|
| eye      |   0.6172635|  -0.0026380|  -0.2859116|
| cough    |  -0.3053243|   0.1688631|   0.6479924|
| thyroid  |  -0.2251053|  -0.4781070|  -0.1793006|

Terms from the chosen category can be visualized:

``` r
visualize_term_embeddings(terms_categories, interview_term_vectors, c("anatomic"), method = "PCA")
```

![](man/figures/README-unnamed-chunk-9-1.png)

To validate the quality of embeddings we can perform the term analogy task (see more by ?analogy\_task). The package delivers the analogy test set.

``` r
knitr::kable(evaluate_term_embeddings(examination_term_vectors, n = 5, terms_pairs_test))
```

|         |    1|          2|          3|          4|          5|       MEAN|
|---------|----:|----------:|----------:|----------:|----------:|----------:|
| person  |    0|  0.5000000|  1.0000000|  1.0000000|  1.0000000|  0.7000000|
| spec    |    0|  0.0000000|  0.0000000|  0.0000000|  0.0000000|  0.0000000|
| synonym |    0|  0.0000000|  0.0000000|  0.0000000|  0.0000000|  0.0000000|
| MEAN    |    0|  0.1666667|  0.3333333|  0.3333333|  0.3333333|  0.2333333|

For each type of analogy we compute the mean accuracy.

Analogies can be plotted to see if the connection lines are parallel:

``` r
visualize_analogies(examination_term_vectors, terms_pairs_test$person, find_analogies = TRUE, n = 10)
```

![](man/figures/README-unnamed-chunk-11-1.png)

### Visits embeddings

Having the embeddings of terms, we can compute embeddings of visits:

``` r
visits_vectors <- embed_list_visits(interviews, examinations, interview_term_vectors, examination_term_vectors)
knitr::kable(visits_vectors[1:5, ])
```

|     |            |            |           |            |            |            |
|:----|-----------:|-----------:|----------:|-----------:|-----------:|-----------:|
| 101 |   0.1502818|   0.2209768|  0.0015184|          NA|          NA|          NA|
| 102 |  -0.2652148|  -0.1546220|  0.2343459|   0.0315736|  -0.1512805|  -0.0030038|
| 103 |  -0.2709026|  -0.0167577|  0.0548239|  -0.0211255|   0.0591052|   0.0587511|
| 104 |   0.1502818|   0.2209768|  0.0015184|   0.0436967|   0.0468291|  -0.0488440|
| 105 |  -0.2652148|  -0.1546220|  0.2343459|  -0.0186915|  -0.1348312|   0.0672795|

And now we can visualize the visits on the plot and color by the doctors' IDs:

``` r
visualize_visit_embeddings(visits_vectors, visits, color_by = "doctor",
                                spec = "internist")
```

![](man/figures/README-unnamed-chunk-13-1.png)

or by ICD-10 code:

``` r
visualize_visit_embeddings(visits_vectors, visits, color_by = "icd10",
                                spec = "internist")
```

![](man/figures/README-unnamed-chunk-14-1.png)

### Clustering

On the visits' embeddings we can run the k-means algorithm:

``` r
clusters <- cluster_visits(visits_vectors, visits, spec = "internist", cluster_number = 2)
```

and plot the clusters:

``` r
visualize_visit_embeddings(visits_vectors, visits, color_by = "cluster",
                                spec = "internist", clusters = clusters)
```

![](man/figures/README-unnamed-chunk-16-1.png)

For every cluster we can see the most frequent recommendations from chosen categories:

``` r
rec_tables <- get_cluster_recommendations(recommendations, clusters,
                                          category = "recommendation",
                                          recom_table = terms_categories)
knitr::kable(rec_tables)
```

<table class="kable_wrapper">
<tbody>
<tr>
<td>
| recommendation |  count|  frequency|
|:---------------|------:|----------:|
| hospital       |      2|  0.6666667|
| sleep          |      1|  0.3333333|
| treatment      |      1|  0.3333333|

</td>
<td>
| recommendation |  count|  frequency|
|:---------------|------:|----------:|
| hospital       |      2|        1.0|
| treatment      |      1|        0.5|

</td>
</tr>
</tbody>
</table>
or from all categories:

``` r
rec_tables <- get_cluster_recommendations(recommendations, clusters, category = "all")
rec_tables
#> [[1]]
#> # A tibble: 5 x 3
#>   recommendation  count frequency
#>   <fct>           <int>     <dbl>
#> 1 hospital            2     0.667
#> 2 endocrinologist     1     0.333
#> 3 internist           1     0.333
#> 4 sleep               1     0.333
#> 5 treatment           1     0.333
#> 
#> [[2]]
#> # A tibble: 4 x 3
#>   recommendation  count frequency
#>   <fct>           <int>     <dbl>
#> 1 hospital            2       1  
#> 2 endocrinologist     1       0.5
#> 3 ophthalmologist     1       0.5
#> 4 treatment           1       0.5
```

If we have a new visit, we can assign it to the most appropriate cluster:

``` r
inter_descr <- paste("cough", sep = ", ")
exam_descr <- paste("fever", sep = ", ")
visit_description <- c(inter_descr, exam_descr)
names(visit_description) <- c("inter", "exam")
cl <- assign_visit_to_cluster(visit_description, clusters, interview_term_vectors, examination_term_vectors)
cl
#> $cl
#> 2 
#> 2 
#> 
#> $distances
#>         1         2 
#> 0.1641233 0.1238790
```

As the last nice thing we can see the embeddings of ICD-10 codes:

``` r
visualize_icd10(visits_vectors, visits)
```

![](man/figures/README-unnamed-chunk-20-1.png)

Acknowledgements
================

The package was created during the research financially supported by the Polish Centre for Research and Development (Grant POIR.01.01.01-00-0328/17).

References
==========

Dobrakowski, Adam, Agnieszka Mykowiecka, Małgorzata Marciniak, Wojciech Jaworski, and Przemysław Biecek. 2019. “Interpretable Segmentation of Medical Free-Text Records Based on Word Embeddings.” *arXiv Preprint arXiv:1907.04152*.
