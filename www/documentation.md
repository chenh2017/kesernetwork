## KESER network

### Objective of the KESER Network

EHR can be largely available, and presents a huge potential for translational research. However, due to the large number of codes present in EHR, a main challenge is to know all the codes relevant to a phenotype of interest. The codified concepts include diagnosis codes (PheCode), medications(RxNorm), procedures (CCS), as well as laboratory test results (LOINC).

Knowledge extraction via Sparse Embedding Regression^[1] (KESER) is an embedding-based method that allows effective feature selection and knowledge discovery with EHR data. The method can be performed on summary data that does not include patient-level information and can be shared across research groups and institutions.

KESER^[2] was performed at two sites: the Veterans Affairs (VA) and Massachusetts General Brigham (MGB). By integrating data from both sites, KESER is able to achieve higher accuracy in reflecting clinical knowledge. 

As a result, KESER can successfully select information and clinically meaningful features that can be used for phenotyping and other downstream analyses.


### Using the app

This tool allows to infer relatedness among diseases, treatment, procedures and laboratory measurements. By performing KESER across all PheCode and RxNorm, we create a knowledge map to help identify and visualize :
the node-wise relationship between a target code (PheCode or RxNorm) and its neighborhood codes (PheCode, RxNorm, CCS and Labs)
disease-disease pairs
disease-drug pairs.

The maximum number of input nodes is set to 50. For clarity of the network, it is recommended to use less than 10 nodes.
Note that the distance between nodes is proportional to the absolute coefficients obtained from the embedding regression.


### References

<hr>

[1]: https://doi.org/10.1101/2021.03.13.21253486

[2]: https://celehs.github.io/KESER/

[1] Chuan Hong, Everett Rush, Molei Liu, Doudou Zhou, Jiehuan Sun, Aaron Sonabend, et al. Clinical Knowledge Extraction via Sparse Embedding Regression (KESER) with Multi-Center Large Scale Electronic Health Record Data. medRxiv 2021.03.13.21253486; doi: https://doi.org/10.1101/2021.03.13.21253486

[2] https://celehs.github.io/KESER/

