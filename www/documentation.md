## KESER network

### Objective of the KESER Network

EHR can be largely available, and presents a huge potential for translational research. However, due to the large number of codes present in EHR, a main challenge is to identify the subset of codes relevant to a phenotype or medication of interest. The codified concepts include diagnosis codes (rolled up to PheCodes), medications (mapped to RxNorm at ingredient level), procedures (rolled up to CCS procedure codes), as well as laboratory test results (mapped LOINC, short names at VA, or local lab codes).

Knowledge extraction via Sparse Embedding Regression (KESER) is an embedding-based method that allows effective feature selection and knowledge discovery with EHR data. The method can be performed on summary data that does not include patient-level information and can be shared across research groups and institutions.

KESER was trained using code co-occurrence information obtained from two large healthcare systems: the Veterans Affairs (VA) and Massachusetts General Brigham (MGB). By integrating data from both sites, KESER is able to achieve higher accuracy in reflecting clinical knowledge. 

As a result, KESER can successfully select information and clinically meaningful codified features that can be used for phenotyping, causal modeling, and other downstream analyses. The current version of the KESER network only allows PheCodes or RxNorm codes as target codes. 


### Using the app

This tool allows to infer relatedness among diseases, treatment, procedures and laboratory measurements. By performing KESER across all PheCode and RxNorm, we create a knowledge map to help identify and visualize :
the node-wise relationship between a target code (currently only PheCode or RxNorm can be a target node) and its neighborhood codes (PheCode, RxNorm, CCS and Labs)
all potential target PheCodes and RxNorm codes connected with non-target nodes (e.g. CCS or Lab codes)

The maximum number of target nodes (as input) is set to 50. For clarity of the network, it is recommended to use less than 10 target nodes.


### Data Information

Clinical Classifications Software (CCS): https://hcup-us.ahrq.gov/toolssoftware/ccs_svcsproc/ccssvcproc.jsp

LOINC Multiaxial Hierarchy: https://loinc.org/multiaxial-hierarchy/

RxNorm: https://mor.nlm.nih.gov/RxNav/

PheCode Map 1.2 with ICD-10cm Codes (beta): https://phewascatalog.org/phecodes_icd10cm

PheCode Map 1.2 with ICD-9 Codes: https://phewascatalog.org/phecodes

### References

<hr>

[1]: https://doi.org/10.1101/2021.03.13.21253486

[2]: https://celehs.github.io/KESER/

[1] Chuan Hong, Everett Rush, Molei Liu, Doudou Zhou, Jiehuan Sun, Aaron Sonabend, et al. Clinical Knowledge Extraction via Sparse Embedding Regression (KESER) with Multi-Center Large Scale Electronic Health Record Data. medRxiv 2021.03.13.21253486; doi: https://doi.org/10.1101/2021.03.13.21253486

[2] https://celehs.github.io/KESER/

