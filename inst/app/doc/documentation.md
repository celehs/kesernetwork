## KESER network

### Objective of the KESER Network

The increasing availability of Electronic Health Record (EHR) systems has created enormous potential for translational research. However, it is difficult to know all the relevant codes related to a phenotype due to the large number of codes available. Codified concepts include all ICD diagnosis codes (rolled up to PheCodes), medications (mapped to RxNorm[[1]] at ingredient level), procedures (rolled up to procedure categories), as well as laboratory test results (mapped LOINC, short names at VA, or local lab codes). Traditional data mining approaches often require the use of patient-level data, which hinders the ability to share data across institutions. In this project, we demonstrate that summary data on the co-occurrence patterns of EHR codes from multiple institutions can be used to create large scale code embeddings that encodes the meaning of codified concepts. Using co-occurrence matrices of EHR codes from Veterans Affairs (VA) and Massachusetts General Brigham (MGB), the knowledge extraction via sparse embedding regression (KESER) algorithm [[2]][[3]] was used to construct knowledge networks for the code concepts. The KESER algorithm enables researchers to automatically select important codified concepts relevant to a disease or a medication of interest to assist downstream analysis. The KESER network was constructed based on either the co-occurrence matrix of the single institution or integrative analyses of the co-occurrence matrices from both institutions. 

The current version of the KESER network only allows PheCodes or RxNorm codes as target codes although the App allows one to search for any node and visualize all target codes connected with a lab or procedure code of interest. The current network does not show relatedness among Labs, among Procedures, or between Labs and Procedures. 
 


### Using the app

This tool allows to infer relatedness among diseases, treatment, procedures and laboratory measurements. By performing KESER across all PheCode and RxNorm, we create a knowledge map to help identify and visualize :

- the node-wise relationship between a **target code** (currently only PheCode or RxNorm can be a target node) and its **neighborhood codes** (PheCode, RxNorm, Procedure codes and Labs)

- all potential target PheCodes and RxNorm codes connected with non-target nodes (e.g. Procedures or Lab codes) of interest

For PheCodes, LOINC codes and Procedures, see more information at 

- **PheCode Hierarcy:** 
  * ICD9: https://phewascatalog.org/phecodes
  * ICD10-CM: https://phewascatalog.org/phecodes_icd10cm
  <br>

- **RxNorm Hierarchy:** https://mor.nlm.nih.gov/RxNav/
  * RxNorm disclaimer: This product uses publicly available data courtesy of the U.S. National Library of Medicine (NLM), National Institutes of Health, Department of Health and Human Services; NLM is not responsible for the product and does not endorse or recommend this or any other product.
  <br>

- **LOINC Hierarchy:** https://loinc.org/multiaxial-hierarchy/ 
  * LOINC disclaimer: This material contains content from LOINC (http://loinc.org). LOINC is copyright © 1995-2023, Regenstrief Institute, Inc. and the Logical Observation Identifiers Names and Codes (LOINC) Committee and is available at no cost under the license at http://loinc.org/license. LOINC® is a registered United States trademark of Regenstrief Institute, Inc.
  <br>

- **Procedure codes:** A unique ID is assigned to each of the procedure category.

The maximum number of target nodes (as input) is set to 50. For clarity of the network, it is recommended to use less than 10 target nodes.


### References

<hr>


[1]: https://pubmed.ncbi.nlm.nih.gov/21515544/

[2]: https://doi.org/10.1038/s41746-021-00519-z

[3]: https://celehs.github.io/KESER/

<a id="1">[1]</a>
Nelson SJ, Zeng K, Kilbourne J, Powell T, Moore R. 
Normalized names for clinical drugs: RxNorm at 6 years. 
J Am Med Inform Assoc. 2011 Jul-Aug;18(4)441-8. 
<https://doi.org/10.1136/amiajnl-2011-000116> Epub 2011 Apr 21. 
PubMed PMID: 21515544; PubMed Central PMCID: PMC3128404.


<a id="2">[2]</a> 
Hong, C., Rush, E., Liu, M. et al.
Clinical knowledge extraction via sparse embedding regression (KESER) with multi-center large scale electronic health record data. 
npj Digit. Med. 4, 151 (2021).
<https://doi.org/10.1038/s41746-021-00519-z>

<a id="3">[3]</a> 
https://celehs.github.io/KESER/


