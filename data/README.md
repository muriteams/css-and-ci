# Generated files

## Networks

*   `networks_truth` The tie `i->j` exists if `i` nominated `j`.

*   `networks_css` Contains the Cognitive Social Structure networks. For any given
    individual reporting on is views of a network, te tie `i->j` exists if
    such individual asserts its existance.
    
*   `networks_advice_las` This is the set of locally aggregated structure networks
    in which a tie between `(i,j)` exists iff both agree on its existance, i.e.
    we are using the intersection method.
    
    
## Accuracy

The accuracy files were created by the R script [accuracy.R]. Currently, the
following metrics were used to compute accuracy:

- `Hamman (S)`           = "shamann",
- `Hamming (D)`          = "dhamming",
- `Mean Manhattan (D)`   = "dmh",
- `Michael (S)`          = "smichael",
- `Sized Difference (D)` = "dsd"
  
These are available in the R package [similR](http://github.com/muriteams/similR).

## Required files:

*   `data-raw/Study1_Group sizes.csv`: Shows the number of individuals in each group

*   `data-raw/MURI_Survey_1.sav`: First survey with network nomination data.

*   `data-raw/MURI_Survey_3.sav`: Second survey with network nomination data.
    Includes CSS
    
    