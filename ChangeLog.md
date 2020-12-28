# Revision history for uniqueness-periods-vector-common

## 0.1.0.0 -- 2020-08-30

* First version. Released on an unsuspecting world.

## 0.2.0.0 -- 2020-09-09

* Second version. Fixed issue with the wrong arguments order in the Languages.UniquenessPeriods.Vector.StrictV.uniquenessVariants2GN function.

## 0.3.0.0 -- 2020-09-12

* Third version. Changed the Languages.UniquenessPeriods.Vector.Data module so that the uniqueness-periods-vector-general package can be fixed and rewritten to
follow the requirements. These changes break the previous behaviour, so check the code.

## 0.4.0.0 -- 2020-09-28

* Fourth version. Simplified the uniquenessVariants2GNP function so that it does not use backpermute function from Data.Vector and permutations from Data.List simultanously.

## 0.4.1.0 -- 2020-10-06

* Fourth version revised A. Fixed issue with being concatenated lists without whitespaces in the Languages.UniquenessPeriods.Vector.StrictV.uniquenessVariants2GNP function.

## 0.5.0.0 -- 2020-10-09

* Fifth version. Added a new data type FuncRep a b c to Languages.UniquenessPeriods.Vector.Data module to avoid significant code duplication with further usage. Switched the 
needed functionality to the new data type variant. 

## 0.5.1.0 -- 2020-10-12

* Fifth version revised A. Fixed issue with unnecessary application of the Data.List.intercalate function in the Languages.UniquenessPeriods.Vector.StrictV module that led to 
inconsistency and (some) divergence in the functions with applied uniquenessVariants2GNP. 

## 0.5.1.1 -- 2020-10-14


* Fifth version revised B. Some minor code improvements.

