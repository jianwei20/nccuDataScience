hw2
--
Rscript hw2_yourID.R --target male/female --files meth1 meth2 … methx --out result.csv
* Read in multiple files
* Positive case defined by “--target option”
* Find out which method contains the max
Inputs : method1.csv, where the last column pred.score is the predicted probability of "Male".

persons,prediction,reference,pred.score

person1,male,male,0.807018548483029

person2,male,male,0.740809247596189

person3,female,male,0.0944965328089893

person4,female,female,0.148418645840138

Output : result.csv

method,sensitivity,specificity,F1,AUC

method1,0.91,0.96,0.85,0.79

method2,0.99,0.98,0.86,0.70

highest,method2,method2,method2,method1

