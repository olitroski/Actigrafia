cd "D:\OneDrive\INTA\Actigrafia\varios"
use analisis_filtro2, replace

// filter meanS sdS pctS meanW sdW pctW periodo nas

* Dejar noche en 0
list in 1/10
gen per = 0 if periodo == "Noche"
replace per = 1 if periodo == "Dia"
label define per 1 "Dia" 0 "Noche", replace
label val per per
list in 1/10

* Regresión
logit filter sdS pctS meanW sdW i.per
lsens
estat class, cut(0.64)  // 98.5

* Predicciones
predict prob, p
list in 1/10

sort filter -prob
list filter sdS pctS meanW sdW per prob in 1/10

// Ecuacion
logit
* -28.53082 - 0.1097845*sdS + 39.62804*pctS - 0.0141863*meanW + 0.0164165*sdW - 4.834989*per
gen probMine =  -33.36581 - 0.1097845*sdS + 39.62804*pctS - 0.0141863*meanW + 0.0164165*sdW + 4.834989*per
list in 1/5
replace probMine = exp(probMine)
list in 1/5

// Igual, funciona bien
* Tonces si es el valor mayor a 0.64 se clasifica como 1 si no 0




