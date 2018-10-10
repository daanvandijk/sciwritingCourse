all:
	Rscript plots.R
	Rscript multivariateplots.R

clean:
	rm .Rhistory || true
	rm .RData || true
	rm *.png || true
	rm mcd.tex || true
