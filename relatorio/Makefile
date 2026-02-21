# Nome base do arquivo
MAIN = main

# Compilador LaTeX
LATEX = pdflatex

# Conversor PDF -> TXT
PDF2TXT = pdftotext

# Regra padrão
all: $(MAIN).txt

# Gerar PDF a partir do TEX
$(MAIN).pdf: $(MAIN).tex
	$(LATEX) $(MAIN).tex
	$(LATEX) $(MAIN).tex

# Gerar TXT a partir do PDF
$(MAIN).txt: $(MAIN).pdf
	$(PDF2TXT) $(MAIN).pdf $(MAIN).txt

# Limpeza de arquivos auxiliares
clean:
	rm -f *.aux *.log *.out *.toc *.pdf

# Limpeza total (inclusive txt)
distclean: clean
	rm -f $(MAIN).txt
