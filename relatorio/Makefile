PROJECT = main

LATEXMK = latexmk
FLAGS = -pdf -interaction=nonstopmode -use-make

all: $(PROJECT).pdf

$(PROJECT).pdf: $(PROJECT).tex
	$(LATEXMK) $(FLAGS) $(PROJECT).tex

clean:
	$(LATEXMK) -c

distclean:
	$(LATEXMK) -C

view: $(PROJECT).pdf
	@if [ "$$(uname)" = "Darwin" ]; then \
		open $(PROJECT).pdf; \
	elif [ "$$(expr substr $$(uname -s) 1 5)" = "Linux" ]; then \
		xdg-open $(PROJECT).pdf; \
	else \
		echo "Abra o arquivo $(PROJECT).pdf manualmente."; \
	fi

.PHONY: all clean distclean view