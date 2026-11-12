.PHONY: all clean

all:
	@echo "Compiling all dependencies..."
	@echo "Compiling Maps.v..."
	@rocq compile -Q . Model -noglob Maps.v
	@rocq compile -Q . Model -noglob CoreSyntax.v
	@rocq compile -Q . Model -noglob Hoare.v
	@rm *.vok *.vos .*.aux
clean:
	rm -f *.vo *.glob *.vos *.vok