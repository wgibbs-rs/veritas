.PHONY: all clean

all:
	@echo "Compiling all dependencies..."
	@echo "Compiling Maps.v..."
	@rocq compile -Q Foundation Foundation -noglob Foundation/Maps.v
	@rm Foundation/*.vok Foundation/*.vos Foundation/.*.aux
clean:
	rm -f Foundation/*.vo Foundation/*.glob Foundation/*.vos Foundation/*.vok