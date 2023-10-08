graphics = queries.dot.png

all: $(graphics)

%.dot.png: %.dot
	for i in $^; do dot -Tpng "$${i}" > "$${i}.png"; done
