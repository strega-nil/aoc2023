OCAMLCC ?= ocamlopt

DAYS := $(patsubst src/%.ml,%,$(wildcard src/*.ml))
LIBS := $(patsubst lib/%.ml,out/%.cmx,$(wildcard lib/*.ml))

.PHONY: clean run-%.1 run-%.2
.SECONDARY:

all: $(DAYS)

run-day%.1: day% src/day%.txt
	./$< 1 src/day$*.txt
run-day%.2: day% src/day%.txt
	./$< 2 src/day$*.txt

day%: out/day%.cmx $(LIBS)
	$(OCAMLCC) $(LIBS) $< -o $@

out/%.cmx : src/%.ml $(LIBS) | out
	$(OCAMLCC) -c $< -o out/$*.cmx -I out

out/%.cmi : lib/%.mli | out
	$(OCAMLCC) -c $< -o out/$*.cmi -I out

out/%.cmx : lib/%.ml out/%.cmi | out
	$(OCAMLCC) -c $< -o out/$*.cmx -I out

out:
	mkdir -p out

clean:
	rm -rf out
	rm -f $(DAYS)
