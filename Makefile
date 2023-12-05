OCAMLCC ?= ocamlopt

DAYS := $(patsubst src/bin/%.ml,bin/%,$(wildcard src/bin/*.ml))
LIBS := $(patsubst src/lib/%.ml,out/%.cmx,$(wildcard src/lib/*.ml))

.PHONY: clean run-%.1 run-%.2 new-day%
.SECONDARY:

all: $(DAYS)

run-day%.1: bin/day% data/day%.txt
	./$< 1 data/day$*.txt
run-day%.2: bin/day% data/day%.txt
	./$< 2 data/day$*.txt

test-day%.1: bin/day% data/test/day%.txt
	$< 1 data/test/day$*.txt
test-day%.2: bin/day% data/test/day%.txt
	$< 2 data/test/day$*.txt

new-day%: template.ml
	cp template.ml src/bin/day$*.ml

bin/day%: out/day%.cmx $(LIBS) | bin
	$(OCAMLCC) $(LIBS) $< -o $@

out/%.cmx : src/bin/%.ml $(LIBS) | out
	$(OCAMLCC) -c $< -o out/$*.cmx -I out

out/%.cmi : src/lib/%.mli | out
	$(OCAMLCC) -c $< -o out/$*.cmi -I out

out/%.cmx : src/lib/%.ml out/%.cmi | out
	$(OCAMLCC) -c $< -o out/$*.cmx -I out

out bin:
	mkdir -p $@

clean:
	rm -rf out bin
