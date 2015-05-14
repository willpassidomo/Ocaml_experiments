all: minSpanningTree

heap: heap.ml
	corebuild -lib str heap.native

helpers: helpers.ml
	corebuild -lib str helpers.native

mysort: mysort.ml
	corebuild -lib str mysort.native

minSpanningTree:  mysort heap minSpanningTree.ml test helpers
	corebuild -lib str minSpanningTree.native

test: test.ml
	corebuild -lib str test.native

clean:
	rm -rf _build *.native
