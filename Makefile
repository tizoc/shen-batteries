ifndef SHEN
$(error SHEN must be specified)
endif

PRODUCTION_MODULES = \
	box \
	cexpr \
	defpattern \
	dict \
	features \
	iter \
	lazy \
	lazy/pattern \
	let-match \
	maybe \
	nullable \
	pipe-macro \
	seq \
	seq/cexpr \
	shendoc \
	typ/or \
	typ/sexp \
	typ/verified-and-head \
	typ/verified-if \
	typ/verified-objects \
	typ/void \
	with-exit

.PHONY: test test-modules test-native docs
test: test-modules
	"$(SHEN)" script tests/run.shen
	"$(SHEN)" script tests/run-verified-if.shen

test-modules:
	@set -e; \
	for Module in $(PRODUCTION_MODULES); do \
		"$(SHEN)" script tests/load-module.shen "$$Module"; \
	done

test-native:
	mkdir -p _build
	"$(SHEN)" build-module-app tests/native/maybe-pattern.shenmod --module-dir . -o _build/native-test.so
	"$(SHEN)" eval -q \
		-e '(shen-scheme.load-compiled "_build/native-test.so")' \
		-e '(if (= (batteries-native-test.answer) 42) ok (error "native module smoke test failed"))'

docs:
	"$(SHEN)" script shendoc.shen box/box.shen docs/box.adoc
	"$(SHEN)" script shendoc.shen cexpr/cexpr.shen docs/cexpr.adoc
	"$(SHEN)" script shendoc.shen dict/dict.shen docs/dict.adoc
	"$(SHEN)" script shendoc.shen iter/iter.shen docs/iter.adoc
	"$(SHEN)" script shendoc.shen seq/seq.shen docs/seq.adoc
	"$(SHEN)" script shendoc.shen shendoc/core.shen docs/shendoc.adoc
	"$(SHEN)" script shendoc.shen typ/or/or.shen docs/typ/or.adoc
	"$(SHEN)" script shendoc.shen typ/sexp/sexp.shen docs/typ/sexp.adoc
	"$(SHEN)" script shendoc.shen typ/verified-and-head/verified-and-head.shen docs/typ/verified-and-head.adoc
	"$(SHEN)" script shendoc.shen typ/verified-if/verified-if.shen docs/typ/verified-if.adoc
	"$(SHEN)" script shendoc.shen typ/verified-objects/verified-objects.shen docs/typ/verified-objects.adoc
	"$(SHEN)" script shendoc.shen typ/void/void.shen docs/typ/void.adoc
