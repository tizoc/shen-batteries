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
	maybe/cexpr \
	nullable \
	pipe-macro \
	seq \
	seq/dict \
	seq/cexpr \
	shendoc \
	typ/or \
	typ/sexp \
	typ/verified-and-head \
	typ/verified-if \
	typ/verified-objects \
	typ/void \
	with-exit

MODULE_CONFORMANCE_CASES = descriptors loading source-failure cycle feature

GENERATED_DOCS = \
	docs/box.adoc \
	docs/cexpr.adoc \
	docs/defpattern.adoc \
	docs/dict.adoc \
	docs/features.adoc \
	docs/iter.adoc \
	docs/lazy.adoc \
	docs/lazy/pattern.adoc \
	docs/let-match.adoc \
	docs/library.adoc \
	docs/maybe.adoc \
	docs/maybe/cexpr.adoc \
	docs/nullable.adoc \
	docs/pipe-macro.adoc \
	docs/seq.adoc \
	docs/seq/cexpr.adoc \
	docs/seq/dict.adoc \
	docs/shendoc.adoc \
	docs/typ/or.adoc \
	docs/typ/sexp.adoc \
	docs/typ/verified-and-head.adoc \
	docs/typ/verified-if.adoc \
	docs/typ/verified-objects.adoc \
	docs/typ/void.adoc \
	docs/with-exit.adoc

.PHONY: test test-modules test-authoring-modules test-module-conformance test-native docs check-docs
test: test-modules test-authoring-modules test-module-conformance
	"$(SHEN)" script tests/run.shen
	"$(SHEN)" script tests/run-typ-conformance.shen
	"$(SHEN)" script tests/run-verified-if.shen
	"$(SHEN)" script tests/run-portable-fallback.shen

test-modules:
	@set -e; \
	for Module in $(PRODUCTION_MODULES); do \
		"$(SHEN)" script tests/load-module.shen "$$Module"; \
	done

test-authoring-modules:
	"$(SHEN)" script tests/run-authoring-modules.shen

test-module-conformance:
	@set -e; \
	for Case in $(MODULE_CONFORMANCE_CASES); do \
		"$(SHEN)" script tests/run-shen-module-v1-conformance.shen "$$Case"; \
	done

test-native:
	mkdir -p _build
	"$(SHEN)" build-module-app tests/native/maybe-pattern.shenmod --module-dir . -o _build/native-test.so
	"$(SHEN)" eval -q \
		-e '(shen-scheme.load-compiled "_build/native-test.so")' \
		-e '(if (= (batteries-native-test.answer) 42) ok (error "native pattern smoke test failed"))' \
		-e '(if (= (batteries-native-test.iter-answer) [1 2]) ok (error "native iter smoke test failed"))' \
		-e '(if (= (batteries-native-test.maybe-do-answer) 42) ok (error "native maybe.do smoke test failed"))' \
		-e '(if (= (batteries-native-test.seq-do-answer) [11 21 12 22]) ok (error "native seq.do smoke test failed"))'

docs:
	mkdir -p docs/lazy docs/maybe docs/seq
	"$(SHEN)" script shendoc.shen box/box.shen docs/box.adoc
	"$(SHEN)" script shendoc.shen cexpr/cexpr.shen docs/cexpr.adoc
	"$(SHEN)" script shendoc.shen defpattern/defpattern.shen docs/defpattern.adoc
	"$(SHEN)" script shendoc.shen dict/dict.shen docs/dict.adoc
	"$(SHEN)" script shendoc.shen features/features.shen docs/features.adoc
	"$(SHEN)" script shendoc.shen iter/iter.shen docs/iter.adoc
	"$(SHEN)" script shendoc.shen lazy/lazy.shen docs/lazy.adoc
	"$(SHEN)" script shendoc.shen lazy/pattern/lazy-pattern.shen docs/lazy/pattern.adoc
	"$(SHEN)" script shendoc.shen let-match/let-match.shen docs/let-match.adoc
	"$(SHEN)" script shendoc.shen library.shen docs/library.adoc
	"$(SHEN)" script shendoc.shen maybe/maybe.shen docs/maybe.adoc
	"$(SHEN)" script shendoc.shen maybe/cexpr/maybe-cexpr.shen docs/maybe/cexpr.adoc
	"$(SHEN)" script shendoc.shen nullable/nullable.shen docs/nullable.adoc
	"$(SHEN)" script shendoc.shen pipe-macro/pipe-macro.shen docs/pipe-macro.adoc
	"$(SHEN)" script shendoc.shen seq/seq.shen docs/seq.adoc
	"$(SHEN)" script shendoc.shen seq/cexpr/seq-cexpr.shen docs/seq/cexpr.adoc
	"$(SHEN)" script shendoc.shen seq/dict/seq-dict.shen docs/seq/dict.adoc
	"$(SHEN)" script shendoc.shen shendoc/core.shen docs/shendoc.adoc
	"$(SHEN)" script shendoc.shen typ/or/or.shen docs/typ/or.adoc
	"$(SHEN)" script shendoc.shen typ/sexp/sexp.shen docs/typ/sexp.adoc
	"$(SHEN)" script shendoc.shen typ/verified-and-head/verified-and-head.shen docs/typ/verified-and-head.adoc
	"$(SHEN)" script shendoc.shen typ/verified-if/verified-if.shen docs/typ/verified-if.adoc
	"$(SHEN)" script shendoc.shen typ/verified-objects/verified-objects.shen docs/typ/verified-objects.adoc
	"$(SHEN)" script shendoc.shen typ/void/void.shen docs/typ/void.adoc
	"$(SHEN)" script shendoc.shen with-exit/with-exit.shen docs/with-exit.adoc

check-docs: docs
	git diff --exit-code -- $(GENERATED_DOCS)
