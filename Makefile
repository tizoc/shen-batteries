ifndef SHEN
$(error SHEN must be specified)
endif

.PHONY: test docs
test:
	"$(SHEN)" script tests/run.shen
	"$(SHEN)" script tests/run-verified-if.shen

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
