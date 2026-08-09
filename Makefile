ifndef SHEN
$(error SHEN must be specified)
endif

.PHONY: test
test:
	"$(SHEN)" script tests/run.shen
	"$(SHEN)" script tests/run-verified-if.shen
