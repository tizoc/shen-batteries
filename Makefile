ifndef SHEN
$(error SHEN must be specified)
endif

.PHONY: test
test:
	"$(SHEN)" script tests/run.shen
	"$(SHEN)" eval -l tests/harness.shen -l typ/verified-objects/verified-objects.shen -l typ/verified-if/verified-if.shen -l tests/verified-if.shen -e "(test.finish)"
