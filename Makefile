LISP_CACHE ?= $(HOME)/.cache/common-lisp
SBCL_BIN=sbcl
SBCL=$(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
REPO_ROOT=$(abspath $(CURDIR))
LOCAL_SOURCE_REGISTRY=$(REPO_ROOT)//
QUICKLISP=env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
	$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp
DEPENDENCY_AUDIT_OUTPUT ?= dependency-audit.png
DEPENDENCY_AUDIT_ALL_OUTPUT ?= dependency-audit-all.png
DEPENDENCY_AUDIT_SYSTEMS ?= coalton
TEST_FORMS=--load $(QUICKLISP_SETUP) \
	--eval "(ql:quickload :coalton/tests)" \
	--eval "(asdf:test-system :coalton/tests)" \
	--eval "(asdf:load-system :small-coalton-programs)"

.PHONY: test test-release test-safe
test:
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		$(SBCL) \
		$(TEST_FORMS)

test-safe:
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		$(SBCL) \
		--eval "(sb-ext:restrict-compiler-policy 'safety 3)" \
		$(TEST_FORMS)

# Run all tests in release mode

test-release:
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		COALTON_ENV=release \
		$(SBCL) \
		$(TEST_FORMS)

.PHONY: docs
docs:
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		$(SBCL) \
		--load $(QUICKLISP_SETUP) \
		--eval "(ql:quickload :coalton/doc :silent t)" \
		--eval "(coalton/doc:write-stdlib-documentation-to-file \"docs/reference.md\")"

.PHONY: web-docs
web-docs:
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		$(SBCL) \
		--load $(QUICKLISP_SETUP) \
		--eval "(ql:quickload :coalton/doc :silent t)" \
		--eval "(coalton/doc:write-stdlib-documentation-to-file \"../coalton-website/content/reference.md\" :backend :hugo :revision \"main\")"

.PHONY: dependency-audit
dependency-audit:
	@set -e; \
		tmp_dot="$$(mktemp -t coalton-dependency-audit.XXXXXX.dot)"; \
		trap 'rm -f "$$tmp_dot"' EXIT INT TERM; \
		env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
			QUICKLISP_HOME="$(QUICKLISP_HOME)" \
			XDG_CACHE_HOME="/tmp/coalton-dependency-audit-cache" \
			scripts/dependency-audit.lisp \
			$(DEPENDENCY_AUDIT_SYSTEMS) > "$$tmp_dot"; \
		dot -Tpng "$$tmp_dot" -o "$(abspath $(DEPENDENCY_AUDIT_OUTPUT))"; \
		echo "wrote $(abspath $(DEPENDENCY_AUDIT_OUTPUT))"

.PHONY: dependency-audit-all
dependency-audit-all:
	@set -e; \
		tmp_dot="$$(mktemp -t coalton-dependency-audit-all.XXXXXX.dot)"; \
		trap 'rm -f "$$tmp_dot"' EXIT INT TERM; \
		env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
			QUICKLISP_HOME="$(QUICKLISP_HOME)" \
			XDG_CACHE_HOME="/tmp/coalton-dependency-audit-cache" \
			scripts/dependency-audit.lisp \
			--all-local > "$$tmp_dot"; \
		dot -Tpng "$$tmp_dot" -o "$(abspath $(DEPENDENCY_AUDIT_ALL_OUTPUT))"; \
		echo "wrote $(abspath $(DEPENDENCY_AUDIT_ALL_OUTPUT))"


.PHONY: parser-coverage
parser-coverage:
	mkdir coverage-report || true
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		$(SBCL) \
		--load $(QUICKLISP_SETUP) \
		--eval "(require :sb-cover)" \
		--eval "(declaim (optimize sb-cover:store-coverage-data))" \
		--eval "(asdf:test-system :coalton :force '(:coalton :coalton/tests))" \
		--eval "(sb-cover:report \
	              \"coverage-report/\" \
	              :if-matches (lambda (pathname) \
                                (string= (directory-namestring pathname) \
                                         (directory-namestring (merge-pathnames \"src/parser/\" (asdf:system-source-directory \"coalton\"))))))"
	open coverage-report/cover-index.html


###############################################################################
# MINE -- TUI IDE
###############################################################################
.PHONY: mine
mine:
	$(MAKE) -C mine mine

###############################################################################
# BENCHMARK (GOAL-025 step 1 / kreisler PLAN-308)
#
# Per-phase correctness + cost baseline. Run inside `nix develop`, which
# puts the coalton dependencies on CL_SOURCE_REGISTRY; benchmark/run.lisp
# registers the coalton-benchmark system itself.
###############################################################################
BASELINE ?= benchmark/baselines/baseline.sexp

.PHONY: bench bench-diff

# Run the corpus, print the per-phase report, and (re)write the baseline.
bench:
	$(SBCL_BIN) --script benchmark/run.lisp $(BASELINE)

# Run the corpus and compare correctness against the pinned baseline.
# Exits non-zero if any phase output changed.
bench-diff:
	$(SBCL) \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "benchmark/coalton-benchmark.asd"))' \
		--eval '(asdf:load-system :coalton-benchmark)' \
		--eval '(uiop:quit (if (coalton-benchmark:diff-baseline (truename "$(BASELINE)")) 0 1))'

###############################################################################
# CLEAN
###############################################################################
.PHONY: clean-quicklisp clean-cache cleanall

clean-quicklisp:
	@echo "Cleaning up old projects in Quicklisp"
	$(QUICKLISP) \
             --eval '(ql-dist:clean (ql-dist:dist "quicklisp"))'

clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	rm -rf "$(LISP_CACHE)"

cleanall: clean-cache clean-quicklisp
	@echo "All cleaned and reindexed."
