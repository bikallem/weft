# Mutation testing with windtrap (design section 2.17).
#
#   make mutate         Test the mutants on the lines that changed since BASE.
#   make mutate-all     Test all the mutants of the libraries.
#   make mutate-linux   Test the mutants that the Linux suite reaches, on the
#                       lines that changed since BASE.
#   make mutate-report  Merge the verdicts of the last run of mutate or
#                       mutate-all, and list the survivors on the lines that
#                       changed since BASE. Give BASE= for all the lines.
#   make mutate-arm MUTANT=src/core/chan.ml:107:12:skip
#                       Run SUITE with one mutant armed.
#
# The variables:
#
#   BASE       The git revision of the changed lines. The default is the
#              merge base with origin/main. On main, only the changes that are
#              not committed count.
#   JOBS       The number of mutants that each suite tests at the same time
#              (default auto: one for each CPU). Use 1 if a result of the
#              Linux suite looks incorrect. Two children that use the same
#              file or port can kill a mutant falsely.
#   MIN_SCORE  If it is set, the report gives exit status 1 only when the
#              score is less than MIN_SCORE, and not for each survivor.
#   SUITE      The suite of mutate-arm (default test/sim/test_sim.exe).
#
# Outside the dev shell, each command runs in `nix develop`.

BASE ?= $(shell git merge-base origin/main HEAD)
JOBS ?= auto
MIN_SCORE ?=
SUITE ?= test/sim/test_sim.exe
MUTANT ?=

DUNE := $(if $(IN_NIX_SHELL),dune,nix develop -c dune)
MUTATE := --instrument-with ppx_windtrap.mutate

# The suites that can fork a child for each mutant. test/linux_md runs only on
# many domains, and OCaml 5 refuses a fork after a domain starts. test/linux
# has its own target, because some of its tests start a domain.
SUITES := test/core test/sim

# The tests of test/linux that start a domain.
LINUX_EXCLUDE := -e "linux on 4 domains" -e "a waiter at Signal.unwatch raises"

# The report of `windtrap mutants`. $(1) is the revision of the changed lines,
# or empty for all the lines.
report = $(DUNE) exec $(MUTATE) windtrap -- mutants \
  $(if $(1),--since $(1)) $(if $(MIN_SCORE),--min-score $(MIN_SCORE))

.PHONY: help mutate mutate-all mutate-linux mutate-report mutate-arm

help:
	@sed -n '1,/^$$/p' Makefile | sed 's/^# \{0,1\}//'

# The build comes first, so that a compile error stops the target. A suite
# with a survivor gives exit status 1, so the run ignores the status, and the
# report gives the result.
mutate:
	$(DUNE) build $(MUTATE) $(SUITES:%=@%/all)
	-WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_SINCE=$(BASE) WINDTRAP_MUTATE_JOBS=$(JOBS) \
	  $(DUNE) runtest --force $(MUTATE) $(SUITES)
	$(call report,$(BASE))

mutate-all:
	$(DUNE) build $(MUTATE) $(SUITES:%=@%/all)
	-WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_JOBS=$(JOBS) \
	  $(DUNE) runtest --force $(MUTATE) $(SUITES)
	$(call report,)

# Windtrap does not save the verdicts of a run with -e, so this target gives
# its own report, and mutate-report does not include it. Windtrap also gives
# exit status 0 for a survivor in such a run. Each mutant that only
# test/linux/few_fds.exe tests survives here: few_fds is a program that a dune
# rule runs, not a suite, so it reaches no mutant.
mutate-linux:
	$(DUNE) exec $(MUTATE) test/linux/test_linux.exe -- \
	  --mutate --mutate-since=$(BASE) --mutate-jobs $(JOBS) $(LINUX_EXCLUDE)

mutate-report:
	$(call report,$(BASE))

mutate-arm:
	@test -n "$(MUTANT)" || { echo "Give MUTANT, for example MUTANT=src/core/chan.ml:107:12:skip"; exit 2; }
	$(DUNE) exec $(MUTATE) $(SUITE) -- --arm $(MUTANT)
