# Mutation testing with windtrap (design section 2.17).
#
#   make mutate         Test the mutants on the lines that changed since BASE.
#   make mutate-all     Test all the mutants of the libraries.
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
#              Linux suites looks incorrect. Two children that use the same
#              file or port can kill a mutant falsely. A budget above the
#              number of CPUs makes each run slower: windtrap multiplies the
#              deadline of each child by the ratio, and a mutant that hangs
#              waits for its deadline.
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

# The suites of all the libraries.
SUITES := test/core test/sim

# The suites of the Linux world. They test only the mutants of weft.linux and
# weft.posix (LINUX_LIBS), because test/core and test/sim kill each mutant of
# the core and the simulator that these suites kill. A run of test/linux_md on
# src/core/sched.ml took 297 s and gave no other kill. These suites start
# domains, so windtrap starts each child as a new process (exec mode). They
# run after SUITES, one at a time, because their tests use ports and files.
# Each mutant that only test/linux/few_fds.exe tests survives: few_fds is a
# program that a dune rule runs, not a suite, so it reaches no mutant.
LINUX_SUITES := $(if $(filter Linux,$(shell uname -s)),test/linux test/linux_md)
LINUX_LIBS := src/linux,src/posix

# The report of `windtrap mutants`. $(1) is the revision of the changed lines,
# or empty for all the lines.
report = $(DUNE) exec $(MUTATE) windtrap -- mutants \
  $(if $(1),--since $(1)) $(if $(MIN_SCORE),--min-score $(MIN_SCORE))

.PHONY: help check-windtrap mutate mutate-all mutate-report mutate-arm

help:
	@sed -n '1,/^$$/p' Makefile | sed 's/^# \{0,1\}//'

# A dev shell that started before a change of flake.nix keeps its old
# windtrap. windtrap 0.2.0 tests one mutant at a time, has fewer types of
# mutants, and writes verdict files that the fork cannot read. Its `mutants`
# command has no --since.
check-windtrap:
	@$(DUNE) exec $(MUTATE) windtrap -- mutants --help 2>/dev/null | grep -q -- --since || { \
	  echo "The windtrap of this shell is not the fork of flake.nix."; \
	  echo "Start nix develop again, or run direnv reload."; exit 2; }

# The run of the suites. $(1) is the revision of the changed lines, or empty
# for all the lines. A suite with a survivor gives exit status 1, so the run
# ignores the status, and the report gives the result.
define run_suites
-WINDTRAP_MUTATE=1 $(if $(1),WINDTRAP_MUTATE_SINCE=$(1)) WINDTRAP_MUTATE_JOBS=$(JOBS) \
  $(DUNE) runtest --force $(MUTATE) $(SUITES)
-for s in $(LINUX_SUITES); do \
  WINDTRAP_MUTATE=$(LINUX_LIBS) $(if $(1),WINDTRAP_MUTATE_SINCE=$(1)) WINDTRAP_MUTATE_JOBS=$(JOBS) \
    $(DUNE) runtest --force $(MUTATE) $$s; \
done
endef

# The build comes first, so that a compile error stops the target. The run
# removes the old verdict files: it runs each suite again, and a file of an
# older windtrap stops the merge. The report gives a failure only for a
# survivor: a mutant that no test reaches, and a line of weft.kqueue, give no
# failure. The CI job `mutation` runs this target on each pull request.
mutate: check-windtrap
	$(DUNE) build $(MUTATE) $(SUITES:%=@%/all) $(LINUX_SUITES:%=@%/all)
	rm -rf _build/_mutants
	$(call run_suites,$(BASE))
	$(call report,$(BASE))

mutate-all: check-windtrap
	$(DUNE) build $(MUTATE) $(SUITES:%=@%/all) $(LINUX_SUITES:%=@%/all)
	rm -rf _build/_mutants
	$(call run_suites,)
	$(call report,)

mutate-report: check-windtrap
	$(call report,$(BASE))

mutate-arm:
	@test -n "$(MUTANT)" || { echo "Give MUTANT, for example MUTANT=src/core/chan.ml:107:12:skip"; exit 2; }
	$(DUNE) exec $(MUTATE) $(SUITE) -- --arm $(MUTANT)
