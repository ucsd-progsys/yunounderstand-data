.PHONY: all progs progs-fa15 progs-sp14 progs-comb pairs pairs-fa15 pairs-sp14

all: progs pairs

progs: progs-fa15 progs-sp14 progs-comb

progs-fa15:
	@python3 scripts/extract_programs.py data/derived/fa15/progs data/raw/fa15/*.json
progs-sp14:
	@python3 scripts/extract_programs.py data/derived/sp14/progs data/raw/sp14/*.json
progs-comb:
	@python3 scripts/extract_programs.py data/derived/comb/progs data/raw/sp14/*.json data/raw/fa15/*.json

pairs: pairs-fa15 pairs-sp14

pairs-fa15:
	@python3 scripts/extract_pairs.py -s data/raw/fa15 -o data/derived/fa15
pairs-sp14:
	@python3 scripts/extract_pairs.py -s data/raw/sp14 -o data/derived/sp14
