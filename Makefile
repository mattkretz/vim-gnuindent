check: check-vim check-nvim

DEPS := plugin/gnuindent.vim test.cpp

check-%: %-result.cpp
	@diff -u test.cpp $< && echo Passed.

vim-result.cpp: $(DEPS)
	vim --clean -c 'set noet sw=2 ts=8 tw=80' -c 'source plugin/gnuindent.vim' -c 'SetupGnuIndent' -c 'normal gg=G' -c 'x! $@' - <test.cpp >vim.log 2>&1

nvim-result.cpp: $(DEPS)
	nvim --clean --headless -c 'set noet sw=2 ts=8 tw=80' -c 'source plugin/gnuindent.vim' -c 'SetupGnuIndent' -c 'normal gg=G' -c 'x! $@' - <test.cpp >nvim.log 2>&1

help:
	@echo "check"
	@echo "check-vim"
	@echo "check-nvim"
