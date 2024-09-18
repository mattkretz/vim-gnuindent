# gnuindent.vim

[![CI](https://github.com/mattkretz/vim-gnuindent/actions/workflows/CI.yml/badge.svg)](https://github.com/mattkretz/vim-gnuindent/actions/workflows/CI.yml)

This is a replacement for cindent to indent GNU code like in libstdc++. Execute 
the `SetupGnuIndent` command and you're ready to go. The command sets 
`indentexpr` to `GnuIndent()` and `indentkeys` to something more helpful. The 
function `GnuIndent()` returns the indent for the requested line, similar to 
the `cindent()` function.

The plugin is not configurable at this point (except via the shiftwidth 
option).

## Installation

    mkdir -p ~/.vim/pack/mattkretz/start
    cd ~/.vim/pack/mattkretz/start
    git clone https://github.com/mattkretz/vim-gnuindent

## Known Issues

This indenter has to rely on heuristics rather than a full understanding of the 
C++ code. Looking at code in isolation (e.g. `a<b`: template argument list or 
less-than operator?) is never enough. But you don't want to execute a 
full-blown C++ front-end in vim-script every time vim requests re-indentation. 
It also needs to be fast enough. Indenting whole files already can be quite 
slow.

That said, if you find an annoying mis-indentation, please submit an issue.

## License

Copyright © Matthias Kretz.  Distributed under the same terms as Vim itself.
See `:help license`.
