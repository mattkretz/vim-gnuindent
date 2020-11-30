# gnuindent.vim

This is a replacement for cindent to indent GNU code like in libstdc++. Execute 
the SetupGnuIndent command and you're ready to go. It command sets `indentexpr` 
to `GnuIndent()` and indentkeys to something more helpful. The function 
`GnuIndent()` returns the indent for the requested line, similar to the 
`cindent()` function.

The plugin is not configurable at this point (except via the shiftwidth 
option).

## Installation

    mkdir -p ~/.vim/pack/mattkretz/start
    cd ~/.vim/pack/mattkretz/start
    git clone https://github.com/mattkretz/vim-gnuindent

## License

Copyright © Matthias Kretz.  Distributed under the same terms as Vim itself.
See `:help license`.
