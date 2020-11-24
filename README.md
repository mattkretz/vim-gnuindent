# gnuindent.vim

This is a replacement for cindent to indent GNU code like in libstdc++. Set 
`indentexpr` to `GnuIndent()` and you're set. The function `GnuIndent()` returns 
the indent for the requested line, similar to the `cindent()` function.

The plugin is not configurable at this point.

## Installation

    mkdir -p ~/.vim/pack/mattkretz/start
    cd ~/.vim/pack/mattkretz/start
    git clone https://github.com/mattkretz/vim-gnuindent

## License

Copyright © Matthias Kretz.  Distributed under the same terms as Vim itself.
See `:help license`.
