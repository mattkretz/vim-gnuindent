"nmap <F5> :source ~/.vim/pack/mattkretz/start/vim-gnuindent/plugin/gnuindent.vim<CR>:echo GetCxxContextTokens(line('.')-1, 5)<CR>
command! SetupGnuIndent setlocal indentexpr=GnuIndent() indentkeys=:,0#,!^F,o,O,e,(,0),<>>,0<lt>
 \0a,0b,0c,0d,0<e>,0f,0g,0h,0i,0j,0k,0l,0m,0n,0<o>,0p,0q,0r,0s,0t,0u,0v,0w,0x,0y,0z,
 \0A,0B,0C,0D,0E,0F,0G,0H,0I,0J,0K,0L,0M,0N,0<O>,0P,0Q,0R,0S,0T,0U,0V,0W,0X,0Y,0Z,
 \0~,0<!>,0%,0^,0&,0<*>,0-,0_,0+,0=,0{,0},0[,0],0;,0',0.,0?,0/,0<0>,01,02,03,04,05,06,07,08,09,0<:>,0\<Bar>,0\"

function! s:Info(...) "{{{1
  let stack = substitute(expand('<stack>'), expand('<SID>'), 's:', 'g')
  echohl MoreMsg
  echom substitute(stack, 'function \(.*\)\.\.s:Info\[1\]', '\1: ', '').join(a:000)
  echohl None
endfunction

function! s:Debug(...) "{{{1
  return
  let stack = substitute(expand('<stack>'), expand('<SID>'), 's:', 'g')
  echohl Debug
  echom substitute(stack, 'function \(.*\)\.\.s:Debug\[1\]', '\1: ', '').join(a:000)
  echohl None
endfunction "}}}1

function! s:IndexOfMatchingToken(tokens, start) "{{{1
  " Returns the index of the token matching tokens[start]. If start equals len(tokens)
  " then a matching preceding opening token is searched for.
  " If no matching token exists, return -1.
  let n = len(a:tokens)
  let depth_idx = {'<': 0, '>': 0, '>>': 0, '(': 1, ')': 1, '{': 2, '}': 2, '[': 3, ']': 3}
  let depth = [0, 0, 0, 0, 0]
  let direction = 0
  if type(a:start) == v:t_string
    let depth[depth_idx[a:start]] = 1 + (a:start == '>>')
    let i = n
    let direction = -1
  else
    let i = a:start
    if i < 0
      let i += n
    endif
    if i == n
      let depth[4] = 1
      let direction = -1
    else
      if a:tokens[i] =~ '^[<{(\[]$'
        let direction = 1
      elseif a:tokens[i] =~ '^[>})\]]$' || a:tokens[i] == '>>'
        let direction = -1
      else
        " there's no token to find a match for
        return -1
      endif
      let depth[depth_idx[a:tokens[i]]] = 1 + (a:tokens[i] == '>>')
    endif
  endif
  "call s:Debug(a:tokens, a:start, depth, direction)
  if direction != 0
    let i += direction
    while i < n && i >= 0
      if a:tokens[i] == '<'
        if direction == -1 && depth[0] + depth[4] == 0
          "it's a less-than operator
        else
          let depth[0] += direction
        endif
      elseif a:tokens[i] =~ '^[{(\[]$'
        let depth[depth_idx[a:tokens[i]]] += direction
      elseif a:tokens[i] =~ '^[>})\]]$'
        let depth[depth_idx[a:tokens[i]]] -= direction
      elseif a:tokens[i] == '>>'
        if direction == -1
          " try whether we can find matching < tokens
          if s:IndexOfMatchingToken(a:tokens[:i], -1) >= 0
            " we have to assume it's not a shift operator
            let depth[0] += 2
          endif
        elseif depth[0] >= 2
          let depth[depth_idx[a:tokens[i]]] -= 2
        elseif depth[1:3] == [0, 0, 0]
          "call s:Debug(a:tokens, a:start, "returns", i)
          return i
        endif
      endif
      if depth == [0, 0, 0, 0, 0]
        "call s:Debug(a:tokens, a:start, "returns", i)
        return i
      elseif depth[4] == 1
        let d = sort(depth[:3])
        " A single opening token, while trying to ignore greater-than
        " operators (depth[0] > 0)
        if d == [-1, 0, 0, 0] || (d[:2] == [-1, 0, 0] && depth[0] > 0)
          "call s:Debug(a:tokens, a:start, "returns", i)
          return i
        endif
      endif
      let i += direction
    endwhile
  endif
  "call s:Debug(a:tokens, a:start, "returns -1")
  return -1
endfunction

function! s:Index(tokens, pattern, ...) "{{{1
  " Returns the smallest index i where tokens[i] matches pattern.
  " An optional third argument initializes i.
  let i = 0
  if a:0 == 1
    let i = a:1
  endif
  let n = len(a:tokens)
  while i < n && a:tokens[i] !~ a:pattern
    let i += 1
  endwhile
  if i < n
    return i
  else
    return -1
  endif
endfunction

function! s:GetSrcLine(n) "{{{1
  " Returns a string of the n'th line of the current buffer with some extra features:
  " 1. If preceding lines end with a '\', prepend those lines (without the '\')
  " 2. Remove all /**/ and // comments
  let i = a:n
  let s = getline(i)
  while getline(i-1)[-1:] == '\'
    let i -= 1
    let s = getline(i)[:-2] . s
  endwhile
  let s = substitute(s, '/\*.\{-}\*/\s*', ' ', 'g')
  let s = substitute(s, '^.\{-}\ze\*/', '', '')
  return substitute(s, '\s*\(//.*\)\?$', '', '')
endfunction "}}}1
function! s:GetSrcLineDropContinuations(n) "{{{1
  " Returns a string of the n'th line of the current buffer with some extra features:
  " 1. If this line ends with a '\', remove it
  " 2. Remove all /**/ and // comments
  let i = a:n
  let s = getline(i)
  if s[-1:] == '\'
    let s = s[:-2]
  endif
  let s = substitute(s, '/\*.\{-}\*/\s*', ' ', 'g')
  let s = substitute(s, '^.\{-}\ze\*/', '', '')
  return substitute(s, '\s*\(//.*\)\?$', '', '')
endfunction "}}}1

function! s:IndentForAlignment(lnum, pattern, ...) "{{{1
  " Returns the number of columns to indent to align with the pattern in the given linenumber.
  " lnum: line number where to align to
  " pattern: The match string determines the number of columns to indent
  "   additionally to the indent of the referenced line
  let s = getline(a:lnum)
  let s = substitute(s, '/\*\zs.\{-}\ze\*/', {m -> repeat(' ', strlen(m[0]))}, 'g')
  let s = substitute(s, '//\zs.*$', '', '')
  "call s:Debug("'".s."'")
  let p = a:pattern
  if a:0 == 1
    let tok2 = map(copy(a:1), {_, val -> val =~ s:identifier_token ? '\<'.val.'\>' : val})
    let i = 0
    while len(tok2) > i && s =~ p.'\V'.join(tok2[:i], '\.\*')
      let i += 1
    endwhile
    if i > 0
      let p .= '\V'.join(tok2[:i-1], '\.\*')
    endif
  endif
  return indent(a:lnum) + strlen(matchstr(s, p))
endfunction

function! s:GetPrevSrcLineInMacro(lnum) "{{{1
  " Returns [linenumber, string of source code].
  " Similar to GetPrevSrcLine except that a:lnum must point to a line in a macro definition.
  let plnum = a:lnum
  let line = ""
  while (empty(line)) && plnum > 1
    let plnum -= 1
    let line = getline(plnum)
    if line[-1:] == '\'
      let line = line[:-2]
    endif
    let line = substitute(line, '/\*.\{-}\*/\s*', ' ', 'g')
    let line = substitute(line, '\s*$', '', '')
  endwhile
  return [plnum, line]
endfunction

function! s:GetPrevSrcLine(lnum) "{{{1
  " Returns [linenumber, string of source code].
  " Similar to GetSrcLine(lnum - 1), except that this function searches more greedily for actual source code.
  let plnum = a:lnum - 1
  let line = s:GetSrcLine(plnum)
  let maybe_macro = 1
  " ignore preprocessor directives and labels
  while (empty(line) || line == '*/' || line =~ '^\s*#' || line =~ '^\s*\i\+\s*::\@!') && plnum > 1
    if maybe_macro && line =~ '^\s*#\s*define\s'
      return s:GetPrevSrcLineInMacro(a:lnum)
    endif
    let maybe_macro = 0
    if line =~ '^\s*#\s*el\%(se\|if\)\>'
      " walk up beyond the #if that started it
      let depth = 1
      while plnum > 0 && depth > 0
        let plnum -= 1
        let line = s:GetSrcLine(plnum)
        if line =~ '^\s*#\s*if\%(n\?def\)\?\>'
          let depth -= 1
        elseif line =~ '^\s*#\s*endif\>'
          let depth += 1
        endif
      endwhile
    elseif substitute(getline(plnum), '/\*.\{-}\*/', '', 'g') =~ '^.*\*/'
      while plnum > 0 && getline(plnum - 1) !~ '/\*'
        let plnum -= 1
      endwhile
    endif
    let plnum -= 1
    let line = s:GetSrcLine(plnum)
  endwhile
  return [plnum, line]
endfunction "}}}1

" Almost as defined in the C++ grammar...
" Note that the unary +/- on int_literal and float_literal is not correct but hopefully
" a good approximation
" grammar patterns {{{1
let     s:digit_seq = '\%(\d\%(''\?\d\)*\)'
let s:hex_digit_seq = '\%(\x\%(''\?\x\)*\)'
let s:exponent_part = '\%([eE][+-]\?'.s:digit_seq.'\)'
let s:bin_exponent_part = '\%([pP][+-]\?'.s:digit_seq.'\)'
let s:bin_literal = '\%(0[bB][01]\%(''\?[01]\)*\)'
let s:oct_literal = '\%(0\%(''\?[0-7]\)*\)'
let s:dec_literal = '\%([1-9]\%(''\?\d\)*\)'
let s:hex_literal = '\%(0[xX]'.s:hex_digit_seq.'\)'
" FIXME: int_literal includes unary +/- even though that's wrong and leads to
" indent errors. (`a\n+1` is different to `a\n+ 1`)
let s:int_literal =
  \ '[+-]\?\%('.s:bin_literal
  \ .'\|'.s:oct_literal
  \ .'\|'.s:dec_literal
  \ .'\|'.s:hex_literal
  \ .'\)\%([uU]ll\?\|[uU]LL\?\|ll\?[uU]\|LL\?[uU]\)\?'
let s:fractional_constant = '\%('.s:digit_seq.'\?\.'.s:digit_seq.'\|'.s:digit_seq.'\.\)'
let s:hex_fractional_constant = '\%('.s:hex_digit_seq.'\?\.'.s:hex_digit_seq.'\|'.s:hex_digit_seq.'\.\)'
let s:identifier = '[a-zA-Z_]\i*'
let s:identifier_token = '^[a-zA-Z_]\i*$'
let s:class_key_token = '^\%(class\|struct\|union\)$'
let s:alternate_operators = 'and\|and_eq\|bitand\|bitor\|not\|not_eq\|or\|or_eq\|xor\|xor_eq'
let s:keyword_token = '^\%('.s:alternate_operators.'\|alignas\|alignof\|asm\|auto\|bool\|break\|case\|catch\|char\|char8_t\|char16_t\|char32_t\|class\|compl\|concept\|const\|consteval\|constexpr\|constinit\|const_cast\|continue\|co_await\|co_return\|co_yield\|decltype\|default\|delete\|do\|double\|dynamic_cast\|else\|enum\|explicit\|export\|extern\|false\|float\|for\|friend\|goto\|if\|inline\|int\|long\|mutable\|namespace\|new\|noexcept\|nullptr\|operator\|private\|protected\|public\|reflexpr\|register\|reinterpret_cast\|requires\|return\|short\|signed\|sizeof\|static\|static_assert\|static_cast\|struct\|switch\|template\|this\|thread_local\|throw\|true\|try\|typedef\|typeid\|typename\|union\|unsigned\|using\|virtual\|void\|volatile\|wchar_t\|while\)$'
let s:nontype_keyword_token = '^\%('.s:alternate_operators.'\|alignas\|alignof\|asm\|break\|case\|catch\|class\|compl\|concept\|const\|consteval\|constexpr\|constinit\|const_cast\|continue\|co_await\|co_return\|co_yield\|decltype\|default\|delete\|do\|dynamic_cast\|else\|enum\|explicit\|export\|extern\|false\|for\|friend\|goto\|if\|inline\|mutable\|namespace\|new\|noexcept\|nullptr\|operator\|private\|protected\|public\|reflexpr\|register\|reinterpret_cast\|requires\|return\|sizeof\|static\|static_assert\|static_cast\|struct\|switch\|template\|this\|thread_local\|throw\|true\|try\|typedef\|typeid\|typename\|union\|using\|virtual\|volatile\|while\)$'
let s:decfloat_literal = '\%(\%('.s:fractional_constant.s:exponent_part.'\?\|'
  \ .s:digit_seq.s:exponent_part.'\)[fFlL]\?\)'
let s:hexfloat_literal = '\%(0[xX]\%('.s:hex_fractional_constant.'\|'.s:hex_digit_seq.'\)'
  \ .s:bin_exponent_part.'[fFlL]\?\)'
let s:float_literal = '[+-]\?\%('.s:decfloat_literal.'\|'.s:hexfloat_literal.'\)'
let s:encoding_prefix_opt = '\%(u8\|[uUL]\)\?'
let s:escape_sequence = '\\[''"?\\abfnrtv]\|\\\o\{1,3}\|\\x\x\x\?'
let s:char_literal = s:encoding_prefix_opt.'''\%([^''\\]\|'.s:escape_sequence.'\)'''
let s:string_literal = s:encoding_prefix_opt.'\%("\%([^"\\]\|\\[uU]\%(\x\{4}\)\{1,2}\|'.s:escape_sequence.'\)*"\|R"\([^(]*\)(.\{-})\1"\)'
let s:logic_binary_operators = 'and\|or\|&&\|||'
let s:operators2 = s:logic_binary_operators.'\|->\*\?\|\.\.\.\|\.\*\?\|[()\[\]]\|'.'++\|--\|xor\|not\|<=>\|[|&^!=<>*/%+-]=\|<<=\?\|>>=\?\|::\|[~,:?|&^!=<>*/%+-]'
let cxx_tokenize = '\%(\%(sizeof\.\.\.\|'.s:identifier.'\|'.s:float_literal.'\|'.s:int_literal.'\|'.s:char_literal.'\|[;{}]\|'.s:operators2.'\)\@>\zs\)\|\s\+\|\%('.s:char_literal.'\|'.s:string_literal.'\)\@>\zs'

let s:assignment_operators = '\%(<<\|>>\|[/%^&|*+-]\)\?='
let s:compare_operators = '<=>\|[<>!=]=\|[<>]'
let s:indent_operators = s:assignment_operators.'\|'.s:compare_operators
  \ .'\|->\*\?\|\.\*\?\|[(\[{]\|&&\|||\|<<\|>>\|[:?|&^*/%+-]'

let s:indent_op_token = '^\%('.s:indent_operators.'\)$'
let s:assignment_op_token = '^'.s:assignment_operators.'$'
let s:compare_op_token = '^\%('.s:compare_operators.'\)$'
let s:shift_op_token = '^\%(<<\|>>\)$'
let s:arith_op_token = '^[/*%+-]$'
let s:bit_op_token = '^[~&|^]$'
let s:logic_op_token = '^\%(&&\|||\|!\)$'
let s:operators2_token = '^\%('.s:operators2.'\)$'
let s:operator_fun = '\%(?:\|<=>\|&&\|||\|<<\|>>\|\[\]\|()\|++\|--\|<<=\|>>=\|->\|[<>~!%^&*/=|,+-]=\?\)'
let s:operator_fun_token = '^'.s:operator_fun.'$'

function! s:CxxTokenize(context) "{{{1
  " Returns a list of C++ tokens from the given string of code in context.
  return filter(split(a:context, g:cxx_tokenize), '!empty(v:val)')
endfunction

function! s:SimplifyContext(context, to_keep, ...) "{{{1
  let context = a:context
  if a:0 == 1
    let pattern = a:1
    let pattern2 = '^'.pattern[1:]
  elseif a:0 == 2
    let matched_tok = a:1.'\s*'.a:2
    for tmp in [1, 2, 3, 4, 5, 6]
      let matched_tok = a:1.'\%(\s*\|'.matched_tok.'\)*'.a:2
    endfor
    let pattern = '\%([^'.a:to_keep.']\+\|'.matched_tok.'\)\+'.a:2
    let pattern2 = '^'.pattern
    let pattern = a:1.pattern
  else
    throw "invalid arguments to SimplifyContext"
  endif
  "call s:Debug(a:context, a:to_keep, pattern, pattern2)
  if a:0 == 1 || a:2 != '>'
    let context = substitute(context, pattern2, '', '')
  endif
  let m = matchstrpos(context, pattern)
  "call s:Debug("            next:", context, m)
  while m[1] != -1
    "call s:Debug("matched:", m)
    let left = strcharpart(context, 0, m[1])
    if       count(m[0], '(') != count(m[0], ')')
        \ || count(m[0], '{') != count(m[0], '}')
        \ || count(m[0], '[') != count(m[0], ']')
        \ || m[2] - m[1] < 2
        \ || m[0] =~ '^[ '.a:to_keep.']*$'
        \ || (a:0 == 2 && a:2 == '>' && context[m[2]] == '>' && count(left, '<') <= count(left, '>'))
      " The above special-cases <...> where ...<...>>... might otherwise be
      " interpreted as two closing angle brackets instead of a shift operator
      let m = matchstrpos(context, pattern, m[2])
      "call s:Debug("   skipped. next:", context, m)
    else
      let right = strcharpart(context, m[2])
      let context = left.substitute(m[0], '[^'.a:to_keep.']\+', ' ', 'g').right
      let m = matchstrpos(context, pattern)
      "call s:Debug("simplified. next:", context, m)
    endif
  endwhile
  return context
endfunction

function! s:WalkUpForMoreContext(nextline) "{{{1
  let nextline = a:nextline
  let s = getline(nextline)
  let nextline -= 1
  while getline(nextline)[-1:] == '\'
    let s = getline(nextline)[:-2] . s
    let nextline -= 1
  endwhile
  " ignore preprocessor directives and labels
  if s =~ '^\s*#' || s =~ '^\s*\i\+\s*::\@!'
    if s =~ '^\s*#\s*el\%(se\|if\)\>'
      " walk up beyond the #if that started it
      let depth = 1
      while nextline > 0 && depth > 0
        let s = getline(nextline)
        let nextline -= 1
        if s =~ '^\s*#\s*if\%(n\?def\)\?\>'
          let depth -= 1
        elseif s =~ '^\s*#\s*endif\>'
          let depth += 1
        endif
      endwhile
    endif
    return s:WalkUpForMoreContext(nextline)
  endif
  " trim whitespace and drop // comments
  return [nextline, substitute(s, '^\s*\(.\{-}\)\s*\(//.*\)\?$', '\1', '')]
endfunction

function! GetCxxContextTokens(from, n, ...) "{{{1
  " Returns a list of tokens preceding and including line a:from.
  " Considers at least a:n lines / a:n * &textwidth characters (excluding preprocessor directives and comments).
  " The context is increased if it doesn't suffice to determine indenting width:
  " ['}'] does not suffice and requires more context to see what precedes the matching opening brace.
  "
  " The context is subsequently simplified / minimized (see below for details).
  " The minimized context is the tokenized using CxxTokenize.
  " The token list is further simplified:
  " - keep only tokens after last block {...}, last ';', or last unmatched '{' (with exceptions)
  " - char literals are represented by the single token '''
  " - string literals are represented by the single token '"'
  "
  let context = join(a:000)
  let nextline = a:from
  let n = 0
  if getline(nextline)[-1:] == '\'
    let s = getline(nextline)[:-2]
    let nextline -= 1
    while getline(nextline)[-1:] == '\'
      let s = getline(nextline)[:-2] . s
      let nextline -= 1
    endwhile
    if s =~ '^\s*#\s*define\s'
      " that's it, no more context needed
      return s:SimplifyAndTokenize(s)
    else
      let n = 1
    endif
  endif

  while nextline > 0 && (n < a:n || strlen(context) < a:n * &tw || getline(nextline) !~ '^\s*$') "{{{2
    let [nextline, s] = s:WalkUpForMoreContext(nextline)
    if !empty(s)
      let n += 1
      let context = s . ' ' . context
    endif
  endwhile
  "call s:Debug("done after line", nextline, context)

  " get more context to see what the last closing brace belongs to {{{2
  if count(context, '{') < count(context, '}')
    let need_more = 1
    while nextline > 0 && need_more
      let [nextline, s] = s:WalkUpForMoreContext(nextline)
      if empty(s)
        if need_more > 1
          break
        else
          continue
        endif
      endif
      let context = s . ' ' . context
      let need_more += (count(context, '{') >= count(context, '}'))
    endwhile
    "call s:Debug("done after line", nextline, context)
  endif

  " go back beyond the start of an intializer list {{{2
  while nextline > 0 && count(context, ';') == 0 && count(context, '{') <= count(context, '}')
    let [nextline, s] = s:WalkUpForMoreContext(nextline)
    if !empty(s)
      let context = s . ' ' . context
    endif
  endwhile
  "call s:Debug("done after line", nextline, context)
  return s:SimplifyAndTokenize(context)
endfunction

function! s:MatchesBaseClassCtorInheritance(context) "{{{1
  let tokens = a:context
  let n = len(tokens)
  let i = n - 1
  while n >= 2
    if n >= 4 && tokens[i] == '>'
      let j = s:IndexOfMatchingToken(tokens, i)
      if j == -1
        return 0
      endif
      let j -= 1
      let n -= i - j
      let i = j
    endif
    if tokens[i] =~ s:identifier
      if tokens[i-1] == ':'
        return 1
      elseif tokens[i-1] == '::'
        let n -= 2
        let i -= 2
        if n >= 1 && tokens[i] == ':'
          return 1
        endif
        continue
      endif
    endif
    return 0
  endwhile
endfunction

function! s:SimplifyAndTokenize(context) "{{{1
  let context = a:context

  " drop /* */ comments {{{2
  let context = substitute(context, '/\*.\{-}\*/', ' ', 'g')
  let context = substitute(context, '^.\{-}\ze\*/', '', '')
  if context =~ '/\*'
    return ['/*']
  endif

  " remove char_literal and string_literal contents in case they contain {{{2
  " any of: <>(){}[]
  let context = substitute(context, s:char_literal, "'c'", 'g')
  let context = substitute(context, s:string_literal, '"string_literal"', 'g')

  " guard -> >= <= and << against <...> substitution. {{{2
  " This leaves operator> operator< and operator>> to disambiguate later
  let context = substitute(context, '->', '$`dr`', 'g')
  let context = substitute(context, '>>=', '$`sra`', 'g')
  let context = substitute(context, '>=', '$`ge`', 'g')
  let context = substitute(context, '<=', '$`le`', 'g')
  let context = substitute(context, '<<', '$`sl`', 'g')
  let context = substitute(context, 'operator\s*<', '$`oplt`', 'g')
  let context = substitute(context, 'operator\s*>>', '$`opsr`', 'g')
  let context = substitute(context, 'operator\s*>=', '$`opge`', 'g')
  let context = substitute(context, 'operator\s*>', '$`opgt`', 'g')

  " drop code in matching (), {}, [], and <> {{{2
  " in addition, the opening paren may instead be start-of-line
  " {}: replace everything inside with a space except {}
  " (): replace everything inside with a space except () and {}
  " []: replace everything inside with a space except []
  " <>: replace everything inside with a space except <>
  let context = substitute(context, '{\zs[^{}]\+\ze}', '', 'g')
  let context = substitute(context, '(\zs[^{}()]\+\ze)', '', 'g')
  let context = substitute(context, '\[\zs[^\[\]]\+\ze\]', '', 'g')
  let context = s:SimplifyContext(context, '{}', '{', '}')
  let matched_braces = '{\s*\%({\s*\%({\s*\%({\s*\%({\s*\%({\s*\%({\s*}\s*\)*}\s*\)*}\s*\)*}\s*\)*}\s*\)*}\s*\)*}'
  let matched_parens = '(\s*)'
  for tmp in [1, 2, 3, 4, 5, 6]
    let matched_parens = '(\%(\s*\|'.matched_parens.'\|'.matched_braces.'\)*)'
  endfor
  let pattern = '('.
              \ '\%([^{}()]\+'.
              \ '\|'.matched_parens.
              \ '\|'.matched_braces.
              \ '\)\+)'
  let context = s:SimplifyContext(context, '{}()', pattern)
  let context = s:SimplifyContext(context, '\[\]', '\[', '\]')
  let context = substitute(context, '<\zs[^<>()]\+\ze>\%($\|[^>]\)', '', 'g')
  let context = s:SimplifyContext(context, '<>', '<', '>')

  " get back the compare and shift operators {{{2
  let context = substitute(context, '$`lt`', '<', 'g')
  let context = substitute(context, '$`gt`', '>', 'g')
  let context = substitute(context, '$`sr`', '>>', 'g')
  let context = substitute(context, '$`sra`', '>>=', 'g')
  let context = substitute(context, '$`dr`', '->', 'g')
  let context = substitute(context, '$`ge`', '>=', 'g')
  let context = substitute(context, '$`le`', '<=', 'g')
  let context = substitute(context, '$`sl`', '<<', 'g')
  let context = substitute(context, '$`oplt`', 'operator<', 'g')
  let context = substitute(context, '$`opsr`', 'operator>>', 'g')
  let context = substitute(context, '$`opge`', 'operator>=', 'g')
  let context = substitute(context, '$`opgt`', 'operator>', 'g')

  " simplify `if constexpr` to `if` {{{2
  let context = substitute(context, '\<if\s*constexpr\>', 'if', 'g')

  " simplify conditional expressions `a?b:c` to `a c` {{{2
  let context = substitute(context, '\s*?\%([^?:]\|::\)*::\@!\s*', ' ?: ', 'g')

  " tokenize {{{2
  let tokens = s:CxxTokenize(context)
  if len(tokens) <= 1
    return tokens
  endif
  "call s:Debug(context, tokens)

  " drop requirement-parameter-list in a requires-expression {{{2
  let requires_idx = index(tokens, 'requires')
  while requires_idx > 0
    if requires_idx + 2 < len(tokens) && tokens[requires_idx+1] == '('
          \ && tokens[requires_idx-1] =~ '^\%('.s:logic_binary_operators.'\|(\|=\|requires\)$'
      let last_paren = s:IndexOfMatchingToken(tokens, requires_idx + 1)
      if last_paren > requires_idx + 1 && (last_paren + 1 == len(tokens) || tokens[last_paren + 1] == '{')
        let tokens = tokens[:requires_idx] + tokens[last_paren+1:]
        "call s:Debug("removed balanced parens after requires:", tokens)
      endif
    endif
    let requires_idx = index(tokens, 'requires', requires_idx + 1)
  endwhile

  " keep only tokens after last block {...}, last ';', or last unmatched '{', but: {{{2
  " 1. don't make the list empty
  " 2. don't consider blocks inside parens (...)
  " 3. don't delete anything inside an open for(...;...;
  " 4. don't remove blocks immediately followed by an opening paren `[]() {}();` should not become `();`
  " 5. don't remove blocks following a requires keyword
  " 6. don't remove blocks directly preceding a binary operator (likely a ctor call)
  call reverse(tokens)
  let idx1 = index(tokens, ';', 1)
  if idx1 != -1
    let for_idx = index(tokens, 'for', idx1)
    if for_idx != -1 && tokens[for_idx-1] == '('
        \ && count(tokens[idx1+1:for_idx-2], '(') == count(tokens[idx1+1:for_idx-2], ')')
      " make sure we're inside the for-parens
      let lo = index(tokens, ';') + 1
      let hi = for_idx - 2
      let depth = 0
      for tok in tokens[lo:hi]
        if tok == ')'
          let depth += 1
        elseif tok == '('
          let depth -= 1
        endif
        if depth < 0
          break
        endif
      endfor
      if depth == 0
        call filter(tokens, {i, v -> i < lo || i > hi || v =~ '^[;()]$'})
        let idx1 = index(tokens, ';', index(tokens, 'for', 1))
      endif
    endif
  endif
  let idx3 = index(tokens, '}', 1 + (tokens[0] == ';'))
  while idx3 != -1 && (count(tokens[:idx3], ')') > count(tokens[:idx3], '(')
      \ || tokens[idx3-1] == '->' || tokens[idx3-1] == ',' || tokens[idx3-1] == '('
      \ || tokens[idx3-1] =~ '[-+*/%|&^.<=>?:!]'
      \ || tokens[idx3-1] =~ '^\%('.s:alternate_operators.'\)$')
    let idx3 = index(tokens, '}', idx3 + 1)
  endwhile
  let idx3 = -1 - idx3
  let idx2 = index(tokens, '{', 1)
  let brace_count = 1
  while idx2 != -1 && count(tokens[:idx2-1], '}') >= brace_count
    let idx2 = index(tokens, '{', idx2+1)
    let brace_count += 1
  endwhile
  call reverse(tokens)
  "call s:Debug("remove irrelevant context", idx1, idx2, idx3, tokens)
  if idx1 > -1 && idx2 > -1
    if idx1 < idx2
      let tokens = tokens[len(tokens) - idx1:]
    else
      let tokens = tokens[len(tokens) - idx2:]
    endif
  elseif idx2 > -1
    let tokens = tokens[len(tokens) - idx2:]
  elseif idx1 > -1
    let tokens = tokens[len(tokens) - idx1:]
  endif
  if idx3 != 0
    "call s:Debug("remove unrelated blocks", tokens)
    let i = 0
    while 1
      let open_idx = index(tokens, '{', i)
      while open_idx > 2 && (tokens[open_idx-1] == 'requires' 
            \ || (tokens[open_idx-1] == ')' && tokens[open_idx-2] == '('
            \       && tokens[open_idx-3] == 'requires')
            \ || (tokens[open_idx-1] =~ s:identifier_token && tokens[open_idx-2] =~ '^[,:]')
            \ || s:MatchesBaseClassCtorInheritance(tokens[:open_idx-1]))
        " skip if it matches `, foo{x}` or `: foo{x}` or `: base<x>{y}`, which can occur right
        " before a block with ctors.
        let open_idx = s:IndexOfMatchingToken(tokens, open_idx)
        if open_idx != -1
          let open_idx = index(tokens, '{', open_idx + 1)
        endif
      endwhile
      let i = open_idx + 1
      if open_idx == -1 || i > len(tokens) + idx3
        break
      endif
      "call s:Debug(i, tokens[i], idx3, tokens[idx3])
      let depth = 1
      while depth > 0 && i <= len(tokens) + idx3
        let depth += (tokens[i] == '{') - (tokens[i] == '}')
        let i += 1
      endwhile
      " require balanced {} and () tokens
      if depth != 0
        break
      endif
      "call s:Debug(tokens[:i-1], tokens[i:], idx3)
      if count(tokens[:i-1], '(') == count(tokens[:i-1], ')')
        let tokens = tokens[i:]
        let i = 0
        "call s:Debug("shorten tokens to", tokens)
      "else
        "call s:Debug("unbalanced parenthesis", count(tokens[:i-1], '('), count(tokens[:i-1], ')'), i-1, tokens[:i-1])
      endif
    endwhile
  endif
  while len(tokens) > 1 && tokens[0] == '}'
    let tokens = tokens[1:]
    "call s:Debug tokens
  endwhile

  " remove noise before a template head {{{2
  " (e.g. _GLIBCXX_BEGIN_NAMESPACE_VERSION)
  let template_idx = index(tokens, 'template')
  if template_idx > 0 && template_idx+1 < len(tokens) && tokens[template_idx+1] == '<'
    let tokens = tokens[template_idx:]
  endif

  " clean up the char_literal and string_literal placeholders {{{2
  call map(tokens, {i, v -> [v, v[0]][v == "'c'" || v == '"string_literal"']})
  return tokens
endfunction

function! s:RemoveControlTokens(tokens) "{{{1
  " remove leading if/else/for/while if there's more after it
  let paren_idx = -1
  let n = len(a:tokens)
  if a:tokens[0] == 'else' && n > 1
    if a:tokens[1] != 'if'
      return a:tokens[1:]
    else
      let paren_idx = 2
    endif
  elseif a:tokens[0] =~ '^\%(if\|for\|while\)$'
    let paren_idx = 1
  endif
  if paren_idx > 0 && n > paren_idx && a:tokens[paren_idx] == '('
    let closing_idx = s:IndexOfMatchingToken(a:tokens, paren_idx)
    if closing_idx != -1 && closing_idx + 4 < n
          \ && a:tokens[closing_idx + 1] == '[' && a:tokens[closing_idx + 2] == '['
      let closing_idx = s:IndexOfMatchingToken(a:tokens, closing_idx + 1)
    endif
    if closing_idx != -1 && closing_idx + 1 < n
      return a:tokens[closing_idx+1:]
    endif
  endif
  return a:tokens
endfunction

function! s:RemoveMatchingAngleBrackets(tokens) "{{{1
  let t = a:tokens[:]
  let i = index(t, '<')
  while i > 0
    let j = s:IndexOfMatchingToken(t, i)
    if j == -1
      return t
    endif
    let t = t[:i-1] + t[j+1:]
    let i = index(t, '<')
  endwhile
  return t
endfunction

function! s:GetPrevSrcLineMatching(lnum, pattern) "{{{1
  " Return largest linenumber (less than lnum) such that the code between the returned line and lnum matches pattern.
  " If no such linenumber can be found, returns -1.
  " The pattern can either be a regex or a list of tokens.
  if type(a:pattern) == v:t_list
    let max = &tw / 2
    if max < 40
      max = 40
    endif
    if len(a:pattern) > max
      let pattern = map(a:pattern[0:max], {_, val -> val =~ s:identifier_token ? '\<'.val.'\>' : val})
      let pattern = '\V'.join(pattern, '\.\*')
    else
      let pattern = '\V'.join(a:pattern, '\.\*')
    endif
  else
    let pattern = a:pattern
  endif
  let [lnum, line] = s:GetPrevSrcLine(a:lnum)
  let context = line
  while context !~ pattern
    if lnum <= 1
      "call s:Debug("failure searching for", a:lnum, pattern)
      return -1
    endif
    let [lnum, line] = s:GetPrevSrcLine(lnum)
    let context = line.context
  endwhile
  "call s:Debug("found", lnum, "for", a:lnum, pattern)
  return lnum
endfunction

function! s:TemplateIndent(tokens) "{{{1
  " count how many template heads are nested (and thus indented). Typically
  " only one; out-of-class definitions of member function templates of class
  " templates can have two.
  let n = len(a:tokens)
  let depth = 0
  if n >= 3 && a:tokens[0] == 'template' && a:tokens[1] == '<'
    let i = s:IndexOfMatchingToken(a:tokens, 1)
    while i != -1
      let depth += 1
      if i+3 < n && a:tokens[i+1] == 'template'
        let i = s:IndexOfMatchingToken(a:tokens, i+2)
      else
        break
      endif
    endwhile
  endif
  return shiftwidth() * depth
endfunction

function! s:AdvanceIdentifierOrSimpleTemplateId(tokens, idx) "{{{1
  let i = a:idx
  let n = len(a:tokens)
  if a:tokens[i] =~ s:identifier_token && a:tokens[i] !~ s:keyword_token
    if a:tokens[i + 1] == '<'
      let i = s:IndexOfMatchingToken(a:tokens, i + 1) + 1
      if i >= 3
        "call s:Debug("yes, simple-template-id", a:tokens[a:idx:i-1])
        return i
      endif
    else
      "call s:Debug("yes, identifier", a:tokens[i])
      return i + 1
    endif
  endif
  "call s:Debug("no")
  return a:idx
endfunction

function! s:AdvanceNestedNameSpecifier(tokens, idx) "{{{1
  " Returns idx if the tokens starting from idx do not match the
  " nested-name-specifier grammar.
  " Otherwise returns the index after the matching tokens.
  let i = a:idx
  let n = len(a:tokens)
  if a:tokens[i] == '::'
    let i += 1
  elseif a:tokens[i] =~ s:identifier_token && a:tokens[i] !~ s:keyword_token
    " type-name = (class|enum|typedef)-name = identifier/simple-template-id
    " namespace-name = identifier
    " computed-type-specifier = (decltype|pack-index)-specifier
    let i += 1
    if i + 1 < n && a:tokens[i] == '<'
      let i = s:IndexOfMatchingToken(a:tokens, i) + 1
    elseif i + 2 < n && a:tokens[i] == '...' && a:tokens[i + 1] == '['
      let i = s:IndexOfMatchingToken(a:tokens, i + 1) + 1
    elseif i + 2 < n && a:tokens[i] == 'decltype' && a:tokens[i + 1] == '('
      let i = s:IndexOfMatchingToken(a:tokens, i + 1) + 1
    endif
    if i <= 0 || i >= n || a:tokens[i] != '::'
      return a:idx
    endif
    let i += 1
  endif
  while i + 1 < n
    " identifier or templateₒₚₜ simple-template-id
    if a:tokens[i] == 'template' && i + 3 < n && a:tokens[i + 2] == '<'
      let i += 1
    endif
    if a:tokens[i] =~ s:identifier_token && a:tokens[i] !~ s:keyword_token
      if a:tokens[i + 1] == '::'
        let i += 2
      elseif a:tokens[i + 1] == '<'
        let j = s:IndexOfMatchingToken(a:tokens, i + 1) + 1
        if j <= 0 || a:tokens[j] != '::'
          return i
        endif
        let i = 1 + j
      else
        break
      endif
    else
      break
    endif
  endwhile
  return i
endfunction

function! s:AdvanceTemplateHead(tokens, idx) "{{{1
  " Returns idx if the tokens starting from idx do not match the
  " template-head grammar.
  " Otherwise returns the index after the matching tokens.
  let i = a:idx
  "call s:Debug("init", i)
  let n = len(a:tokens)
  " template head(s) with requires clauses
  while i + 1 < n && a:tokens[i] == 'template' && a:tokens[i+1] == '<'
    let i = s:IndexOfMatchingToken(a:tokens, i+1) + 1
    if i <= 0
      call s:Debug("no")
      return a:idx
    endif
    if i < n && a:tokens[i] == 'requires'
      while 1
        "call s:Debug("looping", i)
        let i += 1
        if a:tokens[i] == '('
          let i = s:IndexOfMatchingToken(a:tokens, i) + 1
        elseif a:tokens[i] == 'requires'
          " requirement-parameter-list always removed from a:tokens by GetCxxContextTokens
          "if a:tokens[i + 1] == '('
          "  let i = s:IndexOfMatchingToken(a:tokens, i + 1)
          "endif
          if a:tokens[i + 1] == '{'
            let i = s:IndexOfMatchingToken(a:tokens, i + 1) + 1
          else
            call s:Debug("no")
            return a:idx;
          endif
        else
          let last = i
          let j = s:AdvanceNestedNameSpecifier(a:tokens, i)
          let i = s:AdvanceIdentifierOrSimpleTemplateId(a:tokens, j)
          if j == i
            call s:Debug("no", i, j)
            return a:idx
          endif
          "call s:Debug("advance by ", a:tokens[last:i-1])
        endif
        if i == n
          call s:Debug("yes", i)
          return i
        elseif i <= 0 || i > n
          call s:Debug("no")
          return a:idx
        endif
        if a:tokens[i] !~ '^\%('.s:logic_binary_operators.'\)$'
          break
        endif
      endwhile
    endif
  endwhile
  call s:Debug("yes", i)
  return i
endfunction

function! s:AdvanceFunctionDecl(tokens, idx) "{{{1
  " Returns idx if the first tokens do not match the function decl/defn grammar
  " Otherwise returns the index after the closing paren of the function argument
  " list.
  let i = s:AdvanceTemplateHead(a:tokens, a:idx)
  let n = len(a:tokens)
  " return type and stuff
  let maybe_return_type = 0
  while i < n
    if i + 2 < n && a:tokens[i] == 'decltype' && a:tokens[i+1] == '('
      let i = s:IndexOfMatchingToken(a:tokens, i+1) + 1
      let maybe_return_type += 1
    elseif i + 2 < n && a:tokens[i] == 'explicit' && a:tokens[i+1] == '('
      let i = s:IndexOfMatchingToken(a:tokens, i+1) + 1
    elseif i + 2 < n && a:tokens[i] == 'operator' && maybe_return_type > 0
      if (a:tokens[i+1] =~ s:operator_fun_token && a:tokens[i+2] == '(')
        let i += 2
      elseif (a:tokens[i+1] == '(' && a:tokens[i+2] == ')' && a:tokens[i+3] == '(')
            \ || (a:tokens[i+1] == '[' && a:tokens[i+2] == ']' && a:tokens[i+3] == '(')
        let i += 3
      else
        call s:Debug("parse error")
      endif
      break
    elseif a:tokens[i] =~ s:identifier_token
      if a:tokens[i] !~ s:nontype_keyword_token
        " it's either the function name, a macro, or the return type
        let maybe_return_type += 1
      endif
      let i += 1
    elseif a:tokens[i] =~ '^\%(::\|\*\|&&\?\)$'
      let i += 1
    elseif a:tokens[i] == '<'
      let i = s:IndexOfMatchingToken(a:tokens, i) + 1
    else
      break
    endif
    if i <= 0
      break
    endif
  endwhile
  if i <= 0 || i >= n || a:tokens[i] != '(' || maybe_return_type < 1
    return a:idx
  endif
  let i = s:IndexOfMatchingToken(a:tokens, i)
  if i <= 0 || i >= n
    return a:idx
  else
    return i + 1
  endif
endfunction

function! s:AdvanceClassHead(tokens, idx) "{{{1
  " Returns idx if the first tokens do not match the template-headₒₚₜ class-head (without base-clause) grammar.
  " Otherwise returns the index after the closing paren of the function argument
  " list.
  let i = s:AdvanceTemplateHead(a:tokens, a:idx)
  let n = len(a:tokens)
  if i < n && a:tokens[i] =~ s:class_key_token
    let i = s:AdvanceAttributeSpecifierSeq(a:tokens, i + 1)
    if i >= n
      return a:idx
    endif
    let i = s:AdvanceNestedNameSpecifier(a:tokens, i)
    if a:tokens[i] =~ s:identifier_token
      let i += 1
      if i < n && a:tokens[i] == '<'
        let i = s:IndexOfMatchingToken(a:tokens, i) + 1
        if i <= 0
          return a:idx
        endif
      endif

      " class-virt-specifierₒₚₜ
      if i < n && a:tokens[i] == 'final'
        let i += 1
      endif

      " don't advance base-clauseₒₚₜ
      return i
    endif
  endif
  return a:idx
endfunction

function! s:LastTokensIsAttribute(tokens) "{{{1
  " Returns 1 if the last tokens in a:tokens for either a GNU or a standard
  " C++ attribute. Otherwise returns 0.
  if a:tokens[-2:] == [')', ')']
    let i = s:IndexOfMatchingToken(a:tokens, -1)
    return (i > 0 && a:tokens[i - 1] == '__attribute__' && a:tokens[i + 1] == '(')
  elseif a:tokens[-2:] == [']', ']']
    let i = s:IndexOfMatchingToken(a:tokens, -1)
    return (i >= 0 && a:tokens[i + 1] == '[')
  endif
endfunction

function! s:AdvanceNoexceptSpecifier(tokens, idx) "{{{1
  " Returns idx if the tokens starting from idx do not match the
  " noexcept-specifier grammar.
  " Otherwise returns the index after the matching tokens.
  let i = a:idx
  if i < len(a:tokens) && a:tokens[i] == 'noexcept'
    if a:tokens[i + 1] == '('
      let i = s:IndexOfMatchingToken(a:tokens, i + 1)
      if i == -1
        call s:Debug("no, returns", a:idx)
        return a:idx
      endif
    endif
    call s:Debug("yes, returns", i + 1)
    return i + 1
  endif
  call s:Debug("no, returns", a:idx)
  return a:idx
endfunction

function! s:AdvanceAttributeSpecifierSeq(tokens, idx) "{{{1
  " Returns idx if the tokens starting from idx do not match the
  " attribute-specifier-seq grammar.
  " Otherwise returns the index after the matching tokens.
  let n = len(a:tokens)
  if a:idx + 3 < n && a:tokens[a:idx] == '[' && a:tokens[a:idx + 1] == '['
    let i = s:IndexOfMatchingToken(a:tokens, a:idx + 1) + 1
    if i < n && a:tokens[i] == ']'
      call s:Debug("yes, returns", i + 1)
      return i + 1
    endif
  endif
  call s:Debug("no, returns", a:idx)
  return a:idx
endfunction

function! s:WalkBackLambda(tokens, idx) "{{{1
  let i = a:idx
  if i < 0
    let i = len(a:tokens) + i
  endif
  " FIXME: requires-clause_opt
  " FIXME: trailing-return-type_opt
  while (i > 1 && a:tokens[i] == ']' && a:tokens[i - 1] == ']') || (i > 0 && a:tokens[i] == '_GLIBCXX_SIMD_ALWAYS_INLINE_LAMBDA')
    " attribute-specifier-seq_opt
    if (a:tokens[i] == ']')
      let i = s:IndexOfMatchingToken(a:tokens, i) - 1
    else
      let i -= 1
    endif
  endwhile
  " noexcept-specifier_opt FIXME: conditional noexcept
  if i > 0 && a:tokens[i] == 'noexcept'
    let i -= 1
  endif
  " lambda-specifier-seq
  while i > 0 && a:tokens[i] =~ '^\%(constexpr\|consteval\|mutable\|static\)$'
    let i -= 1
  endwhile
  if i > 0 && a:tokens[i] == ')'
    " jump over ( parameter-declaration-clause )
    let i = s:IndexOfMatchingToken(a:tokens, i) - 1
  endif
  while i > 1 && a:tokens[i] == ']' && a:tokens[i - 1] == ']'
    " attribute-specifier-seq_opt
    let i = s:IndexOfMatchingToken(a:tokens, i) - 1
  endwhile
  if i > 0 && a:tokens[i] == '>'
    " jump over template-parameter-list
    let i = s:IndexOfMatchingToken(a:tokens, i) - 1
  endif
  if i > 0 && a:tokens[i] == ']'
    let i = s:IndexOfMatchingToken(a:tokens, i)
    call s:Debug(a:tokens, a:idx, "->", i)
    return i
  endif
  return a:idx
endfunction

function! s:IsIndentAfterBlock(tokens, isFunDecl, current)
  " precondition: a:tokens[-1] == '}'
  " 1. false if an operator (that's not the start of an attribute) follows
  " 2. true after a compound-statement of a function-body
  " 3. false after a , (initializer list, argument list)
  " 4. false after a requirement-body
  " 5. false after compound-statement of a lambda-expression
  if a:current =~ '^\s*\%('.s:operators2.'\)' && a:current !~ '^\s*[['
    return 0
  elseif a:isFunDecl
    return 1
  endif
  let i = s:IndexOfMatchingToken(a:tokens, -1) " => a:tokens[i] == '{'
  if i > 0 && a:tokens[i - 1] != ',' && a:tokens[i - 1] != 'requires'
    return s:WalkBackLambda(a:tokens, i - 1) == i - 1
  elseif i == 0
    return 1
  endif
  return 0
endfunction

function! s:AlignedIndent(base_indent_type, base_indent, indent_offset, tokens, ctokens, align_to_identifier_before_opening_token) "{{{1
  if a:indent_offset == 0 && a:ctokens[0] =~ s:operators2_token && a:ctokens[0] !~ '^[\[({})\]]$'
    call s:Debug("++indent_offset because ctokens[0] =", a:ctokens[0])
    let indent_offset = shiftwidth()
  elseif a:indent_offset == 0 && a:tokens[-1] =~ s:operators2_token && a:tokens[-1] !~ '^[()<>\[\],]$'
    if a:tokens[-1] == '>>' && s:IndexOfMatchingToken(a:tokens, -1) >= 0
      let indent_offset = a:indent_offset
    elseif index(['&', '&&', '*'], a:tokens[-1]) >= 0
      " If the preceding token is & && * we might be looking at a type, not a
      " binary operator. That's hard to determine without a full AST. However,
      " GNU style wants binary operators at the start of the new line, not the
      " end of the preceding line. So let's take that as a heuristic.
      call s:Info("assuming", a:tokens[-2:] + a:ctokens[0:0], "is not a binary operation. Place '".a:tokens[-1]."' on the same line as '".a:ctokens[0]."' if that's wrong.")
      let indent_offset = a:indent_offset
    else
      call s:Debug("++indent_offset because tokens[-1] =", a:tokens[-1])
      let indent_offset = shiftwidth()
    endif
  else
    let indent_offset = a:indent_offset
  endif
  call s:Info(a:base_indent_type, a:base_indent, indent_offset, a:ctokens, a:align_to_identifier_before_opening_token)
  return a:base_indent + indent_offset
endfunction

function! GnuIndent(...) "{{{1
  " Return the indent column number according to the GNU and libstdc++ indenting rules (and my own, I guess).
  if a:0 == 0
    let lnum = v:lnum
  elseif type(a:1) == v:t_string
    let lnum = line(a:1)
  else
    let lnum = a:1
  endif
  let current = s:GetSrcLineDropContinuations(lnum)
  let is_access_specifier = 0
  "if current =~ '^\s*#' || current =~ '^\s*\i\+\s*::\@!' {{{2
  if current =~ '^\s*#' || current =~ '^\s*\i\+\s*::\@!'
    if current =~ '^\s*\%(public\|private\|protected\|signals\|slots\|Q_SIGNALS\|Q_SLOTS\)\s*:'
      let is_access_specifier = 1
    elseif current =~ '^\s*#\s*define\s'
      if getline(lnum) =~ '^\s*#\s*define\s'
        call s:Info("macro definition")
        return 0
      endif
      " indent code in lines after #define like normal code
    else
      call s:Info("preprocessor or label")
      return cindent(lnum)
    endif
  endif
  let tokens = GetCxxContextTokens(lnum-1, 5) " {{{2
  call s:Debug(tokens)
  let funDeclEnd = s:AdvanceFunctionDecl(tokens, 0) " {{{2
  call s:Debug("AdvanceFunctionDecl(tokens, 0) =", funDeclEnd, ", len(tokens) =", len(tokens))
  if empty(tokens) "{{{2
    call s:Info("no preceding code")
    return 0
  endif
  " remove #define name() {{{2
  if tokens[0] == "#define" && tokens[1] =~ s:identifier_token
    let tokens = tokens[2:]
    if tokens[0] == '(' && tokens[1] == ')'
      let tokens = tokens[2:]
    endif
    if empty(tokens)
      call s:Info("first line after #define")
      return shiftwidth()
    endif
    call s:Debug("hide macro defn", tokens)
  endif
  " extra indent for template heads (tokens[0] == 'template') {{{2
  if tokens[0] == 'template' && tokens[-1] =~ '>>\?'
    let i = s:IndexOfMatchingToken(tokens, -1)
    let depth = 1
    while i > 3 && tokens[i-1] == 'template'
      let i = s:IndexOfMatchingToken(tokens, i-2)
      let depth += 1
    endwhile
    if i == 1
      call s:Info("extra indent after template-head")
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      return indent(plnum) + shiftwidth() * depth
    endif
  endif
  " if-else / loops / try / catch {{{2
  if tokens[0] =~ '^\%(if\|else\|do\|for\|while\|try\|catch\)$' &&
        \ tokens[-1] =~ '^\%(else\|do\|consteval\|]\|)\|try\)$'
    let depth = 0
    let i = 0
    while i < len(tokens) && tokens[i] =~ '^\%(if\|for\|while\|else\|do\|try\|catch\)$'
      "call s:Debug("condblock check:", depth, i, tokens[i])
      if tokens[i] == 'else' || tokens[i] == 'do'
        if i+1 == len(tokens) || tokens[i+1] != 'if'
          let depth += 1
        endif
        let i += 1
      elseif tokens == ['try']
        let depth += 1
        break
      elseif tokens[i+1] == '('
        let i = s:IndexOfMatchingToken(tokens, i + 1)
        if i != -1 && i+2 < len(tokens) && tokens[i+1] == '[' && tokens[i+2] == '['
          let i = s:IndexOfMatchingToken(tokens, i + 1)
        endif
        if i != -1
          let depth += 1
          let i += 1
          call s:Debug("condblock depth:", depth, i)
        endif
      elseif tokens[i+1] == 'consteval'
        let i += 2
        let depth += 1
        call s:Debug("if consteval - depth:", depth, i)
        break " opening { required after 'if consteval'
      else
        call s:Debug("parse error in if/for/while", tokens)
        break
      endif
    endwhile
    if i > 0 && i < len(tokens)
      call s:Debug("dropping", tokens[:i-1], "from context to indent with real statement")
      let tokens = tokens[i:]
    elseif depth > 0
      call s:Info("extra indent for condblock", depth)
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      return s:IndentForAlignment(plnum, '^\s*\zs.\{-}\ze\<'.tokens[0].'\>') + shiftwidth() * depth
    endif
  endif
  "start of {} block (current =~ '^\s*{') {{{2
  if current =~ '^\s*{'
    if tokens[-1] == '{'
      let plnum = s:GetPrevSrcLineMatching(lnum, ['{'])
      call s:Info("indent block in a block", plnum)
      return indent(plnum) + shiftwidth()
    elseif tokens[-1] == 'requires'
      let concept_idx = index(tokens, 'concept')
      if concept_idx != -1
        let plnum = s:GetPrevSrcLineMatching(lnum, tokens[concept_idx:-1])
        call s:Info("align { of a requires-expression one shiftwidth after preceding concept keyword")
        return indent(plnum) + s:TemplateIndent(tokens)
      else
        let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
        call s:Info("align { of a requires-expression")
        return indent(plnum) + s:TemplateIndent(tokens) + shiftwidth()
      endif
    elseif tokens[-1] !~ '^[,(]$'
      " not a condblock
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      call s:Info("align block indent to preceding statement", plnum, tokens[0:4])
      return indent(plnum) + s:TemplateIndent(tokens)
    endif
    call s:Debug("fall through from block indent section")
  "indent after block {{{2
  elseif tokens[-1] == '}' && s:IsIndentAfterBlock(tokens, funDeclEnd > 0, current)
    let plnum = s:GetPrevSrcLineMatching(lnum, '\V\^\s\*'.join(tokens, '\.\*'))
    call s:Info("indent after block", plnum)
    return indent(plnum) + shiftwidth() * ((current =~ '^\s*->') - (current =~ '^\s*}') - is_access_specifier)
    "- shiftwidth() * (tokens[0] =~ 'template\|if\|else\|for\|while\|do')
  elseif tokens == ['}'] "{{{2
    " not enough context from GetContextTokens => fall back to cindent
    call s:Info("use cindent because of distant context")
    return cindent('.')
  "elseif tokens[-2:] == ['{', '}'] && tokens[-3] =~ 'if\|else\|for\|while\|do' {{{2
  elseif len(tokens) >= 3 && tokens[-2:] == ['{', '}'] && tokens[-3] =~ 'if\|else\|for\|while\|do'
    call s:Info("indent after condblock")
    let plnum = search('^\s*}', 'bnWz', 0, 20)
    return indent(plnum) - shiftwidth()
  "elseif current =~ '^\s*::\@!' {{{2
  elseif current =~ '^\s*::\@!'
    call s:Debug("leading ':', consider ?: alignment, class base type, or ctor initializer list")
    if count(tokens[:-2], '?') > count(tokens, ':')
      let i = -2
      while tokens[i] != '?' || tokens[i+1] == ':'
        let i -= 1
      endwhile
      let plnum = s:GetPrevSrcLineMatching(lnum,
          \ '\V\%('.tokens[i-1].'\.\*\|\^\s\*\)'.join(tokens[i:], '\.\*'))
      let previous = getline(plnum)
      let j = -1
      while j > i && previous !~ '\V'.join(tokens[i:j], '\.\*')
        let j -= 1
      endwhile
      let k = -len(tokens)
      while k < i && previous !~ '\V'.join(tokens[k:i], '\.\*').'\s\*'.join(tokens[i+1:j], '\.\*').'\.\*\$'
        let k += 1
      endwhile
      let pattern = '\V'.join(tokens[k:i], '\.\*').'\zs\s\*'.join(tokens[i+1:j], '\.\*').'\.\*\$'
      let previous = substitute(previous, pattern, '', '')
      let previous = substitute(previous, '\t', repeat('.', &ts), 'g')
      call s:Info("align : to corresponding ?", plnum, k, i, j)
      return strlen(previous) - 1
    endif

    if (s:AdvanceAttributeSpecifierSeq(tokens, s:AdvanceNoexceptSpecifier(tokens, funDeclEnd)) == len(tokens))
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      call s:Info("no indent for ctor initializer list")
      return indent(plnum) + s:TemplateIndent(tokens)
    endif

    if (s:AdvanceClassHead(tokens, 0) == len(tokens))
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      call s:Info("no indent for class base types")
      return indent(plnum) + s:TemplateIndent(tokens)
    endif
    call s:Debug("fall through from : rule")
  "elseif function defn/decl {{{2
  elseif current =~ '^\s*\%('.s:identifier.'\|operator\s*'.s:operator_fun.'\)\s*('
      \ && current !~ '^\s*\%(and\|or\|not\|bit_and\|bit_xor\|bit_or\)\>'
      \ && tokens[-1] =~ s:identifier_token.'\|>>\|>\|&\|&&\|\*\|)'
      \ && tokens[-1] !~ '^\%(else\|do\)$'
    let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
    call s:Info("function definition/declaration")
    return indent(plnum) + s:TemplateIndent(tokens)
  "elseif tokens[-1] == '{' {{{2
  elseif tokens[-1] == '{'
    if current =~ '^\s*}'
      call s:Info("indent closing brace of empty {} block")
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens[-1:])
      return indent(plnum)
    else
      let lambda_idx = s:WalkBackLambda(tokens, -2)
      if lambda_idx >= 0
        " This is a bit tricky:
        " 1.
        " a) auto x = []() {
        "      //
        "    }
        " b) foo([]() {
        "      //
        "    }
        " c) foo(a, b, c, []() {
        "      //
        "    }
        " d) return [] {
        "      //
        "    }
        " 2.
        " a) auto x = foo([] {
        "               //
        "             }
        " b) foo(bar([] {
        "          //
        " c)     }, [] {
        "          //
        "        },
        " d)     [] {
        "          //
        "        }
        " 3.
        " a) auto x = 1 + [] {
        "                   //
        "                 }
        " b) x += [] {
        "           //
        "         }
        "    foo(
        "      x,
        " c)   [] {
        "        //
        " d)   }, [] {
        "        //
        "      },
        " e)   [] {
        "        //
        "      }
        " f) [] {
        "      //
        "    }();
        " 
        let plnum = s:GetPrevSrcLineMatching(lnum, tokens[lambda_idx:])
        if lambda_idx == 0
          " 3f)
          call s:Info("align one sw behind lambda-introducer (3f)")
          return indent(plnum) + shiftwidth()
        endif
        let before_lambda = tokens[lambda_idx - 1]
        if before_lambda == 'return' || before_lambda == '='
          " 1a 1d
          let first = s:AdvanceTemplateHead(tokens, 0)
          let tokens = s:RemoveControlTokens(tokens[first:])
          let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
          call s:Info("align one sw behind first token:", tokens[0])
          return indent(plnum) + shiftwidth()
        endif
        let alignment = s:IndentForAlignment(plnum, '^\s*\zs.*\ze', tokens[lambda_idx:])
        if (alignment == indent(plnum))
          " 2d 3c 3e (3f)
          call s:Info("align one sw behind lambda-introducer (2d 3c 3e)")
          return alignment + shiftwidth()
        endif
        " the remaining cases are handled below
      else
        " first indent inside a new {} block (ignoring labels)
        let plnum = s:GetPrevSrcLineMatching(lnum, tokens[-1:])
        let previous = s:GetSrcLine(plnum)
        let ptok = s:CxxTokenize(previous)
        call s:Debug("ptok: ", ptok)
        if previous =~ '^\s*{' || (ptok[0] !~ ':' &&
            \ count(ptok, '(') == count(ptok, ')') &&
            \ count(ptok, '[') == count(ptok, ']') &&
            \ count(ptok, '<') == count(ptok, '>'))
            "\ count(ptok, '{') - 1 == count(ptok, '}') &&
          call s:Info("indent inside a new {} block:")
          return indent(plnum) + shiftwidth() * (1 - is_access_specifier)
        endif
      endif
      "call s:Debug("fall through to align_to_identifier_before_opening_token", plnum, previous, ptok)
    endif
  elseif tokens[0] == 'case' "{{{2
    let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
    let last_indent = s:IndentForAlignment(plnum, '^\s*\zs.\{-}\ze\V'.tokens[0])
    call s:Info("indent below case label:", plnum, last_indent)
    if current =~ '^\s*}'
      return last_indent - shiftwidth()
    elseif current =~ '^\s*case\>'
      return last_indent
    endif
    return last_indent + shiftwidth()
  elseif tokens[-1] == ';' && (tokens[0] != 'for' || s:IndexOfMatchingToken(tokens, 1) != -1) "{{{2
    let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
    call s:Info("align to indent of last statement:", plnum)
    let last_indent = s:IndentForAlignment(plnum, '^\s*\zs.\{-}\ze\V'.tokens[0])
    if current =~ '^\s*\%(}\|case\>\)' || is_access_specifier
      return last_indent - shiftwidth()
    else
      return last_indent
    endif
  "elseif current =~ '^\s*}' {{{2
  elseif current =~ '^\s*}'
    " Not enough context in tokens: it only goes back to the first element in
    " the initializer list. Query the context of the first element and align
    " to that.
    " If tokens starts on a new line, then we need to query context from the
    " preceding line, otherwise there's more context to consider
    let plnum = s:GetPrevSrcLineMatching(lnum, '\V\%(\^\|{\)\s\*'.join(tokens, '\.\*'))
    let pline = s:GetSrcLine(plnum)
    if pline =~ '^\s*'.tokens[0]
      let tokens2 = GetCxxContextTokens(plnum-1, 5)
    else
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      let src = s:GetSrcLine(plnum)
      let pattern = '^.*\zs\V'.tokens[0]
      let i = 1
      while i < len(tokens) && src =~ pattern.'\.\*'.tokens[i]
        let pattern .= '\.\*'.tokens[i]
        let i += 1
      endwhile
      let tokens2 = GetCxxContextTokens(plnum-1, 5, substitute(src, pattern, '', ''))
    endif
    if count(tokens2, '(') == count(tokens2, ')')
      let plnum = s:GetPrevSrcLineMatching(plnum + 1, tokens2)
      call s:Info("align to indent of initializer list:", plnum, tokens2)
      return indent(plnum)
    elseif s:GetSrcLine(plnum) =~ '{\s*'.tokens[0]
      " might need to align to opening brace of initializer list
      let pline = substitute(s:GetSrcLine(plnum), '{\zs\s*'.tokens[0].'.*$', '', '')
      let pline = substitute(pline, '\t', repeat('.', &ts), 'g')
      call s:Info("align with opening brace of initializer list", pline)
      return strlen(pline) - 1
    endif
  "elseif tokens[-1] == '"' && current =~ '^\s*"' {{{2
  elseif tokens[-1] == '"' && current =~ '^\s*"'
    call s:Info("align string literals")
    let [plnum, line] = s:GetPrevSrcLine(lnum)
    let line = substitute(line, s:string_literal.'[^"]*$', '', '')
    let line = substitute(line, '\t', repeat('.', &ts), 'g')
    return strlen(line)
  " constructor initializer list? or class inheritance list? {{{2
  elseif tokens[-1] == ',' && tokens[-2] =~ '^\%(>>\|[>)}]\|'.s:identifier.'\)$' && index(tokens, ':') != -1
        \ && count(tokens, ':') > count(tokens, '?')
    let t = s:RemoveMatchingAngleBrackets(tokens)
    let i = len(t) - 1
    while i > 1 && t[i] == ','
      let i -= 1
      if t[i] =~ '^[)}]$'
        let i = s:IndexOfMatchingToken(t, i) - 1
      endif
      while i > 0 && t[i] =~ s:identifier_token && t[i-1] == '::'
        let i -= 2
      endwhile
      if t[i] !~ s:identifier_token
        let i = -1
        break
      elseif t[i-1] =~ '^p\%(ublic\|rivate\|rotected\)$'
        let i -= 2
      else
        let i -= 1
      endif
    endwhile
    if i > 0 && t[i] == ':'
      " yes
      let plnum = s:GetPrevSrcLineMatching(lnum, t[i:])
      call s:Info("align to ctor initializer list / class inheritance list", plnum, t[i:])
      return s:IndentForAlignment(plnum, '^\s*\zs.*:\s*\ze', t[i+1:])
    endif
  "elseif tokens[-1] =~ '^\%(inline\|static\|constexpr\|consteval\|explicit\|extern\|const\)$' {{{2
  elseif tokens[-1] =~ '^\%(inline\|static\|constexpr\|consteval\|explicit\|extern\|const\)$'
    call s:Info("indent like previous line")
    let [plnum, previous] = s:GetPrevSrcLine(lnum)
    return indent(plnum)
  " elseif s:LastTokensIsAttribute(tokens) {{{2
  elseif s:LastTokensIsAttribute(tokens) && s:WalkBackLambda(tokens, -1) == -1 && index(tokens, 'if') == -1
    call s:Info("indent like previous line after attribute")
    let [plnum, previous] = s:GetPrevSrcLine(lnum)
    return indent(plnum)
  "elseif tokens[-1] =~ '^[_A-Z][_A-Z0-9]*$' {{{2
  elseif tokens[-1] =~ '^[_A-Z][_A-Z0-9]*$'
    " maybe those are macros before a return type of a function decl/defn
    let i = len(tokens) - 2
    while i >= 0 && tokens[i] =~ '^[_A-Z][_A-Z0-9]*$'
      let i -= 1
    endwhile
    if i == -1 || tokens[i] =~ '^\%(inline\|static\|constexpr\|consteval\|explicit\|extern\|const\)$'
      call s:Info("no indent assuming return type of function decl/defn")
      return indent(s:GetPrevSrcLineMatching(lnum, tokens)) + s:TemplateIndent(tokens)
    endif
  endif
  let ctokens = s:CxxTokenize(current) "{{{2
  if empty(ctokens)
    let ctokens = ['']
  endif
  call s:Debug("current tokens:", ctokens)
  "after function defn/decl: keywords, trailing return type, and ref-qual {{{2
  if ctokens[0] =~ '^\%(const\|requires\|noexcept\|override\|final\|->\|&&\?\)$' &&
      \ funDeclEnd > 0 && index(tokens[funDeclEnd:-1], ':') == -1
    let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
    call s:Info("indent after function declaration/definition")
    return indent(plnum) + s:TemplateIndent(tokens)
  endif
  let align_to_identifier_before_opening_token = [0, -1] "{{{2
  " indent inside for parenthesis "{{{2
  if tokens[-1] == ';' && tokens[0] == 'for'
      \ && s:IndexOfMatchingToken(tokens, ')') == 1
    " inside for
    let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
    call s:Info("align to code inside for parenthesis")
    return s:IndentForAlignment(plnum, '^\s*\zs.*\<for\s*(\s*\ze', tokens[2:])
  "elseif ctokens[0] =~ '^[>)}\]]>\?$' {{{2
  elseif ctokens[0] =~ '^[>)}\]]>\?$'
    let i = 0
    while len(ctokens) > i+1 && ctokens[i][0] == ctokens[i+1]
      let i += 1
    endwhile
    let i = s:IndexOfMatchingToken(tokens + ctokens[:i], len(tokens) + i)
    if i != -1
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens[i:])
      let pline = s:GetSrcLine(plnum)
      let j = i
      while j < len(tokens) && pline =~ '\V'.join(tokens[i:j], '\.\*')
        let j += 1
      endwhile
      if i == j-1
        let align_to_identifier_before_opening_token = [1, i-len(tokens)]
        call s:Debug("set align_to_identifier_before_opening_token: ", align_to_identifier_before_opening_token)
      else
        call s:Info("align closing", ctokens[0], "to opening token", tokens[i], tokens[i:j-1], i, j)
        return s:IndentForAlignment(plnum, '\V\^\s\*\zs\.\*\ze'.join(tokens[i:j-1], '\.\*'))
      endif
    endif
  "elseif tokens[-1] =~ '^[{(<\[]$' {{{2
  elseif tokens[-1] =~ '^[{(<\[]$'
    let align_to_identifier_before_opening_token = [1, -1]
  "alignment inside argument & initializer lists {{{2
  elseif tokens[-1] == ','
      \ || (ctokens[0] =~ s:operators2_token && ctokens[0] != '(')
      \ || (tokens[-1] =~ s:operators2_token && (tokens[-1] !~ '^\%(]\|)\|>>\?\)$' ||
        \ s:IndexOfMatchingToken(tokens, -1) == -1))
    let i = s:IndexOfMatchingToken(tokens, len(tokens))
    call s:Debug("consider alignment inside argument list; last opening token:", i, tokens[i])
    while i > 0 && tokens[i] == '<' && i < len(tokens)-1 "{{{
      " determine whether this '<' token is a less-than operator or an opening
      " bracket of a template parameter/argument list
      " 1. if the preceding token is not an identifier we assume a less-than
      "    operator...
      " 2. ... except if it's an ']', in which case we might be looking at the
      "    template-parameter-list after a lambda-introducer. But it could
      "    also be e.g `data[0] < 0`, where it's a less-than operator
      " 3. ... or except if it's e.g. operator+<T>, in which case tokens[i-2] == 'operator'
      if tokens[i-1] !~ s:identifier_token && tokens[i-1] != ']' && !(i >= 2 && tokens[i-2] == 'operator')
        " tokens[i] is a less-than operator
        let i = s:IndexOfMatchingToken(tokens[:i-1], i)
      elseif s:GetSrcLine(lnum-1) =~ '\V\<'.join(tokens[i-1:i+1], ' ')
        " In addition, if there's a space before and after the '<' we assume less-than.
        call s:Debug("less-than: ", tokens[i-1:i+1]);
        let i = s:IndexOfMatchingToken(tokens[:i-1], i)
      else
        " try harder by looking for the matching closing token in ctokens
        "let j = s:IndexOfMatchingToken(['<'] + ctokens, 0)
        "if j != -1
        "  tokens[i] is an opening angle bracket
        "endif
        " try harder by looking for the next opening/closing token in ctokens
        let j = s:Index(ctokens, '^\%([(){}<>\[\]]\|>>\)$')
        while j != -1 && ctokens[j] =~ '[({<\[]'
          let j = s:IndexOfMatchingToken(ctokens, j)
          if j == -1
            break
          elseif ctokens[j] == '>>' && s:IndexOfMatchingToken(ctokens, j) == -1
            break
          endif
          let j = s:Index(ctokens, '^\%([(){}<>\[\]]\|>>\)$', j+1)
        endwhile
        if j != -1 && ctokens[j] =~ '^[)}\]]$'
          " tokens[i] is a less-than operator
          let i = s:IndexOfMatchingToken(tokens[:i-1], i)
        else
          " if j == -1: give up, we'll have to assume tokens[i] is an opening
          " angle bracket
          " else: ctokens[j] is > or >> and thus we assume tokens[i] is an
          " opening angle bracket
          break
        endif
      endif
    endwhile "}}}
    if i == -1 "{{{
      if tokens[-1] == ','
        " this can happen in initializer lists, because everything up to the opening
        " brace is removed from the context tokens
        " plnum will be >=0 if it's an initializer list
        let tok1 = ['{']
        let tok2 = tokens
        let plnum = s:GetPrevSrcLineMatching(lnum, '\V{\s\*'.join(tokens, '\.\*'))
        call s:Debug("is this an initializer list?", plnum)
      else
        let plnum = -1
      endif
    else
      let plnum = 0
      if tokens[-1] != ',' "{{{
        " search for a relevant , between tokens[i] and tokens[-1]
        let comma = -1
        let assignment = -1
        let j = i + 1
        let depth = 0
        while j < len(tokens)
          if tokens[j] =~ '^[({<\[]$'
            let j = s:IndexOfMatchingToken(tokens, j) + 1
            if j == 0
              break
            endif
          else
            if tokens[j] =~ s:assignment_op_token
              let assignment = j
            elseif tokens[j] == ','
              let comma = j
            endif
            let j += 1
          endif
        endwhile
        if assignment > comma
          " we want alignment after assignment op
          let plnum = -1
        endif
      endif "}}}
      if plnum != -1
        let tok1 = tokens[:i]
        let tok2 = tokens[i+1:]
        let plnum = s:GetPrevSrcLineMatching(lnum, tokens[i:])
        if plnum != s:GetPrevSrcLineMatching(lnum, tok2)
          let plnum = -1
          let align_to_identifier_before_opening_token = [1, i-len(tokens)]
          call s:Debug("set align_to_identifier_before_opening_token: ", align_to_identifier_before_opening_token)
        endif
      endif
    endif "}}}
    if plnum >= 0 "{{{
      let previous = s:GetSrcLineDropContinuations(plnum)
      let first = 0
      let last = -1
      if tok1[-1] == '<'
        let sep = '\[^(<\[]\*'
      else
        let sep = '\[^(\[]\*'
      endif
      let pat1 = join(tok1, sep)
      let pat2 = join(tok2, sep)
      while previous !~ '\V'.pat2
        let last -= 1
        let pat2 = join(tok2[:last], sep)
      endwhile
      while previous !~ '\V'.pat1.'\.\*'.pat2
        let first += 1
        let pat1 = join(tok1[first:], sep)
      endwhile
      " If the preceding line ends with { as its last token then don't take
      " the first branch
      if !empty(pat2) && !(tok1[-1] == '{' && previous[-1:] == '{')
        call s:Debug("alignment context:", plnum, previous, "tok1:", tok1, "pat1:", pat1, "tok2:", tok2, "pat2:", pat2)
        let previous = getline(plnum)
        let previous = substitute(previous, '\V'.pat1.'\s\*\zs\.\*'.pat2.'\.\*\$', '', '')
        let previous = substitute(previous, '\t', repeat('.', &ts), 'g')
        let extra_indent = 0
        if ctokens[0] =~ '^[})>\]]'
          let extra_indent = -1
        elseif tokens[-1] != ','
          let extra_indent += shiftwidth() * (ctokens[0] =~ s:operators2_token)
        endif
        call s:Info('align to opening '.tok1[-1].': '''.previous."'", plnum, tok2, ctokens[0])
        return strlen(previous) + extra_indent
      elseif tok1[-1] == '{'
        let plnum = s:GetPrevSrcLineMatching(lnum, tok2)
        call s:Info('align in initializer list', plnum, tok2)
        return indent(plnum) - shiftwidth() * (ctokens[0] == '}')
      endif
    endif "}}}
  endif
  let lambda_idx = s:WalkBackLambda(tokens, -1)
  if lambda_idx != -1
    let i = 0
    let moretokens = []
    while i < len(ctokens) && ctokens[i] != '{'
      let j = s:IndexOfMatchingToken(ctokens, i)
      if j == -1
        if ctokens[i] == '('
          let moretokens = ['(', ')']
          break
        elseif ctokens[i] == '[' && ctokens[i+1] == '['
          let moretokens = ['[', '[', ']', ']']
          break
        elseif ctokens[i] == '<'
          let moretokens = ['<', '>']
          break
        endif
        let i += 1
      else
        let i = j + 1
      endif
    endwhile
    if lambda_idx == s:WalkBackLambda(tokens + ctokens[:i - len(ctokens) - 1] + moretokens, -1)
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens[lambda_idx:])
      call s:Info("indenting lambda-expression before its compound-statement")
      return s:IndentForAlignment(plnum, '^\s*\zs.*\ze', tokens[lambda_idx:]) + shiftwidth()
    else
      call s:Debug("not a lambda-expression", i, ctokens)
    endif
  endif
  "if tokens[-1] =~ '^[{(<]$' {{{2
  if ctokens[0] == '(' && tokens[-1] != '{'
    let align_to_identifier_before_opening_token = [1, 0]
  endif
  let base_indent = 0
  let base_indent_type = "none"
  let indent_offset = 0
  let tokens = s:RemoveControlTokens(tokens)
  if align_to_identifier_before_opening_token[0] "{{{2
    "call s:Debug("considering align_to_identifier_before_opening_token", align_to_identifier_before_opening_token)
    let j = len(tokens) + align_to_identifier_before_opening_token[1]
    let is_closing = ctokens[0] =~ '^[>)\]]>\?$' &&
        \ s:IndexOfMatchingToken(tokens, ctokens[0]) == j
    let i = index(reverse(tokens[j+1:]), ',')
    let on_empty = -1
    if !is_closing && i > 0
      let i = len(tokens) - i
      let base_indent_type = "align to identifier after last comma after opening ".tokens[j]
      let extra_indent = 0
    else
      let i = j - 1
      while i > 0
        call s:Debug("looking at", tokens[i], "with i ==", i)
        if tokens[i] =~ '^[)>}\]]$' || tokens[i] == '>>'
          let i = s:IndexOfMatchingToken(tokens, i) - 1
        elseif tokens[i] == '('
          if tokens[i+1] == '[' && tokens[i-1] == '}' && on_empty == -1
            " this looks like a directly called, unnamed lambda
            " align to the opening [ of the lambda body
            let on_empty = i + 1
          endif
          let i -= 1
        elseif tokens[i - 1] == 'template'
          let i -= 1
        elseif i > 1 && tokens[i - 1] == '::' && tokens[i - 2] =~ '^>>\?$'
          let i = s:IndexOfMatchingToken(tokens, i - 2) - 1
        elseif tokens[i - 1] =~ '::\|\.\|->'
          let i -= 1 + (i > 1 && tokens[i - 2] =~ '^\%(\i\+\|[)>]\)$')
        elseif tokens[i] == ','
          let nexti = s:IndexOfMatchingToken(tokens[:i-1], i) - 1
          call s:Debug("considering a jump to", tokens[nexti])
          if tokens[i - 1] == '}' && tokens[j] == '{' &&
              \ s:GetPrevSrcLineMatching(lnum, tokens[nexti:]) !=
              \ s:GetPrevSrcLineMatching(lnum, tokens[i:])
            let i -= 1
            break
          else
            call s:Debug("jumping back from , over opening token", tokens[nexti+1], "to", tokens[nexti])
            let i = nexti
          endif
        elseif tokens[i - 1] =~ '^[)>}\]]$'
          let i = s:IndexOfMatchingToken(tokens, i - 1) - 1
        elseif tokens[i] == '?' || (tokens[i] == ':' && tokens[i - 1] == '?')
          let i += 1
          break
        elseif tokens[i - 1] == 'typename'
          let i -= 1
          break
        else
          break
        endif
      endwhile
      if i < 0 && on_empty != -1
        let i = on_empty
      endif
      let extra_indent = shiftwidth()
      if align_to_identifier_before_opening_token[1] == 0
        let base_indent_type = "1 shiftwidth behind preceding identifier"
      else
        let base_indent_type = "1 shiftwidth behind identifier/expression '".join(tokens[i:j-1])."' before opening ".tokens[j]
      endif
      call s:Debug("set up", base_indent_type)
    endif
    let plnum = s:GetPrevSrcLineMatching(lnum, tokens[i:])
    let base_indent = s:IndentForAlignment(plnum, '^\s*\zs.*\ze', tokens[i:])
    if is_closing
      call s:Info("align with identifier before opening", tokens[j], base_indent, plnum, i, tokens[i:])
      return base_indent
    endif
    return s:AlignedIndent(base_indent_type, base_indent + extra_indent, indent_offset, tokens, ctokens, align_to_identifier_before_opening_token)
  elseif tokens == ['/*'] "{{{2
    call s:Info("using cindent inside /*...*/ comment")
    return cindent(lnum)
  else "{{{2
    let i = s:Index(tokens, s:indent_op_token)
    " search after matching token as long as tokens[i] has a matching token: (), [], {}, and <>
    " also skip if '*' or '&' are part of a type or a unary operator (which we can't determine so we decide via whitespace:
    " if the token has whitespace on both sides, it's a binary operator, otherwise it's a unary operator or part of a type)
    while i != -1
      if tokens[i] =~ '^[({\[<]$'
        let j = s:IndexOfMatchingToken(tokens, i)
        if j == -1
          if tokens[i] == '<' && tokens[i-1] == 'template'
            let i = s:Index(tokens, s:indent_op_token, i + 1)
            continue
          endif
          break
        endif
        "s:Debug("restart search at", j+1, tokens[j+1], "found", i)
        let i = s:Index(tokens, s:indent_op_token, j + 1)
      elseif tokens[i] =~ '^[&*+-]$' && (i == 0 || i == len(tokens) - 1
          \ || (i > 0 && tokens[i-1] =~ '^\%([([{<]\|'.s:assignment_operators.'\|'.s:compare_operators.'\)$'))
        " unary operator => skip it
        let i = s:Index(tokens, s:indent_op_token, i + 1)
      elseif i > 0 && tokens[i] =~ '^[&*]$' && tokens[i-1] =~ s:identifier_token
        let tok_lnum = s:GetPrevSrcLineMatching(lnum, tokens[i-1:])
        let tok_src = s:GetSrcLine(tok_lnum)
        if tok_src =~ '\V'.tokens[i-1].tokens[i] || tok_src =~ '\V'.tokens[i].tokens[i+1]
          " no space before or after tokens[i] => skip tokens[i]
          let i = s:Index(tokens, s:indent_op_token, i + 1)
        else
          break
        endif
      else
        break
      endif
    endwhile
    if i > 0 && i < len(tokens) - 1 && tokens[i] =~ s:assignment_op_token
      let oplnum = s:GetPrevSrcLineMatching(lnum, tokens[i:])
      return s:AlignedIndent("align to assignment operator",
            \ s:IndentForAlignment(oplnum, '^\s*\zs.*'.tokens[i].'\s*\ze', tokens[i+1:]),
            \ 0,
            \ tokens, ctokens, align_to_identifier_before_opening_token)
    elseif tokens[0] == 'return'
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      return s:AlignedIndent("align with return",
            \ indent(plnum) + max([shiftwidth(), strlen(matchstr(s:GetSrcLine(plnum), 'return\s\+'))]),
            \ 0,
            \ tokens, ctokens, align_to_identifier_before_opening_token)
    elseif i > 0 && i < len(tokens) - 1 && tokens[i] != ':'
      " exclude aligning to ':' in bitfield decls
      let oplnum = s:GetPrevSrcLineMatching(lnum, tokens[i:])
      call s:Info("align with operator on preceding line", i, tokens[i])
      return s:IndentForAlignment(oplnum, '^\s*\zs.*\ze', tokens[i:])
    elseif tokens[0] == 'template'
      " if not aligned, add 1 shiftwidth for templates
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      return s:AlignedIndent("1 shiftwidth behind template",
            \ indent(plnum) + s:TemplateIndent(tokens), 0,
            \ tokens, ctokens, align_to_identifier_before_opening_token)
    endif
  endif
  let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
  return s:AlignedIndent("same base indent as first line of statement",
        \ indent(plnum), shiftwidth(),
        \ tokens, ctokens, align_to_identifier_before_opening_token)
endfunction "}}}1

" vim: foldmethod=marker foldmarker={{{,}}} sw=2 et
