nmap <F5> :source ~/.vim/pack/mattkretz/start/vim-gnuindent/plugin/gnuindent.vim<CR>:echo GetCxxContextTokens(line('.')-1, 20)<CR>
command! SetupGnuIndent setlocal indentexpr=GnuIndent() indentkeys=:,0#,!^F,o,O,e,(,0),<>>,0<lt>
 \0a,0b,0c,0d,0<e>,0f,0g,0h,0i,0j,0k,0l,0m,0n,0<o>,0p,0q,0r,0s,0t,0u,0v,0w,0x,0y,0z,
 \0A,0B,0C,0D,0E,0F,0G,0H,0I,0J,0K,0L,0M,0N,0<O>,0P,0Q,0R,0S,0T,0U,0V,0W,0X,0Y,0Z,
 \0~,0<!>,0%,0^,0&,0<*>,0-,0_,0+,0=,0{,0},0[,0],0;,0",0',0.,0?,0/,0<0>,01,02,03,04,05,06,07,08,09,0<:>,0<Bar>

function! s:Info(...) "{{{1
  echohl MoreMsg
  echom "GnuIndent:" join(a:000)
  echohl None
endfunction

function! s:Debug(...) "{{{1
  "echohl Debug | echom join(a:000) | echohl None
endfunction "}}}1

" Returns the index of the token matching tokens[start]. If start equals len(tokens)
" then a matching preceding opening token is searched for.
function! s:IndexOfMatchingToken(tokens, start) "{{{1
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
      let depth[depth_idx[a:tokens[i]]] = 1 + (a:tokens[i] == '>>')
      if a:tokens[i] =~ '^[<{(\[]$'
        let direction = 1
      elseif a:tokens[i] =~ '^[>})\]]$' || a:tokens[i] == '>>'
        let direction = -1
      endif
    endif
  endif
  let maybe_shift = []
  "call s:Debug("IndexOfMatchingToken", a:tokens, a:start, depth, direction)
  if direction != 0
    let i += direction
    while i < n && i >= 0
      if a:tokens[i] == '<'
        while !empty(maybe_shift)
          "call s:Debug("maybe_shift:", maybe_shift, "depth:", depth)
          let was_closing = maybe_shift[-1] == depth
          let maybe_shift = maybe_shift[:-2]
          if was_closing
            let depth[0] += 2
            break
          endif
        endwhile
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
          let maybe_shift += [depth]
        elseif depth[0] >= 2
          let depth[depth_idx[a:tokens[i]]] -= 2
        elseif depth[1:3] == [0, 0, 0]
          return i
        endif
      endif
      if depth == [0, 0, 0, 0, 0] || (depth[4] == 1 && sort(depth[:3]) == [-1, 0, 0, 0])
        "call s:Debug("IndexOfMatchingToken", a:tokens, a:start, "returns", i)
        return i
      endif
      let i += direction
    endwhile
  endif
  "call s:Debug("IndexOfMatchingToken", a:tokens, a:start, "returns -1")
  return -1
endfunction

function! s:Index(tokens, pattern, ...) "{{{1
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

" lnum: line number where to align to
" pattern: The match string determines the number of columns to indent
"   additionally to the indent of the referenced line
function! s:IndentForAlignment(lnum, pattern, ...) "{{{1
  let s = getline(a:lnum)
  let s = substitute(s, '/\*\zs.\{-}\ze\*/', {m -> repeat(' ', strlen(m[0]))}, 'g')
  let s = substitute(s, '//\zs.*$', '', '')
  "call s:Debug("'".s."'")
  let p = a:pattern
  if a:0 == 1
    let i = 0
    while len(a:1) > i && s =~ p.'\V'.join(a:1[:i], '\.\*')
      let i += 1
    endwhile
    if i > 0
      let p .= '\V'.join(a:1[:i-1], '\.\*')
    endif
  endif
  return indent(a:lnum) + strlen(matchstr(s, p))
endfunction

function! s:GetPrevSrcLine(lnum) "{{{1
  let plnum = a:lnum - 1
  let line = s:GetSrcLine(plnum)
  " ignore preprocessor directives and labels
  while (empty(line) || line == '*/' || line =~ '^\s*#' || line =~ '^\s*\i\+\s*::\@!') && plnum > 1
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
let s:decfloat_literal = '\%(\%('.s:fractional_constant.s:exponent_part.'\?\|'
  \ .s:digit_seq.s:exponent_part.'\)[fFlL]\?\)'
let s:hexfloat_literal = '\%(0[xX]\%('.s:hex_fractional_constant.'\|'.s:hex_digit_seq.'\)'
  \ .s:bin_exponent_part.'[fFlL]\?\)'
let s:float_literal = '[+-]\?\%('.s:decfloat_literal.'\|'.s:hexfloat_literal.'\)'
let s:encoding_prefix_opt = '\%(u8\|[uUL]\)\?'
let s:escape_sequence = '\\[''"?\\abfnrtv]\|\\\o\{1,3}\|\\x\x\x\?'
let s:char_literal = s:encoding_prefix_opt.'''\%([^''\\]\|'.s:escape_sequence.'\)'''
let s:string_literal = s:encoding_prefix_opt.'\%("\%([^"\\]\|\\[uU]\%(\x\{4}\)\{1,2}\|'.s:escape_sequence.'\)*"\|R"\([^(]*\)(.\{-})\1"\)'
let s:operators2 = '->\*\?\|\.\.\.\|\.\*\?\|[()\[\]]\|'.'++\|--\|&&\|||\|<=>\|[|&^!=<>*/%+-]=\|<<\|>>\|::\|[~,:?|&^!=<>*/%+-]'
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

function! s:CxxTokenize(context) "{{{1
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
  let context = substitute(context, pattern2, '', '')
  let m = matchstrpos(context, pattern)
  "call s:Debug("            next:", context, m)
  while m[1] != -1
    if       count(m[0], '(') != count(m[0], ')')
        \ || count(m[0], '{') != count(m[0], '}')
        \ || count(m[0], '[') != count(m[0], ']')
        \ || m[2] - m[1] < 2
        \ || m[0] =~ '^[ '.a:to_keep.']*$'
      let m = matchstrpos(context, pattern, m[2])
      "call s:Debug("   skipped. next:", context, m)
    else
      let left = context[:m[1]][:-2]
      let right = context[m[2]:]
      let context = left.substitute(m[0], '[^'.a:to_keep.']\+', ' ', 'g').right
      let m = matchstrpos(context, pattern)
      "call s:Debug("simplified. next:", context, m)
    endif
  endwhile
  return context
endfunction

function! GetCxxContextTokens(from, n, ...) "{{{1
  let context = join(a:000)
  let nextline = a:from
  let n = 0
  while nextline > 0 && (n < a:n || strlen(context) < a:n * &tw) "{{{2
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
      continue
    endif
    " trim whitespace and drop // comments
    let s = substitute(s, '^\s*\(.\{-}\)\s*\(//.*\)\?$', '\1', '')
    if empty(s)
      continue
    endif
    let n += 1
    let context = s . ' ' . context
  endwhile

  " get more context to see what the last closing brace belongs to {{{2
  if count(context, '{') < count(context, '}')
    let need_more = 1
    while nextline > 0 && need_more
      let s = getline(nextline)
      let nextline -= 1
      while getline(nextline)[-1:] == '\'
        let s = getline(nextline)[:-2] . s
        let nextline -= 1
      endwhile
      " ignore preprocessor directives and labels
      if s =~ '^\s*#' || s =~ '^\s*\i\+\s*::\@!'
        continue
      endif
      " trim whitespace and drop // comments
      let s = substitute(s, '^\s*\(.\{-}\)\s*\(//.*\)\?$', '\1', '')
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
  endif

  " go back beyond the start of an intializer list {{{2
  while nextline > 0 && count(context, ';') == 0 && count(context, '{') <= count(context, '}')
    let s = getline(nextline)
    let nextline -= 1
    while getline(nextline)[-1:] == '\'
      let s = getline(nextline)[:-2] . s
      let nextline -= 1
    endwhile
    " ignore preprocessor directives and labels
    if s =~ '^\s*#' || s =~ '^\s*\i\+\s*::\@!'
      continue
    endif
    " trim whitespace and drop // comments
    let s = substitute(s, '^\s*\(.\{-}\)\s*\(//.*\)\?$', '\1', '')
    if !empty(s)
      let context = s . ' ' . context
    endif
  endwhile

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
  let context = substitute(context, '>=', '$`ge`', 'g')
  let context = substitute(context, '<=', '$`le`', 'g')
  let context = substitute(context, '<<', '$`sl`', 'g')
  let context = substitute(context, '>>=', '$`sra`', 'g')

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
  let context = substitute(context, '<\zs[^<>]\+\ze>', '', 'g')
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

  " simplify `if constexpr` to `if` {{{2
  let context = substitute(context, '\<if\s*constexpr\>', 'if', 'g')

  " remove matching brackets, because they are a headache for GetPrevSrcLineMatching {{{2
  let pattern = '\[[^\[\]]*\]'
  while context =~ pattern
    let context = substitute(context, pattern, ' ', 'g')
    "call s:Debug(context)
  endwhile

  " simplify conditional expressions `a?b:c` to `a c` {{{2
  let context = substitute(context, '\s*?\%([^?:]\|::\)*::\@!\s*', ' ?: ', 'g')

  " tokenize {{{2
  let tokens = s:CxxTokenize(context)
  if len(tokens) <= 1
    return tokens
  endif
  "call s:Debug(tokens)

  " keep only tokens after last block {...}, last ';', or last unmatched '{', but: {{{2
  " 1. don't make the list empty
  " 2. don't consider blocks inside parens (...)
  " 3. don't delete anything inside an open for(...;...;
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
        "call remove(tokens, index(tokens, ';')+1, for_idx-2)
        let idx1 = index(tokens, ';', index(tokens, 'for', 1))
      endif
    endif
  endif
  let idx2 = index(tokens, '{', 1)
  let idx3 = index(tokens, '}', 1 + (tokens[0] == ';'))
  while idx3 != -1 && (count(tokens[:idx3], ')') > count(tokens[:idx3], '(')
      \ || tokens[idx3-1] == '->' || tokens[idx3-1] == ',')
    let idx3 = index(tokens, '}', idx3 + 1)
  endwhile
  let idx3 = -1 - idx3
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
    "let tokens = tokens[len(tokens) - min([idx1, idx2]):]
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
      while open_idx > 2 && tokens[open_idx-1] =~ s:identifier_token && tokens[open_idx-2] =~ '^[,:]'
        " skip if it matches `, foo{x}` or `: foo{x}`, which can occur right
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
  if a:tokens[0] == 'else' && len(a:tokens) > 1
    if a:tokens[1] != 'if'
      return a:tokens[1:]
    else
      let paren_idx = 2
    endif
  elseif a:tokens[0] =~ '^\%(if\|for\|while\)$'
    let paren_idx = 1
  endif
  if paren_idx > 0 && len(a:tokens) > paren_idx && a:tokens[paren_idx] == '('
    let closing_idx = s:IndexOfMatchingToken(a:tokens, paren_idx)
    if closing_idx != -1 && closing_idx + 1 < len(a:tokens)
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
  if type(a:pattern) == v:t_list
    let pattern = '\V'.join(a:pattern, '\.\*')
  else
    let pattern = a:pattern
  endif
  let [lnum, line] = s:GetPrevSrcLine(a:lnum)
  let context = line
  while context !~ pattern
    if lnum <= 1
      return -1
    endif
    let [lnum, line] = s:GetPrevSrcLine(lnum)
    let context = line.context
  endwhile
  return lnum
endfunction

function! s:TemplateIndent(tokens) "{{{1
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

function! s:IsFunctionDeclDefn(tokens) "{{{1
  let i = 0
  let n = len(a:tokens)
  " template head(s) with requires clauses
  while a:tokens[i] == 'template' && a:tokens[i+1] == '<'
    let i = s:IndexOfMatchingToken(a:tokens, i+1) + 1
    if a:tokens[i] == 'requires'
      let i += 1
      if a:tokens[i] == '('
        let i = s:IndexOfMatchingToken(a:tokens, i) + 1
      else
        if a:tokens[i] =~ s:identifier_token
          let i += 1
        endif
        while a:tokens[i] == '::' && a:tokens[i+1] =~ s:identifier_token
          let i += 2
        endwhile
        if a:tokens[i] != '<'
          return 0
        endif
        let i = s:IndexOfMatchingToken(a:tokens, i) + 1
      endif
    endif
    if i <= 0 || i >= n
      return 0
    endif
  endwhile
  " return type and stuff
  let n_identifiers = 0
  while 1
    if a:tokens[i] == 'decltype' && a:tokens[i+1] == '('
      let i = s:IndexOfMatchingToken(a:tokens, i+1) + 1
      let n_identifiers += 1
    elseif a:tokens[i] == 'operator' &&
        \ a:tokens[i+1] =~ '^\%(?:\|<=>\|&&\|||\|<<\|>>\|\[\]\|()\|++\|--\|[<>~!%^&*=|,+-]=\?\)$' &&
        \ a:tokens[i+2] == '(' && n_identifiers > 0
      return 1
    elseif a:tokens[i] =~ s:identifier_token
      if a:tokens[i] !~ '^\%(if\|else\|for\|while\|do\|return\|sizeof\|alignof\|constexpr\|consteval\|static\|inline\|extern\|template\|class\|struct\|requires\|noexcept\|typename\|using\|typedef\|concept\|const\|operator\|namespace\)$'
        let n_identifiers += 1
      endif
      let i += 1
    elseif a:tokens[i] =~ '^\%(::\|\*\|&&\?\)$'
      let i += 1
    elseif a:tokens[i] == '<'
      let i = s:IndexOfMatchingToken(a:tokens, i) + 1
    else
      break
    endif
    if i <= 0 || i >= n
      return 0
    endif
  endwhile
  if a:tokens[i] != '(' || n_identifiers < 1
    return 0
  endif
  let i = s:IndexOfMatchingToken(a:tokens, i)
  return i > 0
endfunction

function! GnuIndent(...) "{{{1
  if a:0 == 0
    let lnum = v:lnum
  elseif type(a:1) == v:t_string
    let lnum = line(a:1)
  else
    let lnum = a:1
  endif
  let current = s:GetSrcLine(lnum)
  let is_access_specifier = 0
  "if current =~ '^\s*#' || current =~ '^\s*\i\+\s*::\@!' {{{2
  if current =~ '^\s*#' || current =~ '^\s*\i\+\s*::\@!'
    if current =~ '^\s*\%(public\|private\|protected\|signals\|slots\|Q_SIGNALS\|Q_SLOTS\)\s*:'
      let is_access_specifier = 1
    else
      call s:Info("preprocessor or label")
      return cindent(lnum)
    endif
  endif
  let tokens = GetCxxContextTokens(lnum-1, 20) "{{{2
  call s:Debug(tokens)
  if empty(tokens) "{{{2
    call s:Info("no preceding code")
    return 0
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
  " if-else / loops {{{2
  if tokens[0] =~ '^\%(if\|else\|do\|for\|while\)$' &&
      \ tokens[-1] =~ '^\%(else\|do\|)\)$'
    let depth = 0
    let i = 0
    while i < len(tokens) && tokens[i] =~ '^\%(if\|for\|while\|else\|do\)$'
      if tokens[i] == 'else' || tokens[i] == 'do'
        if i+1 == len(tokens) || tokens[i+1] != 'if'
          let depth += 1
        endif
        let i += 1
      elseif tokens[i+1] == '('
        let i = s:IndexOfMatchingToken(tokens, i + 1)
        if i != -1
          let depth += 1
          let i += 1
          call s:Debug("condblock depth:", depth, i)
        endif
      else
        call s:Debug("parse error in if/for/while")
        break
      endif
    endwhile
    let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
    if depth > 0
      call s:Info("extra indent for condblock", depth)
      return indent(plnum) + shiftwidth() * depth
    endif
  endif
  "start of {} block (current =~ '^\s*{') {{{2
  if current =~ '^\s*{'
    if tokens[-1] == '{'
      let plnum = s:GetPrevSrcLineMatching(lnum, ['{'])
      call s:Info("indent block in a block", plnum)
      return indent(plnum) + shiftwidth()
    elseif tokens[-1] !~ '^[,(]$' " not a condblock
      call s:Info("align block indent to preceding statement")
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      return indent(plnum) + s:TemplateIndent(tokens)
    endif
    s:Debug("fall through from block indent section")
  "elseif tokens[-1] == '}' && tokens[s:IndexOfMatchingToken(tokens, -1) - 1] != ',' {{{2
  elseif tokens[-1] == '}' && tokens[s:IndexOfMatchingToken(tokens, -1) - 1] != ','
    let plnum = s:GetPrevSrcLineMatching(lnum, '\V\^\s\*'.join(tokens, '\.\*'))
    call s:Info("indent after block", plnum)
    return indent(plnum) + shiftwidth() * ((current =~ '^\s*->') - (current =~ '^\s*}') - is_access_specifier)
    "- shiftwidth() * (tokens[0] =~ 'template\|if\|else\|for\|while\|do')
  elseif tokens == ['}'] "{{{2
    " not enough context from GetContextTokens => fall back to cindent
    call s:Info("use cindent because of distant context")
    return cindent('.')
  "elseif tokens[-2:] == ['{', '}'] && tokens[-3] =~ 'if\|else\|for\|while\|do' {{{2
  elseif tokens[-2:] == ['{', '}'] && tokens[-3] =~ 'if\|else\|for\|while\|do'
    call s:Info("indent after condblock")
    let plnum = search('^\s*}', 'bnWz', 0, 20)
    return indent(plnum) - shiftwidth()
  "elseif current =~ '^\s*::\@!' {{{2
  elseif current =~ '^\s*::\@!'
    if tokens[-1] =~ '^[)>]>\?$'
      let i = s:IndexOfMatchingToken(tokens, -1) - 1
    else
      let i = len(tokens) - 1
    endif
    if tokens[i] == 'noexcept' && tokens[i-1] == ')'
      let i = s:IndexOfMatchingToken(tokens, i-1) - 1
    endif
    if i >= 0 && tokens[i] =~ s:identifier_token &&
        \ ((tokens[-1] =~ '^\%(noexcept\|)\)$' && (i == 0 || tokens[i-1] =~ '^\%('.s:identifier.'\|[)>]\|>>\|::\)$')) ||
        \  tokens[i-1] =~ '^\%(class\|struct\)$')
      call s:Info("no indent for ctor initializer list or class base types")
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      return indent(plnum) + s:TemplateIndent(tokens)
    elseif count(tokens[:-2], '?') > count(tokens, ':')
      let i = -2
      while tokens[i] != '?' || tokens[i+1] == ':'
        let i -= 1
      endwhile
      let plnum = s:GetPrevSrcLineMatching(lnum,
          \ '\V\%('.tokens[i-1].'\.\*\|\^\s\*\)'.join(tokens[i:], '\.\*'))
      let previous = getline(plnum)
      let j = -1
      while j > i && previous !~ join(tokens[i:j], '[^?]*')
        let j -= 1
      endwhile
      let previous = substitute(previous, '\s*'.join(tokens[i+1:j], '[^?]*').'[^?]*$', '', '')
      let previous = substitute(previous, '\t', repeat('.', &ts), 'g')
      call s:Info("align : to corresponding ?", plnum, i, j)
      return strlen(previous) - 1
    endif
    call s:Debug("fall through from : rule", tokens[i], tokens[-1], i, tokens[i-1])
  "elseif function defn/decl {{{2
  elseif current =~ '^\s*\%('.s:identifier.'\|operator\s*\%(?:\|<=>\|&&\|||\|<<\|>>\|\[\]\|()\|++\|--\|[\[(<>~!%^&*=|,+-]=\?\)\)\s*('
      \ && tokens[-1] =~ s:identifier_token.'\|>>\|>\|&\|&&\|\*'
      \ && tokens[-1] !~ '^\%(else\|do\)$'
    call s:Info("function definition/declaration")
    let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
    return indent(plnum) + s:TemplateIndent(tokens)
  "elseif tokens[-1] == '{' {{{2
  elseif tokens[-1] == '{'
    if current =~ '^\s*}'
      call s:Info("indent closing brace of empty {} block")
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens[-1:])
      return indent(plnum)
    else
      " first indent inside a new {} block (ignoring labels)
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens[-1:])
      let previous = s:GetSrcLine(plnum)
      let ptok = s:CxxTokenize(previous)
      if previous =~ '^\s*{' || (
          \ count(ptok, '(') == count(ptok, ')') &&
          \ count(ptok, '[') == count(ptok, ']') &&
          \ count(ptok, '<') == count(ptok, '>'))
          "\ count(ptok, '{') - 1 == count(ptok, '}') &&
        call s:Info("indent inside a new {} block:")
        return indent(plnum) + shiftwidth() * (1 - is_access_specifier)
      endif
      call s:Debug("fall through to align_to_identifier_before_opening_token")
    endif
  elseif tokens[-1] == ';' && (tokens[0] != 'for' || s:IndexOfMatchingToken(tokens, 1) != -1) "{{{2
    let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
    call s:Info("align to indent of last statement:", plnum)
    if current =~ '^\s*}' || is_access_specifier
      return indent(plnum) - shiftwidth()
    else
      return indent(plnum)
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
      let tokens2 = GetCxxContextTokens(plnum-1, 5,
          \ substitute(s:GetSrcLine(plnum), '^.*\zs'.tokens[0].'.*$', '', ''))
    endif
    if count(tokens2, '(') == count(tokens2, ')')
      let plnum = s:GetPrevSrcLineMatching(plnum, tokens2)
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
  elseif tokens[-1] == ',' && tokens[-2] =~ '^\%(>>\|[>)}]\|'.s:identifier.'\)$' && index(tokens, ':') != -1 "{{{2
    " constructor initializer list? or class inheritance list?
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
      call s:Info("align to ctor initializer list / class inheritance list")
      let plnum = s:GetPrevSrcLineMatching(lnum, t[i:])
      return s:IndentForAlignment(plnum, '^\s*\zs.*:\s*\ze', t[i+1:])
    endif
  "elseif tokens[-1] =~ '^\%(inline\|static\|constexpr\|consteval\|explicit\|extern\|const\)$' {{{2
  elseif tokens[-1] =~ '^\%(inline\|static\|constexpr\|consteval\|explicit\|extern\|const\)$'
    call s:Info("indent like previous line")
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
  "after function defn/decl: keywords, trailing return type, and ref-qual {{{2
  if ctokens[0] =~ '^\%(const\|requires\|noexcept\|override\|final\|->\|&&\?\)$' &&
      \ s:IsFunctionDeclDefn(tokens)
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
      \ || (tokens[-1] =~ s:operators2_token && (tokens[-1] !~ '^[)>]>\?$' ||
        \ s:IndexOfMatchingToken(tokens, -1) == -1))
    "call s:Debug("consider alignment inside argument list")
    let i = s:IndexOfMatchingToken(tokens, len(tokens))
    while i > 0 && tokens[i] == '<'
      if tokens[i-1] !~ s:identifier_token
        " tokens[i] is a less-than operator
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
    endwhile
    if i == -1
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
      if tokens[-1] != ','
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
      endif
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
    endif
    if plnum >= 0
      let previous = getline(plnum)
      let first = 0
      let last = -1
      if tok1[-1] == '<'
        let sep = '\[^(<\[]\*'
      else
        let sep = '\[^(\[]\*'
      endif
      let pat1 = join(tok1, sep)
      let pat2 = join(tok2, sep)
      while previous !~ '\V'.pat1
        let first += 1
        let pat1 = join(tok1[first:], sep)
      endwhile
      while previous !~ '\V'.pat2
        let last -= 1
        let pat2 = join(tok2[:last], sep)
      endwhile
      if !empty(pat2)
        call s:Debug("alignment context:", plnum, previous, tok1, pat1, tok2, pat2)
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
    endif
  endif
  "if tokens[-1] =~ '^[{(<]$' {{{2
  if ctokens[0] == '('
    let align_to_identifier_before_opening_token = [1, 0]
  endif
  let base_indent = 0
  let base_indent_type = "none"
  let indent_offset = 0
  let tokens = s:RemoveControlTokens(tokens)
  if align_to_identifier_before_opening_token[0] "{{{2
    let j = len(tokens) + align_to_identifier_before_opening_token[1]
    let is_closing = ctokens[0] =~ '^[>)\]]>\?$' &&
        \ s:IndexOfMatchingToken(tokens, ctokens[0]) == j
    let i = index(reverse(tokens[j+1:]), ',')
    if !is_closing && i > 0
      let i = len(tokens) - i
      let base_indent_type = "align to identifier after last comma after opening ".tokens[j]
      let extra_indent = 0
    else
      let i = j - 1
      while i > 0
        call s:Debug("looking at", tokens[i])
        if tokens[i] =~ '^[)>}]'
          let i = s:IndexOfMatchingToken(tokens, i) - 1
        elseif tokens[i] == '('
          let i -= 1
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
            call s:Debug("jumping back from , over opening token", token[nexti+1], "to", token[nexti])
            let i = nexti
          endif
        else
          break
        endif
      endwhile
      let extra_indent = shiftwidth()
      if align_to_identifier_before_opening_token[1] == 0
        let base_indent_type = "1 shiftwidth behind preceding identifier"
      else
        let base_indent_type = "1 shiftwidth behind identifier before opening ".tokens[j]
      endif
      call s:Debug("set up", base_indent_type)
    endif
    let plnum = s:GetPrevSrcLineMatching(lnum, tokens[i:])
    let base_indent = s:IndentForAlignment(plnum, '^\s*\zs.*\ze', tokens[i:])
    if is_closing
      call s:Info("align with identifier before opening", tokens[j], base_indent, plnum, i, tokens[i:])
      return base_indent
    endif
    let base_indent += extra_indent
  elseif tokens == ['/*'] "{{{2
    return cindent(lnum)
  else "{{{2
    let i = s:Index(tokens, s:indent_op_token)
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
      else
        break
      endif
    endwhile
    if i > 0 && i < len(tokens) - 1 && tokens[i] =~ s:assignment_op_token
      let oplnum = s:GetPrevSrcLineMatching(lnum, tokens[i:])
      let base_indent = s:IndentForAlignment(oplnum, '^\s*\zs.*'.tokens[i].'\s*\ze', tokens[i+1:])
      let base_indent_type = "align to assignment operator"
    elseif tokens[0] == 'return'
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      let base_indent = indent(plnum) + max([shiftwidth(), strlen(matchstr(s:GetSrcLine(plnum), 'return\s\+'))])
      let base_indent_type = "align with return"
    elseif i > 0 && i < len(tokens) - 1
      let oplnum = s:GetPrevSrcLineMatching(lnum, tokens[i:])
      let base_indent = s:IndentForAlignment(oplnum, '^\s*\zs.*\ze', tokens[i:])
      call s:Info("align with operator on preceding line", i, tokens[i])
      return base_indent
    elseif tokens[0] == 'template'
      " if not aligned, add 1 shiftwidth for templates
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      let base_indent = indent(plnum) + s:TemplateIndent(tokens)
      let base_indent_type = "1 shiftwidth behind template"
      " add 1 shiftwidth per open scope
      " (this shouldn't happen: either align to opening scope or the preceding identifier)
      "let indent_offset += shiftwidth() * (
      "  \   count(tokens, '\[') + count(tokens, '(') + count(tokens, '<')
      "  \ - count(tokens, '\]') - count(tokens, ')') - count(tokens, '>')
      "  \ - 2*count(tokens, '>>'))
    else
      let plnum = s:GetPrevSrcLineMatching(lnum, tokens)
      let base_indent = indent(plnum)
      let base_indent_type = "same base indent as first line of statement"
      let indent_offset = shiftwidth()
    endif
  endif
  if indent_offset == 0 "{{{2
    let indent_offset = shiftwidth() * ((
        \   ctokens[0] =~ s:operators2_token &&
        \   ctokens[0] !~ '^[\[({})\]]$'
        \ ) || (
        \   tokens[-1] =~ s:operators2_token &&
        \   tokens[-1] !~ '^\%(>>\|[\[\]()<>,]\)$'
        \ ))
  endif
  call s:Info(base_indent_type, base_indent, indent_offset, ctokens, align_to_identifier_before_opening_token)
  return base_indent + indent_offset
endfunction "}}}1

" vim: foldmethod=marker foldmarker={{{,}}} sw=2 et
