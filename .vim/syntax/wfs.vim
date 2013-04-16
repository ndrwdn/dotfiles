" Vim syntax file
" Language: Webfocus Scripting (WFS)
" Maintainer: Andrew Dean (ndrwdn) <ndrwdn AT gmail DOT com>
" Last Change: 2/1/2012 1:39:00 PM
" Version: 0.1

if exists("b:current_syntax")
    finish
endif

syn case match

syn keyword wfs_keyword   CopyHTTPCookieToWFVar
syn keyword wfs_keyword   CopyHTTPHeaderToWFVar
syn keyword wfs_keyword   CopyHTTPMethodToWFVar
syn keyword wfs_keyword   CopyWFVarToSessionVar
syn keyword wfs_keyword   CopySessionVarToWFVar

syn case ignore

syn keyword wfs_keyword   AND EQ GT GE LT LE NE OR
syn keyword wfs_keyword   CONTAINS
syn match   wfs_keyword   "<\(call\|exit\|htmlform\|include\|conditional_include\|set\)>"
syn match   wfs_keyword   "<\(if\|ifdef\|ifndef\|else\|endif\)>"
syn match   wfs_keyword   "<\(sendvar\|endsendvar\)>"
syn match   wfs_keyword   "<dba_pass>"
syn match   wfs_keyword   "<ver \d>"
syn match   wfs_keyword   "\.upper"
syn match   wfs_comment   "^#.*$"
syn match   wfs_comment   "<!.*>"
syn region  wfs_string    start=+"+ end=+"+
syn match   wfs_number    "\<\d\+\>"
syn match   wfs_number    "\<\d\+\.\d*\>"
syn match   wfs_variable  "&&\=[A-Za-z0-9_]\+"
syn region  wfs_parameter start="(" end=")" contains=wfs_set_parms
syn keyword wfs_set_parms pass dontpass protect number alpha string contained

command -nargs=+ HiLink hi def link <args>

HiLink wfs_keyword   Keyword
HiLink wfs_comment   Comment
HiLink wfs_string    String
HiLink wfs_number    Number
HiLink wfs_variable  Identifier
HiLink wfs_set_parms Keyword

delcommand HiLink

let b:current_syntax = "wfs"
