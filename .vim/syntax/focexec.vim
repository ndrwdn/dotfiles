" Vim syntax file
" Language:	Focus Executable
" Maintainer:	Rob Brady <robb@datatone.com>
" Last Change:	$Date: 2004/06/13 15:38:04 $
" URL:		http://www.datatone.com/~robb/vim/syntax/focexec.vim
" $Revision: 1.1 $

" this is a very simple syntax file - I will be improving it
" one thing is how to do computes
" I don't like that &vars and FUSE() functions highlight to the same color
" I think some of these things should get different hilights -
"  should MODIFY commands look different than TABLE?

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

let b:focexec_fold='true'
if version < 600
   unlet! b:focexec_fold
endif

syn case match

" A bunch of useful keywords
"syn keyword focexecTable	TABLE TABLEF SUM BY ACROSS END PRINT HOLD LIST NOPRINT STYLE GRAPHSTYLE ENDSTYLE GRAPH
syn keyword focexecTable	TABLEF SUM BY ACROSS END PRINT HOLD LIST NOPRINT STYLE GRAPHSTYLE ENDSTYLE GRAPH
syn keyword focexecTable	SUBFOOT SUBHEAD HEADING FOOTING PAGE-BREAK AS
syn keyword focexecTable	WHERE AND OR NOSPLIT FORMAT
"syn keyword focexecModify	MODIFY DATA ON FIXFORM FREEFORM PROMPT MATCH COMPUTE
syn keyword focexecModify	DATA ON FIXFORM FREEFORM PROMPT COMPUTE
syn keyword focexecModify	GOTO CASE ENDCASE TYPE NOMATCH REJECT INCLUDE
"syn keyword focexecModify	MAINTAIN FILES PERFORM STACK CLEAR FOR INTO REPEAT
syn keyword focexecModify	FILES PERFORM STACK CLEAR FOR INTO REPEAT
syn keyword focexecModify	ENDREPEAT BEGIN ENDBEGIN COPY
syn keyword focexecModify	CONTINUE FROM
"syn keyword focexecNormal	CHECK FILE CREATE EX SET DEFAULT IF FILEDEF DEFINE
syn keyword focexecNormal	CHECK FILE CREATE EX SET DEFAULT IF FILEDEF APPEND
syn keyword focexecNormal	REBUILD IF RECORDLIMIT FI EQ JOIN WHILE
syn keyword focexecNormal	THEN ELSE NE GT GE LT LE WHENCE FOCEXEC
syn keyword focexecJoin		IN TO
syn keyword focexecFileDef	DISK
syn keyword focexecSet		MSG ALL
syn match   focexecDash		"-RUN"
syn match   focexecDash		"-QUIT"
syn match   focexecDash		"-EXIT"
syn match   focexecDash		"-READ"
syn match   focexecDash		"-WRITE"
syn match   focexecDash		"-PROMPT"
syn match   focexecDash		"-REPEAT"
syn match   focexecDash		"-WINFORM"
syn match   focexecDash		"-HTMLFORM BEGIN"
syn match   focexecDash		"-HTMLFORM END"

" String and Character constants
syn region  focexecString1	start=+"+ end=+"+
syn region  focexecString2	start=+'+ end=+'+

"amper variables
syn match   focexecAmperVar	"&&\=[A-Za-z0-9_]\+"

"fuse functions
syn keyword focexecFuse GETUSER GETUSR WHOAMI FEXERR ASIS GETTOK UPCASE LOCASE
syn keyword focexecFuse SUBSTR TODAY TODAYI POSIT HHMMSS BYTVAL EDAUT1 BITVAL
syn keyword focexecFuse BITSON FGETENV FPUTENV HEXBYT SPAWN YM YMI JULDAT
syn keyword focexecFuse JULDATI DOWK DOWKI DOWKLI CHGDAT CHGDATI FTOA ATODBL
syn keyword focexecFuse SOUNDEX RJUST REVERSE PARAG OVRLAY LJUST CTRFLD CTRAN
syn keyword focexecFuse CHKFMT ARGLEN GREGDT GREGDTI DTYMD DTYMDI DTDMY DTDMYI
syn keyword focexecFuse DTYDM DTYDMI DTMYD DTMYDI DTDYM DTDYMI DAYMD DAYMDI
syn keyword focexecFuse DAMDY DAMDYI DADMY DADMYI AYM AYMI AYMD AYMDI CHKPCK
syn keyword focexecFuse IMOD FMOD DMOD PCKOUT EXP BAR SPELLNM SPELLNUM RTCIVP
syn keyword focexecFuse PRDUNI PRDNOR RDNORM RDUNIF LCWORD ITOZ RLPHLD IBIPRO
syn keyword focexecFuse IBIPRW IBIPRC IBIPRU IBIRCP PTHDAT ITOPACK ITONUM
syn keyword focexecFuse DSMEXEC DSMEVAL DSMERRC MSMEXEC MSMEVAL MSMERRC EXTDXI
syn keyword focexecFuse BAANHASH EDAYSI DTOG GTOD HSETPT HPART HTIME HNAME
syn keyword focexecFuse HADD HDIFF HDATE HGETC HCNVRT HDTTM HMIDNT TEMPPATH
syn keyword focexecFuse DATEADD DATEDIF DATEMOV DATECVT EURHLD EURXCH FINDFOC
syn keyword focexecFuse FERRMES CNCTUSR CURRPATH USERPATH SYSTEM ASKYN
syn keyword focexecFuse FUSEMENU POPEDIT POPFILE EDIT

syn match   focexecNumber	"\<\d\+\>"
syn match   focexecNumber	"\<\d\+\.\d*\>"

syn match   focexecCommentChange "-\*!!!$"
syn match   focexecComment	"-\*\(!!!$\)\@!.*"

if exists("b:focexec_fold")
   syntax match focexecTableBegin /\<TABLE FILE\>/ skipwhite
   syntax region focexecTableBlock start="TABLE FILE" end="END\s*$" transparent fold keepend extend

   syntax match focexecMatchBegin /\<MATCH\>/ skipwhite
   syntax region focexecMatchBlock start="MATCH FILE" end="END\s*$" transparent fold keepend extend

   syntax match focexecModifyBegin /\<MODIFY FILE\>/ skipwhite
   syntax region focexecModifyBlock start="MODIFY FILE" end="END\s*$" transparent fold keepend extend

   syntax match focexecMaintainBegin /\<MAINTAIN FILE\(S\)\{0,1\}\>/ skipwhite
   syntax region focexecMaintainBlock start="MAINTAIN FILE" end="END\s*$" transparent fold keepend extend

   syntax match focexecDefineBegin /\<DEFINE FILE\>/ skipwhite
   syntax region focexecDefineBlock start="DEFINE FILE" end="END\s*$" transparent fold keepend extend

   syntax sync fromstart
   setlocal foldmethod=syntax
   setlocal foldlevel<
else
   syntax keyword focexecTableBegin TABLE
   syntax keyword focexecMatchBegin TABLE
   syntax keyword focexecModifyBegin MODIFY
   syntax keyword focexecMaintainBegin MAINTAIN
   syntax keyword focexecDefineBegin DEFINE

   setlocal foldmethod<
"   setlocal foldlevel=0
endif


" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_focexec_syntax_inits")
  if version < 508
    let did_focexec_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink focexecCommentChange   Number
  HiLink focexecString1		String
  HiLink focexecString2		String
  HiLink focexecNumber		Number
  HiLink focexecComment		Comment
  HiLink focexecTable		Keyword
  HiLink focexecTableBegin	Keyword
  HiLink focexecMatchBegin	Keyword
  HiLink focexecModifyBegin	Keyword
  HiLink focexecMaintainBegin	Keyword
  HiLink focexecDefineBegin	Keyword
  HiLink focexecModify		Keyword
  HiLink focexecNormal		Keyword
  HiLink focexecSet		Keyword
  HiLink focexecDash		Keyword
  HiLink focexecFileDef		Keyword
  HiLink focexecJoin		Keyword
  HiLink focexecAmperVar	Identifier
  HiLink focexecFuse		Function

  delcommand HiLink
endif

syn include @Html syntax/html.vim
syn include @Html indent/html.vim
"syn include @Html bundle/sparkup/ftplugin/html/sparkup.vim
"syn include @Html bundle/html_autoclosetag/plugin/html_autoclosetag.vim
syn region focexecHtml start="-HTMLFORM BEGIN" end="-HTMLFORM END" contains=@Html keepend

let b:current_syntax = "focexec"

" vim: ts=8
