" =============================================================================
" File:          plugin/fprompt.vim
" Description:   Fuzzy file, buffer, mru, tag, etc finder.
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================
" GetLatestVimScripts: 3736 1 :AutoInstall: fprompt.zip

if ( exists('g:loaded_fprompt') && g:loaded_fprompt ) || v:version < 700 || &cp
	fini
en
let g:loaded_fprompt = 1

let [g:fprompt_lines, g:fprompt_allfiles, g:fprompt_alltags, g:fprompt_alldirs,
	\ g:fprompt_allmixes, g:fprompt_buftags, g:fprompt_ext_vars, g:fprompt_builtins]
	\ = [[], [], [], [], {}, {}, [], 2]

if !exists('g:fprompt_map') | let g:fprompt_map = '<Leader>]' | en
if !exists('g:fprompt_imap') | let g:fprompt_imap = '<c-]>' | en

" if !exists('g:fprompt_cmd') | let g:fprompt_cmd = 'FPrompt' | en

let s:fprompt_cmd = 'FPrompt'
let s:fprompt_cmd_i = 'FPromptI'

com! -n=? FPrompt         cal fprompt#init()
com! -n=? FPromptI         cal fprompt#init({'restore_insert': 1})

com! -bar FPromptClearCache     cal fprompt#clr()
com! -bar FPromptClearAllCaches cal fprompt#clra()

com! -bar ClearFPromptCache     cal fprompt#clr()
com! -bar ClearAllFPromptCaches cal fprompt#clra()

" exe 'nn <silent> <plug>(fprompt) :<c-u>'.g:fprompt_cmd.'<cr>'

exe 'nn <silent> <plug>(fprompt) :<c-u>'.s:fprompt_cmd.'<cr>'
exe 'nn <silent> <plug>(fprompt_i) :<c-u>'.s:fprompt_cmd_i.' "i"<cr>'

if g:fprompt_map != '' && !hasmapto('<plug>(fprompt)')
	exe 'map' g:fprompt_map '<plug>(fprompt)'
	exe 'imap' g:fprompt_imap '<plug>(fprompt_i)'
en

" vim:ts=2:sw=2:sts=2
