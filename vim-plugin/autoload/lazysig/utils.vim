" =============================================================================
" File:          autoload/fprompt/utils.vim
" Description:   Utilities
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================

" Static variables {{{1
fu! fprompt#utils#lash()
	retu &ssl || !exists('+ssl') ? '/' : '\'
endf

fu! s:lash(...)
	retu ( a:0 ? a:1 : getcwd() ) !~ '[\/]$' ? s:lash : ''
endf

fu! fprompt#utils#opts()
	let s:lash = fprompt#utils#lash()
	let usrhome = $HOME . s:lash( $HOME )
	let cahome = exists('$XDG_CACHE_HOME') ? $XDG_CACHE_HOME : usrhome.'.cache'
	let cadir = isdirectory(usrhome.'.fprompt_cache')
		\ ? usrhome.'.fprompt_cache' : cahome.s:lash(cahome).'fprompt'
	if exists('g:fprompt_cache_dir')
		let cadir = expand(g:fprompt_cache_dir, 1)
		if isdirectory(cadir.s:lash(cadir).'.fprompt_cache')
			let cadir = cadir.s:lash(cadir).'.fprompt_cache'
		en
	en
	let s:cache_dir = cadir
endf
cal fprompt#utils#opts()

let s:wig_cond = v:version > 702 || ( v:version == 702 && has('patch051') )
" Files and Directories {{{1
fu! fprompt#utils#cachedir()
	retu s:cache_dir
endf

fu! fprompt#utils#cachefile(...)
	let [tail, dir] = [a:0 == 1 ? '.'.a:1 : '', a:0 == 2 ? a:1 : getcwd()]
	let cache_file = substitute(dir, '\([\/]\|^\a\zs:\)', '%', 'g').tail.'.txt'
	retu a:0 == 1 ? cache_file : s:cache_dir.s:lash(s:cache_dir).cache_file
endf

fu! fprompt#utils#readfile(file)
	if filereadable(a:file)
		let data = readfile(a:file)
		if empty(data) || type(data) != 3
			unl data
			let data = []
		en
		retu data
	en
	retu []
endf

fu! fprompt#utils#mkdir(dir)
	if exists('*mkdir') && !isdirectory(a:dir)
		sil! cal mkdir(a:dir, 'p')
	en
	retu a:dir
endf

fu! fprompt#utils#writecache(lines, ...)
	if isdirectory(fprompt#utils#mkdir(a:0 ? a:1 : s:cache_dir))
		sil! cal writefile(a:lines, a:0 >= 2 ? a:2 : fprompt#utils#cachefile())
	en
endf

fu! fprompt#utils#glob(...)
	let path = fprompt#utils#fnesc(a:1, 'g')
	retu s:wig_cond ? glob(path, a:2) : glob(path)
endf

fu! fprompt#utils#globpath(...)
	retu call('globpath', s:wig_cond ? a:000 : a:000[:1])
endf

fu! fprompt#utils#fnesc(path, type, ...)
	if exists('*fnameescape')
		if exists('+ssl')
			if a:type == 'c'
				let path = escape(a:path, '%#')
			elsei a:type == 'f'
				let path = fnameescape(a:path)
			elsei a:type == 'g'
				let path = escape(a:path, '?*')
			en
			let path = substitute(path, '[', '[[]', 'g')
		el
			let path = fnameescape(a:path)
		en
	el
		if exists('+ssl')
			if a:type == 'c'
				let path = escape(a:path, '%#')
			elsei a:type == 'f'
				let path = escape(a:path, " \t\n%#*?|<\"")
			elsei a:type == 'g'
				let path = escape(a:path, '?*')
			en
			let path = substitute(path, '[', '[[]', 'g')
		el
			let path = escape(a:path, " \t\n*?[{`$\\%#'\"|!<")
		en
	en
	retu a:0 ? escape(path, a:1) : path
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
