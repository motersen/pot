pot
===

a universal organization tool.  
Pot is simple, fast and powerful.

- [Examples](#examples)
- [Commands](#commands)
- [Installation](#installation)

Examples
--------

###indexing images

display the 5 last modified files in sxiv and tag marked ones with
**cute** and **cats**:

```shell
ls -1t | head -5 | sxiv -iot | pot tag cute,cats
```

create a randomized rotation of wallpapers satisfying a given filter:

[potbg] _filter_ 

and advance on command:

[potbg]

###managing bookmarks

find all resources tagged **sed** that also have the tags
**programming** or **linux** contained in the database located in
bookmarks, select one and load the uri in surf:

```shell
pot --path ~/bookmarks filter "(programming;linux),sed" | dmenu -l 20 | xargs surf
```

Commands
--------

This is a general overview that should get you started. For deeper
understanding please read the manpage.

Arguments in square brackets are read from standard input if not given
on the command line.

`pot delete-tags [tags...]`

`pot filter <filter>`

`pot list-tags`

`pot reverse-search <resource>`

`pot tag some,tags [resource...]`

`pot untag maybe,other,tags [resource...]`

###Filters

Filters combine tags into sets of resources. The following operators
(sorted by descending precedence) can be used in filters:

```
() - parens ensure prioritized evaulation
, - commas intersect two sets
/ - slashes represent the difference of one set from another
; - semicolons unite two sets
```

Installation
------------

[Arch User Repository](https://aur.archlinux.org/packages/pot)

###Dependencies

- [Gambit-C][gambit]
- [musl-libc][musl] (default)

###Building

`make`

####Variables

- `GSC:=gsc` Gambit-C compiler
- `CC:=musl-gcc` C compiler

###Installing

`make install`

By default, pot attempts to install to `/usr`. To change the
installation path, set make variables `DESTDIR` and `PREFIX` accordingly.

[potbg]: https://github.com/motersen/dotfiles/blob/master/potbg/bin/potbg
[gambit]: http://gambitscheme.org/wiki/index.php/Main_Page
[musl]: http://www.musl-libc.org/
