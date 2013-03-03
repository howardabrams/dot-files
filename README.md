My Dot Files
============

I don't know any geek worth his weight in arsenic that doesn't put all
of his/her collection of dot files under source code control. By
putting this stuff up on Github makes it trivial to share between
different computers and share with the world.

What's in this project?
-----------------------

Startup scripts for [Emacs][3] and [Zsh][6]. Specifically:

| Source             | Destination         | Contents                               |
|--------------------+---------------------+----------------------------------------|
| `dot-emacs.org`    | `.emacs-ext.el`     | "Sourced" in from the `.emacs` file    |
| `zsh-theme.org`    | `.oh-my-zsh/themes` | My ZShell "theme"                      |
| `zsh-env.org`      | `.zshenv`           | Global Zsh environment variables       |
| `zsh-rc.org`       | `.zshrc`            | Zsh functions ... calls `sh-funcs.sh`  |
|                    | `.zprofile`         | Machine-specific environment variables |
| `sh-functions.org` | `.sh-funcs.sh`      | Global functions and aliases           |

Why is there a source for your scripts?
----------------------------------

My scripts and other dot files usually use blurbs and blippets from
multiple sources on these here intertubes, and I find that when I get
back around to updating them years ago, I don't always have the
background for maintaining any particular blarp.

So I use a [literate programming][0] approach based on the
[Babel Project][1] (which is built on [org-mode][2] which, incidently,
only runs in [Emacs][3]). Each source file is *tangled* out to the
destination in my home directory.
  
Can I use your files?
---------------------

You may, as this collection is under a [creative commons license][4].
However, you probably don't want to take the files (or even fork this
project), since you probably don't have a neck-beard and use Emacs.

Instead, gander through the documented output for the files, and copy
and paste anything that you find interesting. My Zsh prompt, er,
*theme* will be contributed back to the [Oh My Zsh][5] project, so I
don't know if it is worthy of being committed.

  [0]: http://en.wikipedia.org/wiki/Literate_programming
  [1]: http://orgmode.org/worg/org-contrib/babel/intro.html
  [2]: http://orgmode.org
  [3]: http://www.gnu.org/software/emacs/
  [4]: http://creativecommons.org/licenses/by/3.0/
  [5]: https://github.com/robbyrussell/oh-my-zsh
  [6]: http://zsh.sourceforge.net
