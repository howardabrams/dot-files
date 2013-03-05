My Dot Files
============

I don't know any geek worth his weight in arsenic that doesn't put all
of his/her collection of dot files under source code control. By
putting this stuff up on Github makes it trivial to share between
different computers and share with the world.

What's in this project?
-----------------------

Startup scripts for [Emacs][3] and [Zsh][6]. Specifically:

<!-- Why yes, it is a shame that I can't do multimarkdown or org-mode tables -->
<table><thead><tr>
<th>Source</th>
<th>Destination</th>
<th>Contents</th></tr></thead>
<tbody>
<tr><td>
   [`dot-emacs.org`][f1]
</td><td>
  `.emacs-ext.el`
</td><td>
  "Sourced" in from the `.emacs` file
</td></tr><tr><td>
  [`zsh-theme.org`][f2]
</td><td>
  `.oh-my-zsh/themes`
</td><td>
  My ZShell "theme" 
</td></tr><tr><td>
  [`zsh-env.org`][f3]
</td><td>
  `.zshenv`
</td><td>
  Global Zsh environment variables
</td></tr><tr><td>
  [`zsh-rc.org`][f4]
</td><td>
  `.zshrc`
</td><td>
  Zsh functions ... calls `sh-funcs.sh`
</td></tr><tr><td>
</td><td>
  `.zprofile`
</td><td>
  Machine-specific environment variables
</td></tr><tr><td>
  [`sh-functions.org`][f5]
</td><td>
  `.sh-funcs.sh`
</td><td>
  Global functions and aliases
</td></tr></tbody></table>

  [f1]: http://howardabrams.com/projects/dot-files/dot-emacs.html
  [f2]: http://howardabrams.com/projects/dot-files/zsh-theme.html
  [f3]: http://howardabrams.com/projects/dot-files/zsh-env.html
  [f4]: http://howardabrams.com/projects/dot-files/zsh-rc.html
  [f5]: http://howardabrams.com/projects/dot-files/sh-functions.html


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
