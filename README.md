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
<!-- But the end results are sure purdy. -->
<table><thead><tr>
<th>Source</th>
<th>Destination</th>
<th>Contents</th></tr></thead>
<tbody>
<tr><td>
   <a href="http://howardabrams.com/projects/dot-files/dot-emacs.html"><code>dot-emacs.org</code></a>
</td><td>
   <a href="blob/master/results/emacss.el"><code>.emacs-ext.el</code></a>
</td><td>
  "Sourced" in from the <code>.emacs</code> file
</td></tr><tr><td>
  <a href="http://howardabrams.com/projects/dot-files/zsh-theme.html"><code>zsh-theme.org</code></a>
</td><td>
   <a href="blob/master/results/happiness.zsh-theme"><code>.oh-my-zsh/themes</code></a>
</td><td>
  My ZShell "theme" 
</td></tr><tr><td>
  <a href="http://howardabrams.com/projects/dot-files/zsh-env.html"><code>zsh-env.org</code></a>
</td><td>
   <a href="blob/master/results/zshenv"><code>.zshenv</code></a>
</td><td>
  Global Zsh environment variables
</td></tr><tr><td>
  <a href="http://howardabrams.com/projects/dot-files/zsh-rc.html"><code>zsh-rc.org</code></a>
</td><td>
   <a href="blob/master/results/zshrc"><code>.zshrc</code></a>
</td><td>
  Zsh functions ... calls <code>sh-funcs.sh</code>
</td></tr><tr><td>
</td><td>
  <code>.zprofile</code>
</td><td>
  Machine-specific environment variables
</td></tr><tr><td>
  <a href="http://howardabrams.com/projects/dot-files/sh-functions.html"><code>sh-functions.org</code></a>
</td><td>
   <a href="blob/master/results/sh-functions.sh"><code>.sh-funcs.sh</code></a>
</td><td>
  Global functions and aliases
</td></tr></tbody></table>


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
