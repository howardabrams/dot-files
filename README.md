My Dot Files
============

I don't know any geek worth his weight in arsenic that doesn't put all
of his/her collection of dot files under source code control. By
putting this stuff up on Github makes it trivial to share between
different computers and share with the world.

What's in this project?
-----------------------

Mostly startup scripts for [Emacs][3] and [Zsh][6]. If you are
interested in how I configure Emacs, start with [emacs.org][7].
While the *configuration files* look like a document, that is the
basis for my Emacs Lisp code.

Where is the Code?
------------

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
However, you probably don't want to take complete files (or even fork this
project), as I have amassed a quite a bit of Lisp code for my environment.
Instead, gander through the documented output for the files, and copy
and paste anything that you find interesting.


  [0]: http://en.wikipedia.org/wiki/Literate_programming
  [1]: http://orgmode.org/worg/org-contrib/babel/intro.html
  [2]: http://orgmode.org
  [3]: http://www.gnu.org/software/emacs/
  [4]: http://creativecommons.org/licenses/by/3.0/
  [5]: https://github.com/robbyrussell/oh-my-zsh
  [6]: http://zsh.sourceforge.net
  [7]: https://github.com/howardabrams/dot-files/blob/master/emacs.org
