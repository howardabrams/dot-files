#+TITLE:  Ruby Configuration for Emacs
#+AUTHOR: Howard Abrams
#+EMAIL:  howard.abrams@gmail.com
#+DATE:   2014 Nov 24
#+TAGS:   emacs ruby
#+PROPERTY: header-args:sh :results silent :tangle no

The following instructions create a modern, blinged-out setup for
developing Ruby in Emacs.

* Ruby Installation

  Every Ruby /instance/ is controlled by [[https://rvm.io/][RVM]]. Install it, via:

  #+BEGIN_SRC sh
    curl -sSL https://get.rvm.io | bash -s stable
  #+END_SRC

  Next, install a Ruby version and set it to be used globally:

  #+BEGIN_SRC sh
    rvm install 1.9.3-p550
    rvm use 1.9.3-p550
  #+END_SRC

  See the [[#ruby-virtual-manager][Ruby Virtual Manager]] section below for using RVM within
  Emacs, as well as the Ruby configuration in my [[file:profile.org::*Ruby][shell environment]].

* Ruby Mode

  While the Ruby mode is supplied with Emacs, it needs to be
  associated with a few other file type extensions:

  #+BEGIN_SRC elisp
    (use-package ruby-mode
      :ensure t
      :mode "\\.rb\\'"
      :mode "Rakefile\\'"
      :mode "Gemfile\\'"
      :mode "Berksfile\\'"
      :mode "Vagrantfile\\'"
      :interpreter "ruby"

      :init
      (setq ruby-indent-level 2
            ruby-indent-tabs-mode nil)
      (add-hook 'ruby-mode 'superword-mode)

      :bind
      (([(meta down)] . ruby-forward-sexp)
       ([(meta up)]   . ruby-backward-sexp)
       (("C-c C-e"    . ruby-send-region))))  ;; Rebind since Rubocop uses C-c C-r
  #+END_SRC

  Being able to select code using expand-region, and then sending it
  to the Ruby REPL is often useful. But what does an /s-expression/
  mean in Ruby?

  Other keystrokes to remember:

  - =C-M-p= / =C-M-n= :: Move to the beginning and end of a block
  - =C-M-a= / =C-M-e= :: Move to the beginning and end of a function

  Use [[http://web-mode.org/][web-mode]] for dealing with ERB templates:

  #+BEGIN_SRC elisp
    (use-package web-mode
      :ensure t
      :mode "\\.erb\\'")
  #+END_SRC

* Ruby Virtual Manager

  Using [[https://github.com/senny/rvm.el][RVM integration]] for Emacs:

  #+BEGIN_SRC elisp
    (use-package rvm
      :ensure t
      :config
      (rvm-use-default))
  #+END_SRC

  When jumping from project to project, need to run the command:
  =rvm-use=... which must be done before starting an Eshell (that
  is, if you’re into that sort of thing).

  Start with a default Gemset by placing the following in your local
  configuration:

  #+BEGIN_SRC elisp :tangle no
    (rvm-use "ruby-2.0.0" "p643")
  #+END_SRC

  Note: Run the following regularly:
  #+BEGIN_SRC sh
    rvm get latest
  #+END_SRC

* Ruby Functional Doc

  The [[http://www.emacswiki.org/cgi-bin/emacs/YARI][Yari]] project attempts to hook Ruby calls to the =ri= project.

  #+BEGIN_SRC elisp :tangle no
    (use-package yari
      :ensure t
      :init
      (add-hook 'ruby-mode-hook
                (lambda ()
                  (local-set-key [f1] 'yari))))
  #+END_SRC

  Now, place point on some function, and hit =F1= to see the glory.
  In order for this to work, we need to generate the missing docs:

  #+BEGIN_SRC sh :tangle no
    gem rdoc --all --ri --no-rdoc
    rvm docs generate all
  #+END_SRC

  And we may have to do this for every change to RVM. Seems that
  =dash-at-point= is more effective (=C-c d=), however.

* Ruby REPL

  I am not sure I can learn a new language without a REPL connected to
  my editor, and for Ruby, this is [[https://github.com/nonsequitur/inf-ruby][inf-ruby]]:

  #+BEGIN_SRC elisp
    (use-package inf-ruby
      :ensure t
      :init
      (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))
  #+END_SRC

  To start eval-ing, do: =M-x inf-ruby=

  To run on a remote server:
  #+BEGIN_SRC elisp :tangle no
    (defun inf-ruby-remote (remote)
      "Run an inferior Ruby process on a remote server."
      (interactive "sHost: ")
      (let ((default-directory (format "/ssh:%s:" remote)))
        (inf-ruby)))
  #+END_SRC

  However, I need to specify a particular version, and I haven't
  figured out how to call a particular Ruby implementation:

  #+BEGIN_SRC elisp
    (defun inf-ruby-remote (remote)
      "Run an inferior Ruby process on a remote server."
      (interactive "sHost: ")
      (let ((default-directory (format "/ssh:%s:/opt/ruby2.0/embedded/bin" remote)))
        (run-ruby "/opt/ruby2.0/embedded/bin/irb" (format "%s:ruby" remote))))
  #+END_SRC

* Smart Parens

  Can I get the same wonder from *paredit* and Lisp in my Ruby using
  [[https://github.com/Fuco1/smartparens][smartparens]]? Not really, as it isn’t as pedantic as
  =paredit=. Still, it may be good enough for Ruby:

  #+BEGIN_SRC elisp
    (use-package smartparens
      :ensure t
      :diminish (smartparens-mode .  "()")
      :init
        (use-package smartparens-ruby)
        (add-hook 'ruby-mode-hook 'smartparens-strict-mode))
  #+END_SRC

* Rubocop

  The lint-like style checker of choice for Ruby is [[https://github.com/bbatsov/rubocop][Rubocop]].
  The [[https://github.com/bbatsov/rubocop-emacs][rubocop.el]] mode should just work with [[https://github.com/flycheck/flycheck][Flycheck]].

  #+BEGIN_SRC elisp
    (use-package rubocop
      :ensure t
      :init
      (add-hook 'ruby-mode-hook 'rubocop-mode)
      :diminish rubocop-mode)
  #+END_SRC

  Install it with: =gem install rubocop=

* Food Critic

   Let's get [[http://www.foodcritic.io/][Foodcritic]] working with Flycheck, but only if the file
   is located in a =recipes= or =cookbooks= directory:

   #+BEGIN_SRC elisp
     (use-package flycheck
       :no-require t
       :config
       (flycheck-define-checker chef-foodcritic
         "A Chef cookbooks syntax checker using Foodcritic.
     See URL `http://acrmp.github.io/foodcritic/'."
         :command ("foodcritic" source)
         :error-patterns
         ((error line-start (message) ": " (file-name) ":" line line-end))
         :modes (enh-ruby-mode ruby-mode)
         :predicate
         (lambda ()
           (let ((parent-dir (file-name-directory (buffer-file-name))))
             (or
              ;; Chef CookBook
              ;; http://docs.opscode.com/chef/knife.html#id38
              (locate-dominating-file parent-dir "recipes")
              ;; Knife Solo
              ;; http://matschaffer.github.io/knife-solo/#label-Init+command
              (locate-dominating-file parent-dir "cookbooks"))))
         :next-checkers ((warnings-only . ruby-rubocop))))
   #+END_SRC

* Robe

  [[https://github.com/dgutov/robe][Robe]] is a “code assistance” tool, that pretty much only works with
  methods (and doesn’t seem to work well with direct functions). One
  must install the following before this will work:

  #+BEGIN_SRC sh :tangle no
    gem install pry pry-doc
  #+END_SRC

  And even then, it barely works.
  Once started with =robe-start=, we should get code completion:

  #+BEGIN_SRC elisp :tangle no
    (use-package robe
      :ensure t
      :bind ("C-M-." . robe-jump)

      :init
      (add-hook 'ruby-mode-hook 'robe-mode)

      :config
      (defadvice inf-ruby-console-auto
        (before activate-rvm-for-robe activate)
        (rvm-activate-corresponding-ruby)))
  #+END_SRC

  If we have installed Company for auto-complete, use robe for this purpose:

  #+BEGIN_SRC elisp :tangle no
    (use-package company
      :no-require t
      :config
      (push 'company-robe company-backends))
  #+END_SRC

  With a complex Ruby project, one should evaluate the entire Ruby
  file (=C-c C-l=), and then run:

  - =robe-jump= to go to the method’s definition
  - =robe-ask= will act like jump, but asks for the method first
  - =robe-doc= displays the method documentation (doesn’t seem to be as useful as =dash-at-point=).

  However, it seldom works with any of the Ruby code that I use, so I
  currently have it turned off.

* Ruby Tools

  The little refactoring available with [[https://github.com/rejeep/ruby-tools.el][Ruby Tools]] looks interesting.

  #+BEGIN_SRC elisp
    (use-package ruby-tools
      :ensure t
      :init
      (add-hook 'ruby-mode-hook 'ruby-tools-mode)
      :diminish ruby-tools-mode)
  #+END_SRC

  The primary key-bindings operate on the /thing/ the cursor is on,
  e.g. a string, a symbol, etc.

  - =C-‘= converts the thing into a single-quoted string
  - =C-“= converts the thing into a double-quoted string
  - =C-:= converts the thing into a symbol

  Other options:

  - =C-;= clears the string
  - Inside a string the =#= key will insert a variable interpolation
    if the string is double-quoted (this is actually what I use this
    package the most)

* Technical Artifacts

  Make sure that we can simply =require= this library.

#+BEGIN_SRC elisp
  (provide 'init-ruby)
#+END_SRC

  Before you can build this on a new system, make sure that you put
  the cursor over any of these properties, and hit: =C-c C-c=

#+DESCRIPTION: A literate programming version of my Emacs Initialization for Ruby

#+PROPERTY:    header-args:elisp  :tangle ~/.emacs.d/elisp/init-ruby.el
#+PROPERTY:    header-args:ruby   :tangle no
#+PROPERTY:    header-args:sh     :tangle no
#+PROPERTY:    header-args:       :results silent   :eval no-export   :comments org

#+OPTIONS:     num:nil toc:nil todo:nil tasks:nil tags:nil
#+OPTIONS:     skip:nil author:nil email:nil creator:nil timestamp:nil
#+INFOJS_OPT:  view:nil toc:nil ltoc:t mouse:underline buttons:0 path:http://orgmode.org/org-info.js
