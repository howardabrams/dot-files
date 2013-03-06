#!/usr/local/bin/zsh

# Happy Prompt

#   We first need to load the special prompt features as well as the
#   nicer color variables:

setopt prompt_subst
autoload -U colors && colors

# Our prompt checks the last command ran, and displays a red sad
#   face, if the command failed. Cute, true, but I'm not sure how
#   useful it is.

#   Prompt options:
#    * %n - User's name: habrams
#    * %m - Machine name
#    * %~ - Current directory with HOME substituted
#    * %c - Basename of the current directory
#    * %d - Current directory without substitution

#   Prompt colors: %{%F{red}%}
#    - red
#    - blue
#    - green
#    - yellow

my_prompt_string() {
  local ST=$?
  PROMPT_SAD="%{%F{red}%}(O_o)%{$reset_color%}"
  PROMPT_HAPPY="%{%F{green}%}(^_^)%{$reset_color%}"
  PROMPT_REST="%{%F{yellow}%}%~%{$reset_color%}\n %{%F{blue}%}➜ %{$reset_color%}"
  if [[ $ST = 0 ]]
  then
      echo "$PROMPT_HAPPY $PROMPT_REST "
  else
      echo "$PROMPT_SAD $PROMPT_REST "
  fi
}

# The prompt is simply the execution of our =my_prompt_string= function.

PROMPT='$(my_prompt_string)'

# Git Prompt

#    According to [[http://blog.joshdick.net/2012/12/30/my_git_prompt_for_zsh.html][this article]], we have a clever way of describing the
#    status of the current git repository with traffic lights.

#    Modify the colors and symbols in these variables as desired.

GIT_PROMPT_SYMBOL="%{$fg[blue]%}±"
GIT_PROMPT_PREFIX="%{$fg[green]%}[%{$reset_color%}"
GIT_PROMPT_SUFFIX="%{$fg[green]%}]%{$reset_color%}"
GIT_PROMPT_AHEAD="%{$fg[red]%}ANUM%{$reset_color%}"
GIT_PROMPT_BEHIND="%{$fg[cyan]%}BNUM%{$reset_color%}"
GIT_PROMPT_MERGING="%{$fg_bold[magenta]%}⚡︎%{$reset_color%}"
GIT_PROMPT_UNTRACKED="%{$fg_bold[red]%}●%{$reset_color%}"
GIT_PROMPT_MODIFIED="%{$fg_bold[yellow]%}●%{$reset_color%}"
GIT_PROMPT_STAGED="%{$fg_bold[green]%}●%{$reset_color%}"

# Show Git branch/tag, or name-rev if on detached head

parse_git_branch() {
  (git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD) 2> /dev/null
}

# Show different symbols as appropriate for various Git repository states

parse_git_state() {
  # Compose this value via multiple conditional appends.
  local GIT_STATE=""

  local NUM_AHEAD="$(git log --oneline @{u}.. 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_AHEAD" -gt 0 ]; then
    GIT_STATE=$GIT_STATE${GIT_PROMPT_AHEAD//NUM/$NUM_AHEAD}
  fi

  local NUM_BEHIND="$(git log --oneline ..@{u} 2> /dev/null | wc -l | tr -d ' ')"
  if [ "$NUM_BEHIND" -gt 0 ]; then
    GIT_STATE=$GIT_STATE${GIT_PROMPT_BEHIND//NUM/$NUM_BEHIND}
  fi

  local GIT_DIR="$(git rev-parse --git-dir 2> /dev/null)"
  if [ -n $GIT_DIR ] && test -r $GIT_DIR/MERGE_HEAD; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MERGING
  fi

  if [[ -n $(git ls-files --other --exclude-standard 2> /dev/null) ]]; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_UNTRACKED
  fi

  if ! git diff --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_MODIFIED
  fi

  if ! git diff --cached --quiet 2> /dev/null; then
    GIT_STATE=$GIT_STATE$GIT_PROMPT_STAGED
  fi

  if [[ -n $GIT_STATE ]]; then
    echo "$GIT_PROMPT_PREFIX$GIT_STATE$GIT_PROMPT_SUFFIX"
  fi
}

# If inside a Git repository, print its branch and state

git_prompt_string() {
  local git_where="$(parse_git_branch)"
  [ -n "$git_where" ] && echo "$GIT_PROMPT_SYMBOL$(parse_git_state)$GIT_PROMPT_PREFIX%{$fg[yellow]%}${git_where#(refs/heads/|tags/)}$GIT_PROMPT_SUFFIX"
}

# Set the right-hand prompt. Not sure why we don't use =RPROMPT=.

RPROMPT='$(git_prompt_string)'
