;;; KNIFE --- Runs Chef's knife command with ido completions
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2016, Howard Abrams, all rights reserved.
;; Created: 11 July 2016
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Build knife commands from completing read functions and execute
;;  the results.
;;
;;  Anyone who plays with Chef's `knife' command realizes that, while
;;  flexible, has a dizzying array of options, commands, sub-commands
;;  and sometimes, sub-sub-commands. I figured that I could use the
;;  IDO's `ido-completing-read' function at each step along the way to
;;  build up the command line. Once you've executed it once, it
;;  remembers it, so that calling `M-x knife' again, allows you to
;;  select a previous knife call, or create another.
;;
;;  If the command typed ends with a -c, it prompts for a knife
;;  configuration file (change the `knife--config-directory' for the
;;  default directory for this).
;;
;;  If the command typed ends with a -o, it prompts for a directory
;;  containing the cookbook you want to upload (change the
;;  `knife--repository-directory' for the default directory value for
;;  this).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;   - v.1.0 :: Banged out while waiting for a Jenkins build job
;;
;;   - v.1.1 :: Created a `history' ring to choose past commands
;;              Refactored the code to be more readable
;;
;;   - v.1.2 :: Pulled out hard-coded strings into customizable values.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defcustom knife--knife-command "knife"
  "The command to run `knife'. This can be changed to
  `proxychains4 -q knife' to work through certain tunnels.")

(defcustom knife--config-directory "~/.chef"
  "Default directory for Chef configuration files. Directory read
  when a `knife' command ends with a `-c'.")

(defcustom knife--repository-directory "~/chef-repo/cookbooks"
  "Default directory for Chef cookbooks. Directory read when a
  `knife' commands ends with a `-o'.")

(defvar knife--previous-commands '("<New Request>")
  "A list of all `knife` commands we've used.")

(defun knife--build-command-line ()
  "Build a `knife' command line string by using IDO to select
each command and sub-command."

  ;; Create a list of parts for the `knife' command invocation.
  ;; Begin with the actual `knife' executable:
  (let ((cmd-list (list knife--knife-command)))

    ;; Created a few helper functions to make the code in this
    ;; function easier to parse and read ... of course, these
    ;; mini-functions may require a bit of explanation:

    (cl-flet* (;; Combine our list into a string, but in reverse:
               (join (lst) (concat (mapconcat 'identity (reverse lst) " ") " "))
               ;; Wrappers around completing-read for each type of data:
               (choose-cmd (options)
                           (push (ido-completing-read (join cmd-list)
                                                      options) cmd-list))
               (choose-file (&optional dir)
                            (push (ido-read-file-name (join cmd-list) dir) cmd-list))
               (choose-dir (&optional dir)
                           (push (ido-read-directory-name (join cmd-list) dir) cmd-list))
               (choose-str () (push (read-string (join cmd-list)) cmd-list))

               (first (lst elt)   (equal (car lst) elt))
               (second (lst elt)  (equal (cadr lst) elt))
               (first-ends-with (lst elt) (string-suffix-p elt (car lst))))

      ;; Top-level KNIFE commands:
      (choose-cmd '("client" "configure" "cookbook" "data bag"
                    "environment" "exec" "index rebuild" "node"
                    "recipe" "role" "search" "ssh" "ssl" "status"
                    "tag" "user"
                    ;; Path-base commands
                    "delete" "deps" "diff" "download" "edit"
                    "list" "show" "upload" "xargs"))

      ;; The RECIPE command is odd, in that it only has a 'list' option:
      
      (if (first cmd-list "recipe")
          (push "list" cmd-list)

        ;; Many of the other options, accept a SUB-COMMAND, add one
        ;; based on the command at the top of the `cmds' list:
        (choose-cmd (pcase (car cmd-list)
                      ("client" '("bulk delete" "create" "delete" "edit" "list" "reregister" "show"))
                      ("cookbook" '("bulk delete" "create" "delete" "download" "list" "metadata" "metadata from" "show" "test" "upload"))
                      ("data bag" '("create" "delete" "edit" "from file" "list" "show"))
                      ("environment" '("compare" "create" "delete" "edit" "from file" "list" "show"))
                      ("node" '("bulk delete" "create" "delete" "edit" "from file" "list" "run_list" "show"))
                      ("role" '("bulk delete" "create" "delete" "edit" "from file" "list" "show"))
                      ("ssl" '("check" "fetch"))
                      ("tag" '("create" "delete" "list"))
                      ("user" '("create" "delete" "edit" "list" "reregister" "show")))))

      ;; If the sub-command was RUN_LIST, then we can add another sub-sub-command:
      
      (when (first cmd-list "run_list")
        (choose-cmd '("add" "remove" "set")))

      ;; Some sub-commands ask for a file:
      (when (or (first cmd-list "metadata from") (first cmd-list "from file"))
        (choose-file))

      ;; The DATA BAG FROM FILE command asks for
      ;; the name of the data bag before the file:
      
      (when (and (second cmd-list "data bag") (first cmd-list "from file"))
        (choose-str)                 ; data bag BAG
        (choose-file))               ; data bag BAG FILE

      ;; Final options are just added to the list:
      (choose-str)

      ;; If the knife command sequence ends with a -c, we choose a
      ;; file. If it ends with a -o, we choose a directory. We then
      ;; ask for more options:
      (while (or (first-ends-with cmd-list "-c") (first-ends-with cmd-list "-o"))
        (if (first-ends-with cmd-list "-c")
            (choose-file knife--config-directory)
          (choose-dir knife--repository-directory))
        (choose-str))

      ;; The actual knife command line is just a join of what was specified:
      (join cmd-list))))

(defun knife ()
  "An IDO interface to the `knife` command. Each step prompts for
the next command option, and attempts to be somewhat intelligent
about the choice. After completing, it accepts final options,
like references to a configuration file.

Given a prefix option, it simply re-runs the previous command."
  (interactive)
  (let ((new-cmd (car (last knife--previous-commands)))
        (knife-cmd (ido-completing-read "Run Command: " knife--previous-commands)))
    (when (equal knife-cmd new-cmd)
      ;; Requested a new command, so let's go through the process
      ;; and reset the 'knife-cmd' to the new choice:
      (setq knife-cmd (knife--build-command-line))
      (push knife-cmd knife--previous-commands))

    (message "Running: %s" knife-cmd)
    (shell-command knife-cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; knife.el ends here
