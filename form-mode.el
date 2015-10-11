;;; form-mode.el --- Major mode for FORM source code

;; Filename: form-mode.el
;; Description: Major mode for FORM source code
;; Author: Jan-Lukas Wynen
;; Version 0.9

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Major mode for editing FORM source code. (See http://www.nikhef.nl/~form/
;; for language details)
;; Warning: still in developement, may be unstable! (TODO)

;;; Code:

(defvar form-mode-hook nil)

;;; TODO: for later use
(defvar form-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for FORM major mode.")



;;; -------------------------
;;;    Customisation
;;; -------------------------

(defgroup form nil
	"Major mode for editing FORM langauge files."
	:group 'languages)

(defface font-lock-wildcard-face
  '((((class color) (background light)) :foreground "DarkSlateGrey")
    (((class color) (background dark)) :foreground "CadetBlue")
    (((class grayscale) (background light)) :foreground "grey35")
    (((class grayscale) (background dark)) :foreground "grey65")
    (t :inverse-video t))
  "Font-lock mode face used to highlight wildcards in FORM-mode."
  :group 'font-lock-highlighting-faces
  :group 'form)

(defface font-lock-pattern-face
  '((((class color) (background light)) :foreground "DarkSlateGrey")
    (((class color) (background dark)) :foreground "CadetBlue")
    (((class grayscale) (background light)) :foreground "grey35")
    (((class grayscale) (background dark)) :foreground "grey65")
    (t :inverse-video t))
  "Font-lock mode face used to highlight matched patterns in FORM-mode."
  :group 'font-lock-highlighting-faces
  :group 'form)



;;; -------------------------
;;;    Syntax Highlighting
;;; -------------------------

;; Define lists for various keywords
(defconst form-syntax-module-keywords
  (eval-when-compile
    (regexp-opt '(".end" ".sort" ".store" ".global" ".clear") t ))
  "Regex for FORM module keywords."
  )

(defconst form-keywords
  (eval-when-compile  
    '("argument" "do" "else" "elseif ""if" "inexpression" "inside" "repeat" "term" "while"))
  "Keywords of FORM language."
  )

(defconst form-syntax-keywords
  (eval-when-compile
    ; combine keywords and the corresponding end... keywords
    (regexp-opt (append
                 form-keywords
                 (mapcar (lambda (str) (concat "end" str)) form-keywords) )
                t ))  ;; TODO: no mapping of end, see elseif
  "Regex for FORM keywords."
  )

(defconst form-syntax-type-names
  (eval-when-compile
    (regexp-opt '("f" "function" "functions" "cf" "cfunction" "cfunctions" "commuting"
                  "i" "index" "indices"
                  "v" "vector" "vectors" "t" "tensor" "tensors"
                  "ct" "ctensor" "ctensors" "nt" "ntensor" "ntensors"
                  "s" "sym" "symb" "symbol" "symbols" "table") t ))
  "Regex for FORM type names."
  )

(defconst form-syntax-assign-type-names
  (eval-when-compile
    (regexp-opt '("l" "local" "g" "global") t ))
  "Regex for FORM assignment type names, i.e. types of variables
to which a value is assigned."
  )

(defconst form-syntax-functions
  (eval-when-compile
    (regexp-opt
     '("ABracket" "ABrackets" "AntiB" "AntiBracket" "AntiBrackets"
       "AntiPutInside" "AntiSymmetrize" "Apply" "ArgExplode" "ArgImplode"
       "Auto" "Bracket" "Brackets" "ChainIn" "ChainOut"
       "Chrisholm" "ClearTable" "Collect" "CommuteInSet" "Compress" "Contract"
       "CycleSymmetrize" "DeallocateTable" "Delete" "Denominators" "Dimension" "Dim"
       "Discard" "Disorder" "Drop" "DropCoefficient" "DropSymbols" "Exit"
       "ExtraSymbols" "FactArg" "FactDollar" "Factorize" "Fill" "FillExpression"
       "FixIndex" "Format" "FromPolynomial" "FunPowers" "GFactorized"
       "GlobalFactorized" "Go" "Goto" "Hide" "InParallel" "InsideFirst"
       "IntoHide" "Keep" "Label" "LFactorized" "Load" "MakeInteger" "Merge"
       "Metric" "ModuleOption" "Modulus" "Multiply" "NDrop" "NFactorize"
       "NHide" "Normalize" "NotInParallel" "NPrint" "NSkip" "NunFactorize"
       "NunHide" "NWrite" "On" "Off" "PolyFun" "PolyRatFun" "PopHide" "Print" "Print[]"
       "PrintTable" "ProcessBucketSize" "ProperCount" "PushHide" "PutInside"
       "Ratio" "RCycleSymmetrize" "Redefine" "ReNumber" "ReplaceLoop"
       "Save" "Select" "SetExitFlag" "Shuffle" "Skip" "Sort" "SplitArg"
       "SplitFirstArg" "SplitLastArg" "Stuffle" "Sum" "Symmetrize"
       "TestUse" "TherdBucketSize" "ToPolynomial" "ToTensor" "ToVector"
       "Trace4" "Traven" "Transform" "TryReplace" "Unfactorize" "Unhide"
       "UnitTrace" "Write")  t))
  "Regex for FORM functions, i.e. Print."
  )


(defconst form-syntax-builtins
  (eval-when-compile
    (regexp-opt '("abs_" "bernoulli_" "binom_" "conjg_" "content_" "count_"
                  "d_" "dd_" "delta_" "deltap_" "denom_" "distrib_" "div_"
                  "dum_" "dummy_" "dummyten_" "e_" "exp_" "exteuclidean_"
                  "extrasymbol_" "fac_" "factorin_" "farg_" "firstbracket_"
                  "firstterm_" "g5_" "g6_" "g7_" "g_" "gcd_" "gi_"
                  "i_" "inverse_" "invfac_" "makerational_" "match_" "max_"
                  "maxpowerof_" "min_" "minpowerof_" "mod_" "mod2_" "nargs_"
                  "nterms_" "numfactors_" "pattern_" "poly_" "prime_" "random_"
                  "ranperm_" "rem_" "replace_" "reverse_" "root_" "setfun_"
                  "sig_" "sign_" "sum_" "sump_" "table_" "tbl_" "term_"
                  "termsin_" "termsinbracket_" "theta_" "thetap_"
                  "sqrt_" "ln_" "sin_" "cos_" "tan_" "asin_" "acos_"
                  "atan_" "atan2_" "sinh_" "cosh_" "tanh_" "asinh_"
                  "acosh_" "atanh_" "li2_" "lin_"
                  "int_" "integer_" "pos_" "pos0_" "neg_" "symbol_" "fixed_"
                  "index_" "number_" "even_" "odd_" "dummyindices_") t ))
  "Regex for FORM builtin functions."
  )


(defvar form-matcher-pattern nil
  "Pattern that has been found by the matcher.  This variable 
is used by the syntax highlighter, do not use it for anything else!")

(defun form-search-for-id-statement (limit)
  "Search for the id-statement and set FORM-matcher-pattern to nil."
  (setq form-matcher-pattern nil)
  (re-search-forward "[ \t]*\\(id\\(entify\\|old\\|new\\)?\\|also\\)[ \t,]+" limit t)
  )


;;          TODO
;; multiline
;; preprocessor strings
;; use of builtin sets overwrites wildcard colouring

(defun form-search-for-wildcard (limit)
  "Search for a wildcard expression in an id-statement and store the wildcard
variable in matcher-pattern. The matcher data will be set to the found wildcard."
  (let ((name "[a-zA-Z][a-zA-Z0-9]*\\|\\[[^]]+]"))
    (when (re-search-forward (concat
                              "\\(\\(" name "\\)\\?\\(" name "\\)?\\(\\?" name "\\)?\\|\\(\\?[a-zA-Z][a-zA-Z0-9]*\\|\\?\\[[^]]+]\\)\\).*="
                              )
                             limit t)

        ;; append the new wildcard to the list
        ;; use match 2 (foo?) or match 5 (?bar) depending on which is non-nil
        (if (match-string-no-properties 2)
            ;; then
            (setq form-matcher-pattern
                  (cons (match-string-no-properties 2)
                        form-matcher-pattern)
                  )
          ;; else
          (when (match-string-no-properties 5)
            (setq form-matcher-pattern
                  (cons (match-string-no-properties 5)
                        form-matcher-pattern)
                  )
            )
          ) ; endif

        ;; move point to the end of the wildcard
        (goto-char (match-end 1))
      ) ; when
    ) ; let
  ) ; defun


(defun form-search-for-pattern (limit)
  "Search for patterns that have been found by FORM-search-for-wildcard
in  an id-statement."

  ;; do nothing when there are no patterns to be matched
  (when form-matcher-pattern
    ;; if form-matcher-pattern is a list, convert it to a regexp
    (when (listp form-matcher-pattern)
      ;; first precede all brackets with backslashes
      (setq form-matcher-pattern (mapcar
                                  (lambda (arg) (replace-regexp-in-string "\\[" "\\[" arg t t))
                                  (mapcar
                                   (lambda (arg) (replace-regexp-in-string "\\]" "\\]" arg t t))
                                   form-matcher-pattern)
                                  )
            )
      ;; precede all questionmarks with backslashes
      (setq form-matcher-pattern (mapcar
                                  (lambda (arg) (replace-regexp-in-string "\\?" "\\?" arg t t))
                                  form-matcher-pattern)
            )
      
      ;; regexp-opt treats \\[ in the wrong way -> use custom function
      (setq form-matcher-pattern
            (concat;
             "[^\\?a-zA-Z0-9]\\("
             (car form-matcher-pattern)
             (apply 'concat (mapcar (lambda (str) (concat "\\|" str))
                                    (cdr form-matcher-pattern))
                    )
             "\\)\\>"
             )
            )
      )
    
                                        ; search for any pattern that was found previously
    (re-search-forward form-matcher-pattern limit t)
    )
  )

(defconst form-font-lock-keywords-1
  (list   
   ;; comment
   '("\\(^\\|;[ \t]*\\)\\(*.*\\)$" 2 font-lock-comment-face)

   ;; preprocessor
   '("^[ \t]*\\(#[:$]??[a-zA-Z]+\\|#[+-]\\)[ \t\n]+" . font-lock-preprocessor-face)

   ;; keywords
   `(,(concat "^[ \t]*\\("
              form-syntax-keywords
              "\\)\\>") 1 font-lock-keyword-face)
   
   ;; module statements
   `(,(concat "^[ \t]*"
              form-syntax-module-keywords
              "\\>")
     . font-lock-reference-face)
   )
  "Basic keywords for font-lock in FORM-mode. Highlight comments, preprocessor directives, language keywords and module statements."
  )

(defconst form-font-lock-keywords-2
  (append form-font-lock-keywords-1
          (list 
           ;; functions
           `(,(concat
               "\\<"
               form-syntax-functions
               "\\>"
               )  . font-lock-function-name-face)
           
           ;; builtins
           `(,(concat "\\<\\("
                      form-syntax-builtins
                      "\\)\\>") 1 font-lock-builtin-face)

           ;; colour type and variable name in definition (without =)
           ;; TODO: treat index ranges correctly (whitespaces)
           `(,(concat
               "^[ \t]*"
               form-syntax-type-names
               "\\>") (0 font-lock-type-face)

               (",?[ \t]*<??\\([a-zA-Z][a-zA-Z0-9]*\\|\\[[^]]+]\\)>??\\([ \t,;]\\|([a-zA-Z0-9:+\\-0]*)\\|#[RCI]\\|[ \t]*=[ \t]*[a-zA-Z0-9]+[ \t]*:[ \t]*[a-zA-Z0-9]+\\)"
                nil nil (1 font-lock-variable-name-face))
               )
           ;; TODO: add others?
           `("^AutoDeclare\\>" (0 font-lock-function-name-face)
             (,(concat
               "\\<"
               form-syntax-type-names
               "\\>") nil nil (0 font-lock-type-face))

               (",?[ \t]*\\([a-zA-Z][a-zA-Z0-9]*\\|\\[[^]]+]\\)\\([ \t,;]\\|([a-zA-Z0-9:+\\-0]*)\\|#[RCI]\\|[ \t]*=[ \t]*[a-zA-Z0-9]+[ \t]*:[ \t]*[a-zA-Z0-9]+\\)"
                nil nil (1 font-lock-variable-name-face))
               )
           
           ;; colour type and variable name in assignment (with =)
           `(,(concat
               "^[ \t]*"
               form-syntax-assign-type-names
               "\\>") (0 font-lock-type-face)

               ("[ \t]+\\([a-zA-Z][a-zA-Z0-9]*\\|\\[[^]]+]\\)[ \t]*=" nil nil (1 font-lock-variable-name-face))
               )

           ;; declaration of sets
           '("^[ \t]*\\(set\\)\\>" (1 font-lock-type-face)
             ("[ \t]+\\([a-zA-Z][a-zA-Z0-9]*\\|\\[[^]]+]\\)[ \t]*\\(;\\|:.+\\)" nil nil (1 font-lock-variable-name-face))
             )
           ))
  "Additional keywords for font-locking in FORM-mode. Add special functions and variable definitions."
  )

(defconst form-font-lock-keywords-3
  (append form-font-lock-keywords-2
          (list
           ;; wildcards
           '(form-search-for-id-statement (1 font-lock-type-face)
                                          (form-search-for-wildcard nil (re-search-forward "[^=]*=" (line-end-position) t) (1 'font-lock-wildcard-face))
                                          (form-search-for-pattern nil nil (1 'font-lock-pattern-face)))
   ))
  "Complete keywords for font-locking in FORM-mode including matching of wildcards."
  )

(defvar form-font-lock-keywords form-font-lock-keywords-3
  "Default highlighting expressions for FORM-mode.")



;;; -----------------
;;;    Indentation
;;; -----------------

(defvar form-basic-offset 2
  "Depth of one indentation step")

(defun form-prev-line-indent ()
  "Return the level of indentation of the current line if the previous line is not closed or nil otherwise."
  (beginning-of-line)
  (if (bobp) 0
    (save-excursion
      (forward-line -1)
      (end-of-line)
      ;; recurse for empty lines or comments
      (if (looking-back "\\(^[ \t]*\\|^\\*.*\\)")
          (form-prev-line-indent)
        (unless (looking-back "\\(;[ \t]*\\(\\*.*\\)??\\|#.*\\|^.[a-zA-Z]*[ \t]*\\)")
          ;; return nil for closed lines

          (if (re-search-backward "[=:]" (line-beginning-position) t)
              (+ 2 (current-column))              
            (+ (current-indentation) form-basic-offset)
            )
          )
        )
      )
    ) ; if (bobp)
  )

(defun form-block-indent ()
  "Return the level of indentation of the block point is currently in."
  (if (bobp) 0
    (save-excursion
      (forward-line -1)
      (beginning-of-line)
      (if (looking-at "^\\([ \t]*$\\|\\*\\)")
          (form-block-indent)
        (if (re-search-forward
             (eval-when-compile
               (concat
                "\\(^\\|[ \t;]\\)\\("
                (regexp-opt '("#do" "#procedure" "#inside" "#if" "#ifdef" "#ifndef" "#switch"
                              "#case" "default" "#else" "#elseif"
                              "inside" "term" "do" "inexpression" "argument" "else") t)
                "\\|"
                "\\(\\(\\(else\\)??if\\|while\\)[ \t]*(.*)\\|repeat\\)[ \t]*;"
                "\\)")
               )
             (line-end-position) t)
            ;; then
            (+ (current-indentation) form-basic-offset)
          ;; else
          ;; need to recurse when prev line is not closed
          ;; -> current line not on right level of indentation
          (if (form-prev-line-indent)
              (form-block-indent)
            (current-indentation)
            )
          )
        ) ; if (looking-at)
      )
    ) ; if (bobp)
  )


(defun form-indent-line ()
  "Indent current line in FORM code."
  (interactive)

  (let ((cur-indent))
    (save-excursion
      (beginning-of-line)
      
      (if (bobp)   ; beginning of buffer
          (setq cur-indent 0)
        
        (setq cur-indent  (form-prev-line-indent))
        (unless cur-indent  ; previous line is closed
          (if (looking-at "^[ \t]*\\*") ; cur line is comment or empty
              (setq cur-indent 0)
            (progn
              (setq cur-indent (form-block-indent))
              (when (re-search-forward
                     (eval-when-compile
                       (concat
                        "\\("
                        (regexp-opt
                         '("endif" "enddo" "endwhile" "endrepeat" "endargument"
                           "endinexpression" "endinside" "endterm"
                           "else" "elseif" "#case" "#else" "#elseif")
                         t)
                        "\\|"
                        "#end[a-zA-Z]*"
                        "\\)"
                        )
                       )
                     (line-end-position) t)
                ;; then
                (setq cur-indent (- cur-indent form-basic-offset))
                )
              )
            )
          )
        ) ; if (bobp)
      )
    ;; now indent line
    (when (< cur-indent 0) (setq cur-indent 0))
    (indent-line-to cur-indent)

    ) ; let  
  ) ; defun form-indent-line



;;; -----------------
;;;   Syntax Table
;;; -----------------

(defvar form-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)  ; "_" is part of a word 
    (modify-syntax-entry ?\" "w" st)

    (modify-syntax-entry ?? "." st)
    
    (modify-syntax-entry ?. "w" st)    ; correct for ac-mode but scalar prod?

    (modify-syntax-entry ?] "w" st)
    (modify-syntax-entry ?[ "w" st)
  
    (modify-syntax-entry ?( "()" st)
    (modify-syntax-entry ?) ")(" st)
    
    (modify-syntax-entry ?` "('" st)
    (modify-syntax-entry ?' ")`" st)
    
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?/ "." st)
    st) ; let
  "Syntax table for form-mode.")



;;; -----------------------
;;;      Commenting
;;; -----------------------
(defun form-comment-dwim (arg)
  "Comment or uncomment the current line or text selection."
  (interactive)
  
  ;; If there's no text selection, comment or uncomment the line
  ;; depending whether the line is a comment. If there is a text
  ;; selection, using the first line to determine whether to
  ;; comment/uncomment.
  (let (p1 p2)
    (if (use-region-p)
        ;; then
        (save-excursion
          (setq p1 (region-beginning) p2 (region-end))
          (goto-char p1)
          (setq p1 (line-beginning-position))
          (if (form-contains-uncmt-p p1 p2)
              (form-comment-region p1 p2)
            (form-uncomment-region p1 p2)
            ))
      ;; else
      (progn
        (if (form-line-is-cmt-p)
            (form-uncomment-current-line)
          (form-comment-current-line)
          )
        )
      )
    )
  )

(defun form-contains-uncmt-p (p1 p2)
  "Test whether there are un commented lines in the selected region."
  (catch 'form-comment-test
    (save-excursion
      (goto-char p1)
      ;; endless loop if not tested for eob
      (while (and (< (point) p2) (not (eobp)))
        (unless (form-line-is-cmt-p)
          (throw 'form-comment-test t))
        (forward-line 1)
        )
      (when (eobp)
        (forward-line 1)
        (unless (form-line-is-cmt-p)
          (throw 'form-comment-test t)))
      )
    (throw 'form-comment-test nil)
    )
  )

(defun form-line-is-cmt-p ()
  "Test whether the current line is a comment or not."
  (save-excursion
    (beginning-of-line 1)
    (looking-at "\\*"))
  )

(defun form-comment-current-line ()
  "Make the current line a comment by adding '*' to the line beginning."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert "* ")
    )
  )

(defun form-uncomment-current-line ()
  "Remove a '*' from the current line's beginning if there is any.
Indents the line afterwards using form-indent-line."
  (interactive)
  (when (form-line-is-cmt-p)
    (save-excursion
      (beginning-of-line 1)
      (delete-forward-char 1)
      (form-indent-line)
      )
    )
  )

(defun form-comment-region (p1 p2)
  "Add '*' to the beginning of each line in the current region."
  (interactive "r")
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char p2)
      (while (>= (point) p1)
        (form-comment-current-line)
        (previous-line)
        )
      )
    )
  )

(defun form-uncomment-region (p1 p2)
  "Remove '*' from the beginning of each line in the current region if there is any.
All modified lines are reindented afterwards using form-indent-line."
  (interactive "r")
  (deactivate-mark)
  (save-excursion
    (goto-char p1)
    ;; endless loop if not tested for eob
    (while (and (< (point) p2) (not (eobp)))
      (form-uncomment-current-line)
      (form-indent-line)
      (forward-line 1)
      )
    (when (eobp) (forward-line 1) (form-indent-line))
    )
  )



;;; -----------------------
;;;     Auto-Complete
;;; -----------------------
(ac-define-source formsource
  (eval-when-compile
    '((candidates . (append
                     form-keywords
                     (mapcar (lambda (str) (concat "end" str)) form-keywords)
                     '(".end" ".sort" ".store" ".global" ".clear")
                     '("Function" "CFunction" "Commuting" "Index" "Vector" "Tensor"
                       "CTensor" "NTensor" "Symbol")
                     '("Local" "Global")
                     '("abs_" "bernoulli_" "binom_" "conjg_" "content_" "count_"
                       "delta_" "deltap_" "denom_" "distrib_" "div_" "dum_"
                       "dummy_" "dummyten_" "exp_" "exteuclidean_" "extrasymbol_"
                       "fac_" "factorin_" "farg_" "firstbracket_" "firstterm_" 
                       "inverse_" "invfac_" "makerational_" "match_" "max_"
                       "maxpowerof_" "min_" "minpowerof_" "mod_" "mod2_" "nargs_"
                       "nterms_" "numfactors_" "pattern_" "poly_" "prime_" "random_"
                       "ranperm_" "rem_" "replace_" "reverse_" "root_" "setfun_"
                       "sig_" "sign_" "sum_" "sump_" "table_" "tbl_" "term_"
                       "termsin_" "termsinbracket_" "theta_" "thetap_"
                       "sqrt_" "sin_" "cos_" "tan_" "asin_" "acos_"
                       "atan_" "atan2_" "sinh_" "cosh_" "tanh_" "asinh_"
                       "acosh_" "atanh_" "li2_" "lin_"
                       "int_" "integer_" "pos_" "pos0_" "neg_" "symbol_" "fixed_"
                       "index_" "number_" "even_" "odd_" "dummyindices_")
                     '("ABracket" "ABrackets" "AntiB" "AntiBracket" "AntiBrackets"
                       "AntiPutInside" "AntiSymmetrize" "Apply" "ArgExplode" "ArgImplode"
                       "Auto" "AutoDeclare" "Bracket" "Brackets" "ChainIn" "ChainOut"
                       "Chrisholm" "ClearTable" "Collect" "CommuteInSet" "Compress" "Contract"
                       "CycleSymmetrize" "DeallocateTable" "Delete" "Denominators" "Dimension"
                       "discard" "disorder" "Drop" "DropCoefficient" "DropSymbols" "Exit"
                       "ExtraSymbols" "FactArg" "FactDollar" "Factorize" "Fill" "FillExpression"
                       "FixIndex" "Format" "FromPolynomial" "FunPowers" "GFactorized"
                       "GlobalFactorized" "Goto" "Hide" "InParallel" "InsideFirst"
                       "IntoHide" "Keep" "Label" "LFactorized" "Load" "MakeInteger" "Merge"
                       "Metric" "ModuleOption" "Modulus" "Multiply" "NDrop" "NFactorize"
                       "NHide" "Normalize" "NotInParallel" "NPrint" "NSkip" "NunFactorize"
                       "NunHide" "NWrite" "Off" "PolyFun" "PolyRatFun" "PopHide" "Print" "Print[]"
                       "PrintTable" "ProcessBucketSize" "ProperCount" "PushHide" "PutInside"
                       "Ratio" "RCycleSymmetrize" "Redefine" "ReNumber" "ReplaceLoop"
                       "Save" "Select" "SetExitFlag" "Shuffle" "Skip" "Sort" "SplitArg"
                       "SplitFirstArg" "SplitLastArg" "Stuffle" "Sum" "Symmetrize"
                       "TestUse" "TherdBucketSize" "ToPolynomial" "ToTensor" "ToVector"
                       "Trace4" "Traven" "Transform" "TryReplace" "Unfactorize" "Unhide"
                       "UnitTrace" "Write")
                     '("symmetric" "antisymmetric" "Statistics" "AllFunPowers" "HighFIrst")
                     '("#procedure" "#endprocedure")
                     '("match" "count" "coefficient" "findLoop" "multipleof" "termsin")
                     ) )
      (cache))
    )
  )


;;; -----------------------
;;;       Flycheck
;;; -----------------------
;; TODO: delay before checking -> its annoying
;; TODO: warning handling: needs line info
;; TODO: do not check .prc files
(eval-after-load 'flycheck
  '(progn
     (flycheck-define-checker form-checker
       "A FORM syntax checker."
       :command ("form" "-c" "-q" source)
       :error-patterns
       ((error line-start (file-name) " Line " line " --> " (message) line-end)
        (error line-start (file-name) " Line " line " ==> " (message) line-end)
        (warning line-start " --> Warning: " (message) line-end)
        )
       :modes form-mode
       )
     (add-to-list 'flycheck-checkers 'form-checker)
     ))



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(frm\\|prc\\)\\'" . form-mode))



(define-derived-mode form-mode prog-mode
  ""
  :syntax-table form-mode-syntax-table

  (setq font-lock-defaults '(form-font-lock-keywords t t))
  (setq indent-line-function 'form-indent-line)
  (setq mode-name "FORM")

  (setq comment-start "*")

  (setq ac-sources '(ac-source-formsource))
  )

(provide 'form-mode)

;;; form-mode.el ends here
