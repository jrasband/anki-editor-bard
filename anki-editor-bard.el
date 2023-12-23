;;; anki-editor-bard.el
;; Functions to create anki flashcards in the style of https://ankiweb.net/shared/info/2084557901

;; brief description of capture:
;; Text to be barded will appear like this:
;; **** Title
;; #+begin_verse
;; a
;; b
;; c
;; d
;; #+end_verse

;; prototype capture template

;;; Functions

(defun bard-prepare-for-capture (element)
  "Take clozed list element and make it an Org subtree.
The finished Org subtree will have the correct properties and linking."
  (let ((my-new-id (org-id-new)))
    (s-join "\n" (list "\n** Lyric Cloze"
		       ":PROPERTIES:"
		       ":ANKI_NOTE_TYPE: Cloze"
		       ":ANKI_DECK: Mega"
		       (concat ":ID: " my-new-id)
		       ":ROAM_EXCLUDE: t"
		       ":END:"
		       "*** Text"
		       (concat "**** " element)
		       "*** Back Extra"
		       (concat "[[org-protocol://open-id?id=" my-new-id "][Link to Roam File]]")))))

(defun bard-create-notes (text &optional recite-length context-length)
  "Create a list of all notes.
Notes are clozed, have titles, and are in `verse' environment.
Content is the text inside a verse block."
  (let ((content-list (bard-make-content-list text recite-length context-length))
	(title (bard-get-title text)))
    (bard-make-element-list content-list title)))

(defun bard-make-content-list (text recite-length context-length)
  "Function to create the text inside a verse block.
Returns a list of lists of note elements.
Each list element is a list of the context lines, then the lines to be recited."
  (let ((content-text (bard-get-content-text text)))
    (let ((sequence (number-sequence 0 (1- (length content-text)) recite-length))
	  (beginning-lines-list (append (make-list context-length "Beginning") content-text)))
      (let ((context-recite-list (seq-map (lambda (i) (seq-subseq beginning-lines-list i (min (+ i context-length recite-length) (length beginning-lines-list)))) sequence)))
	(bard-cloze-content-list context-recite-list 1 1)))))

(defun bard-get-content-text (text)
  "Function to get only relevant content text."
  (let ((lines-list (split-string text "\n")))
    (butlast (cddr lines-list))))

(defun bard-cloze-content-list (context-recite-list recite-length context-length)
  "Function to cloze elements in `context-recite-list.'"
  (cl-mapcar (lambda (var)
	       (let ((context-lines-split (-take context-length var))
				 (recite-lines-split (-drop context-length var)))
			     (append
			      context-lines-split (mapcar
						   #'bard-cloze-simple recite-lines-split))))
	     context-recite-list))

(defun bard-make-element-list (content-list title)
  (mapcar (lambda (content-list-element) (s-join "\n" (append (list title) (list "#+begin_verse") content-list-element (list "#+end_verse")))) content-list))

(defun bard-get-title (text)
  "Get the title.
This assumes that the poem is in the form:
Title
#+begin_verse
a
b
c
d...
#+end_verse"
    (car (split-string text "\n")))

(defun bard-cloze-simple (line &optional prompt)
  "Cloze a single line with PROMPT if given."
  (if prompt
      (concat "{{c1::" line "::" prompt "}}")
    (concat "{{c1::" line "}}")))

(provide 'anki-editor-bard)
;;; end anki-editor-bard.el
