;;; lisppaste.el --- Interact with the lisppaste pastebot via XML-RPC.


;;; Commentary:
;; 

;;; Code:

(require 'cl)
(require 'xml-rpc)

(defconst lisppaste-url "http://common-lisp.net:8185/RPC2")

(defun lisppaste-send-command (command &rest stuff)
  "Send COMMAND to the lisppaste bot with STUFF as arguments."
  (apply #'xml-rpc-method-call lisppaste-url command stuff))

(defun lisppaste-new-paste (channel nick title content)
  "Create a new paste with the specified arguments.
CHANNEL is the channel the paste will appear in.
NICK is the nickname the paste will appear to be from.
TITLE is the paste's title.
CONTENT is the paste content."
  (lisppaste-send-command 'newpaste channel nick title content))

(defun lisppaste-annotate-paste (channel nick title content paste-number)
  "Annotate a paste with the specified arguments.
CHANNEL is the channel the paste will appear in.
NICK is the nickname the paste will appear to be from.
TITLE is the paste's title.
CONTENT is the paste content.
PASTE-NUMBER is the paste to annotate."
  (lisppaste-send-command 'newpaste channel nick title content
                          paste-number))

(defun lisppaste-get-paste (paste &optional n)
  "Fetch PASTE.

If N is non-nil, fetch the Nth annotation."
  (if n
      (lisppaste-send-command 'pastecontents paste n)
    (lisppaste-send-command 'pastecontents paste)))

(defun lisppaste-list-annotations (paste)
  "List the annotations for PASTE."
  (lisppaste-send-command 'pasteheaders paste))

(defvar lisppaste-default-nick nil
  "*The default nick for pastes.

See also the function `lisppaste-default-nick'.")

(defsubst lisppaste-default-nick (channel)
  "Return the default nick for CHANNEL.

If ERC is loaded, try and find a nick by looking for
`erc-current-nick' in CHANNEL's buffer.

If that returns nil, return the value of the variable
`lisppaste-default-nick'."
  (or (when (featurep 'erc)
        (erc-with-buffer ((get-buffer channel))
          (erc-current-nick)))
      lisppaste-default-nick))

(defmacro defpaste (name)
  "Define a plist-get method for NAME."
  `(defsubst ,name (p)
     (plist-get p ',name)))

(defpaste lisppaste-paste)
(defpaste lisppaste-annotation)
(defpaste lisppaste-channel)

(defsubst lisppaste-read-number (prompt &optional annotation)
  "Read a number prompting with PROMPT.

Default values are picked up from the text-properties around `point'.
If ANNOTATION is non-nil, look for annotation text-properties."
  (let* ((p (text-properties-at (point)))
         (num (lisppaste-paste p))
         (ann (lisppaste-annotation p)))
    (string-to-number
     (if annotation
         (read-from-minibuffer prompt
                               (and ann
                                    (number-to-string ann)))
       (read-from-minibuffer prompt
                             (and num
                                  (number-to-string num)))))))

(defsubst lisppaste-read-channel ()
  "Read a channel name."
  (read-string "Channel: "))

(defsubst lisppaste-read-nick (c)
  "Read a nick.

C is the default channel to look for a nick in with `lisppaste-default-nick'."
  (read-string "Nick: " (lisppaste-default-nick c)))

(defsubst lisppaste-read-title ()
  "Read a paste title."
  (read-string "Title: "))

(defun lisppaste-clean-returned-paste (paste)
  "Clean PASTE of HTML character entities."
  (with-temp-buffer
    (insert paste)
    (goto-char (point-min))
    (save-excursion (while (search-forward "&#xD;" nil t)
                      (replace-match "")))
    (while (re-search-forward "&\\(#x[^;]+\\);" nil t)
      (insert (read (match-string 1)))
      (replace-match ""))
    (buffer-string)))

(defvar lisppaste-creation-help
  ";; Enter your paste below, and press C-c C-c to exit.\n\n"
  "Paste creation help text.")
  
(defsubst lisppaste-buffer-substring (beg end)
  "Return part of the current buffer as a string.

BEG and END delimit the part of the buffer to return.

The string is returned with all tabs replaced by spaces.  See also
`untabify'."
  (let ((s (buffer-substring beg end))
        (tw tab-width))
    (with-temp-buffer
      (let ((tab-width tw))
        (insert s)
        (untabify (point-min) (point-max))
        (buffer-string)))))

(defun lisppaste-paste-region (beg end)
  "Send the region between BEG and END as a paste."
  (interactive "r")
  (let* ((annotate (if (y-or-n-p "Send this region as an annotation? ")
                       (lisppaste-read-number "Paste to annotate: ")))
         (channel (lisppaste-read-channel))
         (nick (lisppaste-read-nick channel))
         (title (lisppaste-read-title))
         (content (lisppaste-buffer-substring beg end)))
    (if annotate
        (lisppaste-annotate-paste channel nick title content annotate)
      (lisppaste-new-paste channel nick title content))))


(defun lisppaste-display-paste (paste &optional n)
  "Display PASTE.

If N is non-nil, display PASTE's Nth annotation."
  (interactive
   (list (lisppaste-read-number "Paste number: ")))
  (when current-prefix-arg
    (setq n (lisppaste-read-number "Annotation number: " t)))
  (let ((result (lisppaste-get-paste paste n))
        (buffer-read-only nil))
    (switch-to-buffer (get-buffer-create
                       (format "*Paste %s%s*" paste
                               (if n
                                   (format " annotation %s"
                                           n)
                                 ""))))
    (erase-buffer)
    (insert (propertize (lisppaste-clean-returned-paste result)
                        'lisppaste-paste paste
                        'lisppaste-annotation n))
    (lisppaste-mode)))

(defun lisppaste-list-paste-annotations (paste)
  "List PASTE's annotations."
  (interactive
   (list (lisppaste-read-number
          "List annotations for paste number: ")))
  (let ((result (lisppaste-list-annotations paste))
        (buffer-read-only nil))
    (unless result
      (error "Paste %s has no annotations" paste))
    (switch-to-buffer (get-buffer-create
                       (format "*Paste %s Annotations*" paste)))
    (erase-buffer)
    (loop for (num time user channel title annotations) in result
          do (insert
              (propertize (format
                           "Annotation number: %s\nUser: %s\nchannel: %s\nTitle: %s\n"
                           num user channel title)
                          'lisppaste-paste paste
                          'lisppaste-annotation num
                          'lisppaste-channel channel)
              "\n"))
    (lisppaste-mode)))

(defun lisppaste-create-paste (callback-fn)
  "Create a new paste.

CALLBACK-FN should be a function accepting one argument to send the
paste.  See also `lisppaste-send-paste'."
  (switch-to-buffer (get-buffer-create "*paste*"))
  (erase-buffer)
  (insert lisppaste-creation-help)
  (local-set-key (kbd "C-c C-c") `(lambda ()
                                    (interactive)
                                    (lisppaste-send-paste ,callback-fn))))

(defun lisppaste-send-paste (callback-fn)
  "Send a paste via CALLBACK-FN.

CALLBACK-FN is called with one argument, the contents of the
current-buffer from the end of `lisppaste-creation-help' to
`point-max'."
  (goto-char (point-min))
  (search-forward lisppaste-creation-help)
  (funcall callback-fn (buffer-substring (match-end 0) (point-max)))
  (kill-this-buffer))

(defun lisppaste-create-new-paste (&optional channel nick title)
  "Interactively create a new paste.

CHANNEL, NICK and TITLE are defaults for the paste's channel, nick
and title arguments respectively."
  (interactive)
  (let* ((channel (or channel (lisppaste-read-channel)))
         (nick    (or nick (lisppaste-read-nick channel)))
         (title   (or title (lisppaste-read-title))))
    (lisppaste-create-paste `(lambda (x)
                               (lisppaste-new-paste ,channel ,nick
                                                    ,title x)))))

(defun lisppaste-create-new-annotation (&optional channel nick title n)
  "Interactively annotate a paste.

CHANNEL, NICK, TITLE and N are defaults for the annotations's
channel, nick, title, and paste to annotate respectively."
  (interactive)
  (let* ((channel (or channel (lisppaste-read-channel)))
         (nick    (or nick (lisppaste-read-nick channel)))
         (title   (or title (lisppaste-read-title)))
         (n       (or n (lisppaste-read-number "Paste to annotate: "))))
    (lisppaste-create-paste `(lambda (x)
                               (lisppaste-annotate-paste ,channel ,nick
                                                         ,title x ,n)))))

(defun lisppaste-dwim ()
  "Annotate either the paste or annotation at `point'."
  (interactive)
  (let ((props (text-properties-at (point))))
    (unless (lisppaste-paste props)
      (error "No paste at point"))
    (if (lisppaste-annotation props)
        (lisppaste-display-paste (lisppaste-paste props)
                                 (lisppaste-annotation props))
      (lisppaste-display-paste (lisppaste-paste props)))))

(defun lisppaste-quit ()
  "Quit the current paste buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))

(defun lisppaste-annotate ()
  "Annotate the paste at `point'."
  (interactive)
  (let ((props (text-properties-at (point))))
    (lisppaste-create-new-annotation (lisppaste-channel props)
                                     nil
                                     nil
                                     (lisppaste-paste props))))

(defun lisppaste-help ()
  "Show some help for `lisppaste-mode'."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Lisppaste help*"))
  (erase-buffer)
  (insert "Commands:\n"
          "`a' -- lisppaste-annotate\n"
          "       Annotate the paste at point.  With prefix arg, prompt\n"
          "       for a paste number to annotate.\n"
          "`h' -- lisppaste-help\n"
          "       Show this help.\n"
          "`l' -- lisppaste-list-paste-annotations\n"
          "       List a paste's annotations.\n"
          "`n' -- lisppaste-create-new-paste\n"
          "       Create a new paste.\n"
          "RET -- lisppaste-dwim\n"
          "       Fetch either the paste or the annotation at point.\n"
          "`s' -- lisppaste-display-paste\n"
          "       Fetch a paste.  With prefix arg, fetch an annotation.\n"
          "`q' -- lisppaste-quit\n"
          "       Quit the paste display.\n"))

(defvar lisppaste-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'lisppaste-annotate)
    (define-key map "h" #'lisppaste-help)
    (define-key map "l" #'lisppaste-list-paste-annotations)
    (define-key map "n" #'lisppaste-create-new-paste)
    (define-key map (kbd "RET") #'lisppaste-dwim)
    (define-key map "s" #'lisppaste-display-paste)
    (define-key map "q" #'lisppaste-quit)
    map)
  "Keymap for `lisppaste-mode'.")

(define-derived-mode lisppaste-mode fundamental-mode "Lisppaste"
  "Major mode for viewing and creating IRC pastes via the lisppaste pastebot."
  (setq buffer-read-only t))

(provide 'lisppaste)

;;; lisppaste.el ends here
