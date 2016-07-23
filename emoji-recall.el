;;; emoji-recall.el --- How many emoji can you recall from memory?  -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2016 DarkSun <lujun9972@gmail.com>

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-07-11
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Keywords: game
;; URL: https://github.com/lujun9972/emoji-recall.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; emoji-recall's code can be found here:
;;   http://github.com/lujun9972/emoji-recall.el

;;; Commentary:

;; emoji-recall is a portable verson of  [[https://itunes.apple.com/app/emoji-recall/id1114387537][emoji-recall]]

;;; Code:

;; 
(defgroup emoji-recall nil
  "Emoji recall game"
  :prefix "emoji-recall-"
  :group 'game)

(defcustom emoji-recall-option-buffer "*emoji-recall-option*"
  "buffer name for the options of emoji-recall"
  :group 'emoji-recall
  :type 'string)

(defcustom emoji-recall-answer-buffer "*emoji-recall-answer*"
  "buffer name for the answers of emoji-recall"
  :group 'emoji-recall
  :type 'string)

(defcustom emoji-recall-pics-dir (concat (if load-file-name
                                             (file-name-directory load-file-name)
                                           default-directory)
                                         "emoji-cheat-sheet/")
  "Directory storing emoji pictures which should be png file"
  :group 'emoji-recall
  :type 'file)

(defvar emoji-recall-round 1
  "Round of the game")

(defcustom emoji-recall-display-interval 3
  "Emoji becomes an asterisk after the number of seconds"
  :group 'emoji-recall
  :type 'number)

(defun emoji-recall-create-emoji-image (emoji)
  "Create an image displaying EMOJI"
  (let ((emoji-file (concat emoji-recall-pics-dir emoji ".png")))
    (create-image emoji-file nil nil)))
(defun emoji-recall-insert-emoji (emoji)
  "Insert an emoji and turn it into an asterisk after certain seconds"
  (with-current-buffer (get-buffer-create emoji-recall-answer-buffer)
    (goto-char (point-max))
    (search-backward-regexp "^Round [0-9]+: \\(.*\\)$")
    (move-end-of-line nil)
    (let* ((emoji-image (emoji-recall-create-emoji-image emoji))
           (start (point))
           end)
      (insert (propertize emoji 'display emoji-image))
      (setq end (point))
      (insert " ")
      (run-at-time emoji-recall-display-interval nil (lambda ()
                                                       (put-text-property start end
                                                                          'display "*" (get-buffer-create emoji-recall-answer-buffer)))))))

(defun emoji-recall-list-all-emojis (emoji-dir)
  "List all emojis stored in EMOJI-DIR"
  (mapcar #'file-name-base (directory-files emoji-dir nil "\\.png$")))

(defun emoji-recall-random-emoji ()
  "Return random emoji stored in `emoji-recall-pics-dir"
  (let* ((emojis (emoji-recall-list-all-emojis emoji-recall-pics-dir))
         (len (length emojis))
         (idx (random len)))
    (nth idx emojis)))

(defun emoji-recall-insert-random-emojis (N)
  "Insert N random emojis"
  (when (> N 0)
    (emoji-recall-insert-emoji (emoji-recall-random-emoji))
    (run-at-time emoji-recall-display-interval nil (lambda ()
                                                     (emoji-recall-insert-random-emojis (- N 1))))))

(defun emoji-recall-draw-question (N)
  "Draw round N question"
  (with-current-buffer (get-buffer-create emoji-recall-answer-buffer)
    (goto-char (point-max))
    (move-beginning-of-line nil)
    (delete-region (point) (point-max))
    (insert (format "Round %d: " N))
    (newline)
    (insert "> ")
    (emoji-recall-insert-random-emojis N)))

(define-button-type 'emoji-recall-answer-button
  'action (lambda (b)
            (delete-region (button-start b)
                           (+ 1 (button-end b)))) ;这里+1是因为每个emoji后面都带个空格
  'follow-link t)

(defun emoji-recall-insert-answer-button (b)
  (let ((label (button-label b))
        (display (button-get b 'display))
        (help-echo (button-get b 'help-echo)))
    (with-current-buffer (get-buffer-create emoji-recall-answer-buffer)
      (goto-char (point-max))
      (insert-text-button label
                          'display display
                          'help-echo help-echo
                          :type 'emoji-recall-answer-button)
      (insert " "))))                   ;这里必须带个空格时因为当相同的emoji靠在一起时,由于display属性相同,Emacs只显示一个emoji image

(define-button-type 'emoji-recall-option-button
  'action #'emoji-recall-insert-answer-button
  'follow-link t)

(defun emoji-recall-insert-option-button (emoji)
  "Insert an option-button with label EMOJI"
  (let ((emoji-image (emoji-recall-create-emoji-image emoji)))
    (with-current-buffer (get-buffer-create emoji-recall-option-buffer)
      (goto-char (point-max))
      (insert-text-button emoji
                          'display emoji-image
                          'help-echo emoji
                          :type 'emoji-recall-option-button)
      (insert " "))))


(defun emoji-recall-draw-options ()
  "Draw all the options"
  (with-current-buffer (get-buffer-create emoji-recall-option-buffer)
    (erase-buffer)
    (mapc #'emoji-recall-insert-option-button
          (emoji-recall-list-all-emojis emoji-recall-pics-dir))))

(defun emoji-recall-get-user-answer ()
  "Geth the user answer"
  (with-current-buffer emoji-recall-answer-buffer
    (goto-char (point-min))
    (search-forward "> ")
    (buffer-substring-no-properties (point) (point-max))))

(defun emoji-recall-get-correct-answer ()
  "Geth the user answer"
  (with-current-buffer emoji-recall-answer-buffer
    (goto-char (point-max))
    (search-backward-regexp "^Round [0-9]+: \\(.+\\)$")
    (match-string-no-properties 1))) ;remove the last newline

(defun emoji-recall-verify-user-answer ()
  (string= (emoji-recall-get-user-answer)
           (emoji-recall-get-correct-answer)))

(define-button-type 'emoji-recall-submit-button
  'action (lambda (b)
            (if (emoji-recall-verify-user-answer)
                (emoji-recall-next-level)
              (emoji-recall-game-over)))
  'follow-link t)

(defun emoji-recall-draw-submit-button ()
  "Draw the submit-button"
  (with-current-buffer (get-buffer-create emoji-recall-option-buffer)
    (goto-char (point-max))
    (insert-text-button "Submit"
                        :type 'emoji-recall-submit-button)))

(defvar emoji-recall-orign-window-configuration nil
  "orign widndow configuration")

;;;###autoload
(defun emoji-recall-game-start ()
  (interactive)
  (setq emoji-recall-round 1)
  (setq emoji-recall-orign-window-configuration (current-window-configuration))
  (switch-to-buffer (get-buffer-create emoji-recall-answer-buffer))
  (erase-buffer)
  (delete-other-windows)
  (split-window-below)
  (windmove-down)
  (switch-to-buffer (get-buffer-create emoji-recall-option-buffer))
  (emoji-recall-draw-question emoji-recall-round)
  (emoji-recall-draw-options)
  (emoji-recall-draw-submit-button))

(defun emoji-recall-next-level ()
  (setq emoji-recall-round (+ 1 emoji-recall-round))
  (emoji-recall-draw-question emoji-recall-round)
  (with-current-buffer emoji-recall-answer-buffer
    (goto-char (point-min))
    (search-forward "> ")
    (delete-region (point) (point-max))))

(defun emoji-recall-game-over ()
  "Game over and show achievements"
  (interactive)
  (switch-to-buffer emoji-recall-answer-buffer)
  (erase-buffer)
  (insert (propertize (format "Game Over! You Got Round %d\nPress any key to quit !" emoji-recall-round) 'display '(height 2)))
  (while (not (input-pending-p))
    (sit-for 0.01))
  (emoji-recall-game-quit))

(defun emoji-recall-game-quit ()
  "Quit game"
  (interactive)
  (kill-buffer emoji-recall-answer-buffer)
  (kill-buffer emoji-recall-option-buffer)
  (when emoji-recall-orign-window-configuration
    (set-window-configuration emoji-recall-orign-window-configuration)
    (setq emoji-recall-orign-window-configuration nil)))

(provide 'emoji-recall)

;;; emoji-recall.el ends here
