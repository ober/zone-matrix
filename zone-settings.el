;;; zone-settings.el --- Settings for `zone'

;;; Commentary:

;;; Code:


(defcustom zone-ad-restore nil
  "*A piece of code that should run by \"after\" advice of `zone'."
  :group 'zone)


(defadvice zone (before zone-ad-clean-ui)
  "Clean up the ui before `zone' runs."
  ;;
  ;; Save the current states for future restore.
  ;; Notice the sequence of code execution in the "after" advice
  ;; is the reversion of "before" advice.
  ;;
  ;; the window config
  (add-to-list 'zone-ad-restore
               `(set-window-configuration ,(current-window-configuration)))

  ;; fullfill the window with just one current buffer
  (delete-other-windows)
  ;;
  ;; the state of menu bar, tool bar and tabbar
  (when tabbar-mode
    (tabbar-mode -1)
    (add-to-list 'zone-ad-restore '(tabbar-mode 1)))
  (when scroll-bar-mode
    (scroll-bar-mode -1)
    (add-to-list 'zone-ad-restore '(scroll-bar-mode 1)))
  (when tool-bar-mode
    (tool-bar-mode -1)
    (add-to-list 'zone-ad-restore '(tool-bar-mode 1)))
  (when menu-bar-mode
    (menu-bar-mode -1)
    (add-to-list 'zone-ad-restore '(menu-bar-mode 1)))
  ;;
  ;; Make `zone-ad-restore' a self-disabling one-shot function
  (setq zone-ad-restore `(lambda ()
                           ,@zone-ad-restore
                           (setq zone-ad-restore nil))))


(defadvice zone (after zone-ad-restore-ui)
  "Retore the ui which is cleaned by the \"before\" advice."
  ;; restore the states before `zone' runs.
  (when zone-ad-restore
    (funcall zone-ad-restore)))


(use-package zone
  :defer t
  :config
  (progn
    (require 'zone-matrix)
    (require 'zone-matrix-settings)

    ;; set `zone-matrix' to be the only zone program
    (setq zone-programs [
                         zone-pgm-putz-with-case
                         zone-pgm-random-life
                         zone-matrix
                         ])

    ;; activate advices
    (ad-activate 'zone)

    ;; trigger screen saver when Emacs is idle for a while
    ;; (zone-when-idle (* 60
    ;;                    15 ;; personally I feel 15 minutes is fine
    ;;                    ))
    )
  )

(provide 'zone-settings)

;;; zone-settings.el ends here
