(in-package #:cl-glut)

(defcenum profile
  (:core-profile 1)
  :compatibility-profile)

(defcfun ("glutInitContextProfile" init-context-profile) :void
  (flags profile))

(defmethod display-window :around ((win window))
  (when (game-mode win)
    (setup-game-mode win))
  (init-window-position (pos-x win) (pos-y win))
  (init-window-size (width win) (height win))
  (without-fp-traps
    (apply #'init-display-mode (slot-value win 'mode))
    (init-context-profile :core-profile)
    (setf (slot-value win 'id)
          (if (game-mode win)
              (enter-game-mode)
              (create-window (title win))))
    (call-next-method)
    (when *run-main-loop-after-display*
      (glut:main-loop))))



