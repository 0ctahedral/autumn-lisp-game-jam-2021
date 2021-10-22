(asdf:defsystem :froggy
	:description "My Autumn Lisp Game Jam 2021 entry"
	:author "Morgan Rosenkranz"
    :depends-on (trivial-gamekit
                  trivial-gamekit-input-handler
                  cl-bodge/physics
                  cl-bodge/physics/2d)
  :serial t
  :pathname "src/")

(asdf:load-system :froggy)
