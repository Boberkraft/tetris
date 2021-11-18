# Tetris

This is a project to test common lisp with OpenGL 


## How to start?

Turn on Common lisp:

> sbcl

```common-lisp
;; add path to asdf so it can be localized be quicklisp
;; or you can just put the git repo to ~/quicklisp/local-projects/
(push #p"C:/Users/user/Desktop/PATH-TO-REPO/" asdf:*central-registry*)

(ql:quickload :testing-rendering) ; load up
(play :start)
```

### Multiplayer added

(now you can play versus yourself, or something.)
![animation](readme-images/image4.gif)

### Third iteration

(added option to have draw multiple games at once.)
![animation](readme-images/image3.gif)

### Second iteration
![animation](readme-images/image2.gif)

### First iteration
![animation](readme-images/image.gif)
