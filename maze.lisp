(in-package #:maze)

(defstruct maze-cell
  "A structure representing a maze cell.  A maze cell can have a south and east
wall. North and west walls are handled by adjacent cell's south and east walls."
  (wall-south t)
  (wall-east t)
  (visited-p nil))

(defun maze-init (width height)
  "Initialize a maze object.  A maze object is represented by a 2d array of
maze-cells."
  (let ((maze (make-array (list width height) :initial-element nil)))
    (loop for x from 0 below width
       do (loop for y from 0 below height
             do(setf (maze-cell maze x y) (make-maze-cell))))
    maze))

(defun maze-width (maze)
  "Get the width of a maze."
  (array-dimension maze 0))

(defun maze-height (maze)
  "Get the height of a maze."
  (array-dimension maze 1))

(defun maze-cell (maze x y)
  "Gets a maze-cell at position x, y in a maze object."
  (aref maze x y))

(defun (setf maze-cell) (value maze x y)
  "Sets a maze-cell at position x, y in a maze object"
  (setf (aref maze x y) value))

(defun maze-cell-in-bounds-p (maze x y)
  (and (>= x 0)(>= y 0)
       (< x (maze-width maze))
       (< y (maze-height maze))))

(defun clear-wall (maze x y side)
  "Clears walls at cell position x, y on the given side.  Side can be NORTH,
SOUTH, EAST or WEST."
  (ecase side
    (north (setf (maze-cell-wall-south (maze-cell maze x (1- y))) nil))
    (east (setf (maze-cell-wall-east (maze-cell maze x y)) nil))
    (south (setf (maze-cell-wall-south (maze-cell maze x y)) nil))
    (west (setf (maze-cell-wall-east (maze-cell maze (1- x) y)) nil))))

(defun shuffle (seq)
  "Knuth shuffle the given sequence."
  (let ((n (length seq)))
    (dotimes (i n seq)
      (rotatef (elt seq i)
               (elt seq (+ i (random (- n i))))))))

(defun maze-neighbors (maze x y)
  "Get all neighbors of a cell at position x, y and the direction traveled to
get there."
  (loop for (ox oy) in '((0 1) (0 -1) (1 0) (-1 0))
     for direction in '(south north east west)
     for nx = (+ x ox)
     for ny = (+ y oy)
     when (maze-cell-in-bounds-p maze nx ny)
     collect (list nx ny direction)))

(defun make-maze (width height)
  "Makes a maze."
  (let ((maze (maze-init width height))
        (x 0)
        (y 0))
    (labels ((tunnel (x y)
               (setf (maze-cell-visited-p (maze-cell maze x y)) t)
               (loop for neighbor in (shuffle (maze-neighbors maze x y))
                  do (destructuring-bind (nx ny direction)
                         neighbor
                       (unless (maze-cell-visited-p (maze-cell maze nx ny))
                         (clear-wall maze x y direction)
                         (tunnel nx ny))))))
      (tunnel x y))
    maze))

(defun print-maze (maze)
  "Prints a maze to stdout."
  (loop for x from 0 below (maze-width maze)
     when (= x 0) do (princ "  ") ;; north entrance
     ;; north wall
     else do (if (and (= x 1)
                      (maze-cell-wall-east (maze-cell maze (1- x) 0)))
                 (princ " _")
                 (princ "__")))
  (terpri)
  (loop for y from 0 below (maze-height maze)
     do (progn (princ "|") ;; west wall
               (loop for x from 0 below (maze-width maze)
                  do (progn (if (and (maze-cell-wall-south (maze-cell maze x y))
                                     (not (and (= x (1- (maze-width maze)))
                                               (= y (1- (maze-height maze))))))
                                (princ "_") ;; south wall
                                (princ " "))
                            (if (maze-cell-wall-east (maze-cell maze x y))
                                (princ "|") ;; east wall
                                (if (and (maze-cell-wall-south (maze-cell maze x y))
                                         (maze-cell-in-bounds-p maze (1+ x) y)
                                         (maze-cell-wall-south (maze-cell maze (1+ x) y)))
                                    (princ "_")
                                    (princ " ")))))
               (terpri))))





