;; 18.2 ダイスオブドゥーム、バージョン4
(load "lazy.lisp")

(defparameter *num-players* 4)
(defparameter *max-dice* 5)
(defparameter *board-size* 5)
(defparameter *board-hexnum*
  (* *board-size* *board-size*))
(defparameter *ai-level* 2)
(defparameter *dice-probability*
  #(#(0.84 0.97 1.0 1.0)
    #(0.44 0.78 0.94 0.99)
    #(0.15 0.45 0.74 0.91)
    #(0.04 0.19 0.46 0.72)
    #(0.01 0.06 0.22 0.46)))

;; リストで表現されたゲーム盤を配列表現へと変える
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

;; 初期化
;; ランダムなゲーム盤を作成
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players* (make-random-state t))
                                   (1+ (random *max-dice* (make-random-state t)))))))

(defun player-letter (n)
  (code-char (+ 97 n)))

(defun draw-board (board)
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ "  "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex))
                                   (second hex))))))

;; ゲーム木をすべて作る
(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

;; ゲーム木をメモ化する
(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply old-game-tree rest)))))

;; 相手に手番を渡す
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
    moves
    (lazy-cons (list nil
                     (game-tree (add-new-dice board player
                                              (1- spare-dice))
                                (mod (1+ player) *num-players*)
                                0
                                t))
               moves)))

;; 攻撃の手を計算する
;; 可能な攻撃の指し手をゲーム木に追加する関数
;; 攻撃の手は、攻撃元のマスの番号と攻撃先のマスの番号をリストにしたもの
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
                   (car (aref board pos)))
           (dice (pos)
                 (cadr (aref board pos))))
    (lazy-mapcan
      (lambda (src)
        (if (eq (player src) cur-player)
          (lazy-mapcan
            (lambda (dst)
              (if (and (not (eq (player dst)
                                cur-player))
                       (> (dice src) 1))
                (make-lazy
                  (list (list (list src dst)
                              (game-tree (board-attack board
                                                       cur-player
                                                       src
                                                       dst
                                                       (dice src))
                                         cur-player
                                         (+ spare-dice (dice dst))
                                         nil)
                              (game-tree (board-attack-fail board
                                                       cur-player
                                                       src
                                                       dst
                                                       (dice src))
                                         cur-player
                                         (+ spare-dice (dice dst))
                                         nil))))
                (lazy-nil)))
            (make-lazy (neighbors src)))
          (lazy-nil)))
      (make-lazy (loop for n below *board-hexnum*
                       collect n)))))

;; 攻撃に失敗した時
(defun board-attack-fail (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (if (eq pos src)
                               (list player 1)
                               hex))))

;; サイコロを実際に降る
(defun roll-dice (dice-num)
  (let ((total (loop repeat dice-num
                     sum (1+ (random 6)))))
    (fresh-line)
    (format t "On ~a dice rolled ~a. " dice-num total)
    total))

(defun roll-against (src-dice dst-dice)
  (> (roll-dice src-dice) (roll-dice dst-dice)))

(defun pick-chance-branch (board move)
  (labels ((dice (pos)
                 (cadr (aref board pos))))
    (let ((path (car move)))
      (if (or (null path) (roll-against (dice (car path))
                                        (dice (cadr path))))
        (cadr move)
        (caddr move)))))

;; 隣接したマスを見つける
(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
          when (and (>= p 0) (< p *board-hexnum*))
          collect p)))

(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall old-neighbors pos)))))

(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))
;; (princ (board-attack #((0 3) (0 3) (1 3) (1 1)) 0 1 3 3))

;; 末尾呼び出し最適化版
(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
              (cond ((zerop n) lst)
                    ((null lst) nil)
                    (t (let ((cur-player (caar lst))
                             (cur-dice (cadar lst)))
                         (if (and (eq cur-player player) (< cur-dice *max-dice*))
                           (cons (list cur-player (1+ cur-dice))
                                 (f (cdr lst) (1- n)))
                           (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list) (largest-cluster-size board player)))))
;;(print (add-new-dice #((0 1) (1 3) (0 2) (1 1)) 0 2))
;;(princ (game-tree #((0 1) (1 1) (0 2) (1 1)) 0 0 t))

;; 人間 vs 人間でダイス・オブ・ドゥームをプレイする
(defun available-action (tree)
  (caddr tree))

(defun current-board (tree)
  (cadr tree))

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
    (play-vs-human (handle-human tree))
    (announce-winner (current-board tree))))

(defun current-player (tree)
  (car tree))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (current-player tree)))
  (draw-board (current-board tree)))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (available-action tree)))
    (labels ((print-moves
               (moves n)
               (unless (lazy-null moves)
                 (let* ((move (lazy-car moves))
                        (action (car move)))
                   (fresh-line)
                   (format t "~a. " n)
                   (if action
                     (format t "~a -> ~a" (car action) (cadr action))
                     (princ "end turn")))
                 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun winners (board)
  (let* ((tally (loop for hex across board
                      collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
        (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w))))))

;;(print (gen-board))
;; (play-vs-human (game-tree (gen-board) 0 0 t))

;; 15.4 コンピュータによる対戦相手を作る
;; ミニマックスアルゴリズム
(defun rate-position (tree player)
  (let ((moves (available-action tree)))
    (if (not (lazy-null moves))
      (apply (if (eq (current-player tree) player)
               #'max
               #'min)
             (get-ratings tree player))
      (score-board (current-board tree) player))))

;; rate-position関数をメモ化する
(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
          (setf (gethash tree tab)
                (funcall old-rate-position tree player))))))

(defun get-ratings (tree player)
  (let ((board (current-board tree)))
    (labels ((dice (pos)
                   (cadr (aref board pos))))
      (take-all (lazy-mapcar
                  (lambda (move)
                    (let ((path (car move)))
                      (if path
                        (let* ((src (car path))
                               (dst (cadr path))
                               (probability (aref (aref *dice-probability*
                                                        (1- (dice dst)))
                                                  (- (dice src) 2))))
                          (+ (* probability (rate-position (cadr move) player))
                             (* (- 1 probability) (rate-position (caddr move) player))))
                        (rate-position (cadr move) player))))
                  (available-action tree))))))

;;αβ法による最適化
(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
              (unless (lazy-null moves)
                (let ((x (ab-rate-position (cadr (lazy-car moves))
                                           player
                                           upper-limit
                                           lower-limit)))
                  (if (>= x upper-limit)
                    (list x)
                    (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (available-action tree) lower-limit)))

(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
              (unless (lazy-null moves)
                (let ((x (ab-rate-position (cadr (lazy-car moves))
                                           player
                                           upper-limit
                                           lower-limit)))
                  (if (<= x lower-limit)
                    (list x)
                    (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
    (f (available-action tree) upper-limit)))

(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (available-action tree)))
    (if (not (lazy-null moves))
      (if (eq (current-player tree) player)
        (apply #'max (ab-get-ratings-max  tree
                                          player
                                          upper-limit
                                          lower-limit))
        (apply #'min (ab-get-ratings-min tree
                                         player
                                         upper-limit
                                         lower-limit)))
      (score-board (current-board tree) player))))

;; AIプレイヤーを使うゲームループ
(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
                                     (car tree))))
    (pick-chance-branch
      (current-board tree)
      (lazy-nth (position (apply #'max ratings) ratings)
                (available-action tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (available-action tree)) (announce-winner (current-board tree)))
        ((zerop (current-player tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

;; ゲーム木の刈り込み
(defun limit-tree-depth (tree depth)
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
          (lazy-nil)
          (lazy-mapcar (lambda (move)
                         (cons (car move)
                               (mapcar (lambda (x)
                                         (limit-tree-depth x (1- depth)))
                                       (cdr move))))
                       (available-action tree)))))

(defun score-board (board player)
  (loop for hex across board
        for pos from 0
        sum (if (eq (car hex) player)
              (if (threatened pos board)
                1
                2)
              -1)))

(defun threatened (pos board)
  (let* ((hex (aref board pos))
         (player (car hex))
         (dice (cadr hex)))
    (loop for n in (neighbors pos)
          do (let* ((nhex (aref board n))
                    (nplayer (car nhex))
                    (ndice (cadr nhex)))
               (when (and (not (eq player nplayer)) (> ndice dice))
                 (return t))))))

(defun get-connected (board player pos)
  (labels ((check-pos (pos visited)
                      (if (and (eq (car (aref board pos)) player)
                               (not (member pos visited)))
                        (check-neighbors (neighbors pos) (cons pos visited))
                        visited))
           (check-neighbors (lst visited)
                            (if lst
                              (check-neighbors (cdr lst) (check-pos (car lst) visited))
                              visited)))
    (check-pos pos '())))

(defun largest-cluster-size (board player)
  (labels ((f (pos visited best)
              (if (< pos *board-hexnum*)
                (if (and (eq (car (aref board pos)) player)
                         (not (member pos visited)))
                  (let* ((cluster (get-connected board player pos))
                         (size (length cluster)))
                    (if (> size best)
                      (f (1+ pos) (append cluster visited) size)
                      (f (1+ pos) (append cluster visited) best)))
                  (f (1+ pos) visited best))
                best)))
    (f 0 '() 0)))

;(play-vs-computer (game-tree (gen-board) 0 0 t))
;(play-vs-human (game-tree (gen-board) 0 0 t))
