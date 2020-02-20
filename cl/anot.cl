(defvar bin "/opt/ffmpeg-git-20190925-i686-static/ffmpeg")
(defvar quality "0")

(defun ~>s (x) (format nil "~A" x))

(defun prep-text (text &optional (acc ""))
  (if (= (length text) 0) acc
    (prep-text (subseq text 1)
               (concatenate 'string acc (cond ((eql (char text 0) #\') "’")
                                              (t (subseq text 0 1)))))))

(defun run (bin args) 
  (format t "> ~A~%" (reduce (lambda(a v) (format nil "~A ~A" a v))
                           (cons bin args)))
  (sb-ext:run-program bin args :output *standard-output*))

(defun make-piece (source start duration text outname)
  (run bin `("-y" "-v" "warning" "-stats" "-i" ,source ,@(if (> start 0) `("-ss" ,(~>s start)))
                                    ,@(if (> duration 0) `("-t" ,(~>s duration)))
                                    "-vf" ,(format nil "drawtext=text='~A':fontcolor=white:fontsize=25:x=650:y=40"
                                                   (prep-text text))
                                    "-q:v" ,quality ,outname)))

(defun normal-cut (source start duration outname)
  (run bin `("-y" "-v" "warning" "-stats" "-i" ,source ,@(if (> start 0) `("-ss" ,(~>s start)))
                                    ,@(if duration `("-t" ,(~>s duration))) "-q:v" ,quality ,outname)))

(defun connect (names)
  (run bin `("-y" "-v" "warning" "-stats" "-i" ,(format nil "concat:~A"
                                    (reduce (lambda (a v) (format nil "~A|~A" a v))
                                          names))
                    "-q:v" ,quality "vid.avi")))

(let (names)
  (defun nextname ()
    (let ((ans (format nil "~~piece~A.avi" (length names))))
      (setf names (cons ans names))
      ans))
  (defun allnames ()
    (reverse names)))

(let ((input (read)))
  (loop for section in input
        do (let ((file (first section))
                 (data (rest section))
                 (pos 0))
           (loop for x in data
                 do (progn (normal-cut file pos (- (first x) pos) (nextname))
                           (make-piece file (first x) (second x) (third x) (nextname))
                           (setf pos (+ (first x) (second x)))))
           (normal-cut file pos nil (nextname)))))

(connect (allnames))
