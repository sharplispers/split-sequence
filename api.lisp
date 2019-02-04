;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :split-sequence)

(declaim (inline list-long-enough-p check-bounds check-tests))

(defun list-long-enough-p (list length)
  (or (zerop length)
      (not (null (nthcdr (1- length) list)))))

(defun check-bounds (sequence start end)
  (progn
    (check-type start unsigned-byte "a non-negative integer")
    (check-type end (or null unsigned-byte) "a non-negative integer or NIL")
    (typecase sequence
      (list
       (when end
         (unless (list-long-enough-p sequence end)
           (error "The list is too short: END was ~S but the list is ~S elements long."
                  end (length sequence)))))
      (t
       (let ((length (length sequence)))
         (unless end (setf end length))
         (unless (<= start end length)
           (error "Wrong sequence bounds. START: ~S END: ~S" start end)))))))

(defun check-tests (test test-p test-not test-not-p)
  (when (and test test-p test-not test-not-p)
    (error "Cannot specify both TEST and TEST-NOT."))
  (when (and test-not-p (null test-not) (not test-p))
    (error 'type-error :datum test :expected-type '(or function (and symbol (not null)))))
  (when (and test-p (null test) (not test-not-p))
    (error 'type-error :datum test :expected-type '(or function (and symbol (not null))))))

(declaim (ftype (function (&rest t) (values list unsigned-byte))
                split-sequence split-sequence-if split-sequence-if-not))

(defun split-sequence (delimiter sequence &key (start 0) (end nil) (from-end nil)
                                            (count nil) (remove-empty-subseqs nil)
                                            (test #'eql test-p) (test-not nil test-not-p)
                                            (key #'identity))
  (check-bounds sequence start end)
  (check-tests test test-p test-not test-not-p)
  (etypecase sequence
    (list (split-list delimiter sequence start end from-end count
                      remove-empty-subseqs test test-not key))
    (vector (split-vector delimiter sequence start end from-end count
                          remove-empty-subseqs test test-not key))
    #+(or abcl sbcl)
    (extended-sequence (split-extended-sequence delimiter sequence start end from-end count
                                                remove-empty-subseqs test test-not key))))

(defun split-sequence-if (predicate sequence &key (start 0) (end nil) (from-end nil)
                                               (count nil) (remove-empty-subseqs nil) (key #'identity))
  (check-bounds sequence start end)
  (etypecase sequence
    (list (split-list-if predicate sequence start end from-end count
                         remove-empty-subseqs key))
    (vector (split-vector-if predicate sequence start end from-end count
                             remove-empty-subseqs key))
    #+(or abcl sbcl)
    (extended-sequence (split-extended-sequence-if predicate sequence start end from-end count
                                                   remove-empty-subseqs key))))

(defun split-sequence-if-not (predicate sequence &key (start 0) (end nil) (from-end nil)
                                                   (count nil) (remove-empty-subseqs nil) (key #'identity))
  (check-bounds sequence start end)
  (etypecase sequence
    (list (split-list-if-not predicate sequence start end from-end count
                             remove-empty-subseqs key))
    (vector (split-vector-if-not predicate sequence start end from-end count
                                 remove-empty-subseqs key))
    #+(or abcl sbcl)
    (extended-sequence (split-extended-sequence-if-not predicate sequence start end from-end count
                                                       remove-empty-subseqs key))))

(pushnew :split-sequence *features*)
