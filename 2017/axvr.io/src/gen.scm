#!/usr/local/bin/csi -script

;;; -*- scheme -*-
;;; vim: set ft=scheme :

;;;; Alex's lispy static website generator.
;;;; Public domain.  No rights reserved.


(cond-expand
  (compiling (declare (uses alist)))
  (else (load "src/alist.scm")))

(import (chicken io)
        (chicken irregex)
        (chicken process-context)
        (prefix alist av/))


(define default-attrs
  '(("author" . "Alex Vear")
    ("generator" . "Custom generator written in CHICKEN Scheme")
    ("copyright" . "Copyright (c) 2020, Alex Vear.  Available under the CC BY 4.0.")
    #;("copyright" . "Public domain.  No rights reserved.")
    (".copyright-notice" . "<p>All content on this page is available under the terms of the <a href=\"https://creativecommons.org/licenses/by/4.0/\">CC BY 4.0</a> licence.</p>")
    #;(".copyright-notice" . "<p>All content on this page is dedicated to the <a href=\"/legal/copyright/public-domain\">public domain</a> using the <a title=\"Creative Commons Zero 1.0 Universal\" href=\"https://creativecommons.org/publicdomain/zero/1.0/\">CC0</a> public domain dedication.</p>")
    (".head" . "")))


;;; Successively apply each function in a list to a value.
(define (chain-apply fns val)
    (if (eq? '() fns)
      val
      (chain-apply (cdr fns)
                   ((car fns) val))))


;; TODO: clean up everything in this function.
(define (attributes content)
  "Retrieve attributes from a file."
  (let ((start '(: bol (* whitespace) "<!--" (* whitespace) "METADATA" (* whitespace) eol))
        (end '(: bol (* whitespace) "-->" (* whitespace) eol))
        (pair '(: bol (=> key (*? any)) ":" (* whitespace) (=> value (* any)) (* whitespace) eol)))

    (define (get-pair pairs line)
      (let ((regex-match (irregex-match pair line)))
        (if (irregex-match-data? regex-match)
          (av/assoc pairs
                    (irregex-match-substring regex-match 'key)
                    (irregex-match-substring regex-match 'value))
          pairs)))

    ;; Full page title.
    (define (page-title attrs)
      (let ((title (av/get attrs "title"))
            (author (av/get attrs "author")))
        (av/assoc attrs ".page-title" (if (and (not (eq? '() title))
                                               (> (string-length title) 0))
                                        (string-append title " — " author)
                                        author))))

    ;; HTML meta tags.
    (define (meta-tags attrs)
      (av/assoc attrs
                ".meta-tags"
                (apply string-append
                       (av/map-on
                         (lambda (k v)
                           (string-append "<meta name=\"" k "\" content=\"" v "\">\n"))
                         (av/dissoc
                           (av/map-on-keys
                             (lambda (x)
                               (if (eq? #\. (car (string->list x)))
                                 "PRIVATE"
                                 x))
                             attrs)
                           "PRIVATE")))))

    (define (replace-content-tokens attrs)
      (av/assoc attrs ".content" (inject (av/get attrs ".content") attrs)))

    ;; Page redirects.
    (define (redirect attrs)
      (let ((redirect-uri (av/get attrs ".redirect")))
        (av/assoc attrs
                  ".redirect-tag"
                  (if (eq? redirect-uri '())
                    ""
                    (string-append "<meta http-equiv=\"refresh\" content=\"0;url=" redirect-uri "\"/>")))))

    (define attr-modifiers
      (list page-title
            meta-tags
            redirect
            replace-content-tokens))

    ;; TODO: make this faster.  This entire function is the bottleneck.
    (define (generated-attrs attrs)
      (let ((content2 (apply string-append
                             (map (lambda (x) (string-append x "\n"))
                                  (reverse (cdr (reverse content)))))))
        (chain-apply attr-modifiers
                     (av/assoc attrs ".content" content2))))

    (define (process-line collect line rest searching)
      (cond ((eof-object? line)
             (generated-attrs collect))
            (searching
              (process-line (get-pair collect line)
                            (car rest)
                            (cdr rest)
                            (not (irregex-match? end line))))
            (else
              (process-line collect
                            (car rest)
                            (cdr rest)
                            (irregex-match? start line)))))

    (process-line default-attrs
                  (car content)
                  (cdr content)
                  #f)))


(define (inject line attrs)
  "Replace tokens in a line of text."
  (if (eq? '() attrs)
    line
    (let ((key (caar attrs))
          (val (cdar attrs)))
      (inject (irregex-replace/all
                `(: (or "{{" "%7B%7B") (* whitespace) ,key (* whitespace) (or "}}" "%7D%7D"))
                line
                val)
              (cdr attrs)))))


(define (build-file template-fname target-fname attributes)
  "Build new file from template and inject content/attributes."
  (with-output-to-file
    target-fname
    (lambda ()
      (with-input-from-file
        template-fname
        (lambda ()
          (let loop ((line (read-line)))
            (when (not (eof-object? line))
              (display (inject line attributes))
              (newline)
              (loop (read-line)))))))))


(define (read-stdin)
  (define (get-line collect line)
    (if (eof-object? line)
      (reverse (cons line collect))
      (get-line (cons line collect) (read-line))))
  (get-line '() (read-line)))


(let ((template (car (command-line-arguments)))
      (target (cadr (command-line-arguments)))
      (content (read-stdin)))
  (build-file template target (attributes content)))
