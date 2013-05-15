;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2013
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (nala plot)
  #:use-module (ice-9 popen)
  #:export (plot plot*))

(define-syntax-rule (-> exp) (format #f "~a(~{~a~^,~})" (car 'exp) (cdr 'exp)))

(define (do-func-plot gnuplot xmin xmax ymin ymax func brush)
  (format gnuplot "plot ")
  (and xmin (format gnuplot "[~a:" xmin))
  (if xmax (format gnuplot "~a] " xmax) (and xmin (format gnuplot "] ")))
  (and ymin (format gnuplot "[~a:" ymin))
  (if ymax (format gnuplot "~a] " ymax) (and ymin (format gnuplot "] ")))
  (format gnuplot "~a " func)
  (and brush (format gnuplot "with ~a " brush)))

(define (do-file-plot gnuplot file from to title brush)
  (format gnuplot "plot ~s " file)
  (and from to (format gnuplot "using [~a:~a] " from to))
  (and title (format gnuplot "title ~a " title))
  (and brush (format gnuplot "with ~a " brush)))

(define (make-gnuplot config)
  (let ((p (open-input-output-pipe "gnuplot\n")))
    (for-each (lambda (c)
                (format p "~a~%" c))
              config)
    p))

(define (do-arr-plot gnuplot arr from to title brush)
  (do-file-plot gnuplot "-" from to title brush)
  (format gnuplot "~ae" (gen-data arr)))  

(define (run gnuplot)
  (format gnuplot "~%~!"))

(define* (plot* #:optional (config '())
                #:key (xmin #f) (xmax #f) (ymin #f) (ymax #f)
                (func #f) (brush #f) (from #f) (to #f) (title #f)
                (arr #f) (file #f))
  (define gnuplot (make-gnuplot config))
  (cond
   (func (do-func-plot gnuplot xmin xmax ymin ymax func brush))
   (file (do-file-plot gnuplot file from to title brush))
   (arr (do-arr-plot gnuplot arr from to title brush))
   (else (error "none sense!")))
  (run gnuplot))

(define (gen-data lst)
  (call-with-output-string
   (lambda (port)
     (apply for-each (lambda e
                       (format port "~{~a~^ ~}~%" e))
            lst))))

(define-syntax plot
  (syntax-rules (with title using)
    ((_ [xmin xmax] [ymin ymax] exp with what config ...)
     (plot* config ... #:xmin xmin #:xmax xmax #:ymin ymin #:ymax ymax #:func (-> exp) #:brush 'what))
    ((_ exp with what config ...)
     (plot* config ... #:func (-> exp) #:brush 'what))
    ((_ exp config ...)
     (plot* config ... #:func (-> exp)))
    ((_ file using [from to] title name with what config ...)
     (plot* config ... #:from from #:to to #:title 'name #:brush 'what #:file file))
    ((_ file using [from to] with what config ...)
     (plot* config ... #:file file #:brush 'what #:from from #:to to))
    ((_ file using [from to] title name config ...)
     (plot* config ... #:file file #:title 'name #:from from #:to to))
    ((_ file title name config ...)
     (plot* config ... #:file file #:title 'name))
    ((_ file with what config ...)
     (plot* config ... #:file file #:brush 'what))))
