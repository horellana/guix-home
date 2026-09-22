(define-module (my-scripts set-wallpaper)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages imagemagick)
  #:export (random-wallpaper-script random-wallpaper))

;; 1. The Raw Script
(define-public random-wallpaper-script
  (program-file
   "random-wallpaper"
   (with-extensions (list guile-json-4)
     #~(begin
         (use-modules (json)
                      (ice-9 ftw)
                      (ice-9 match)
                      (ice-9 popen)
                      (ice-9 rdelim)
                      (ice-9 regex)
                      (srfi srfi-1)
                      (srfi srfi-26))

         (define wallpaper-dir "/home/hector/images/wallpapers")
         (define cache-file "/home/hector/.cache/random-wallpaper-resolutions.json")
         
         (define (shell-quote s)
           (string-append "'" (string-join (string-split s #\') "'\\''") "'"))

         (define (get-batch-resolutions paths)
           (if (null? paths)
               '()
               (let* ((cmd (string-append #$(file-append imagemagick "/bin/identify")
                                          " -ping -format \"%w|%h|%i\\n\" "
                                          (string-join (map shell-quote paths) " ")))
                      (port (open-input-pipe (string-append cmd " 2>/dev/null")))
                      (results '()))
                 (let loop ((line (read-line port))
                            (results results))
                   (if (eof-object? line)
                       (begin (close-pipe port) (reverse results))
                       (let ((parts (string-split line #\|)))
                         (if (= (length parts) 3)
                             (loop (read-line port)
                                   (cons (list (third parts)
                                               (string->number (first parts))
                                               (string->number (second parts)))
                                         results))
                             (loop (read-line port) results))))))))

         (define (load-cache)
           (catch #t
             (lambda ()
               (if (file-exists? cache-file)
                   (call-with-input-file cache-file
                     (lambda (port) (json->scm port)))
                   '()))
             (lambda (key . args) 
               (format #t "Warning: Could not load cache (~a). Starting fresh.~%" key)
               '())))

         (define (save-cache cache)
           (catch #t
             (lambda ()
               (let* ((dir (dirname cache-file)))
                 (unless (file-exists? dir) (mkdir-p dir))
                 (call-with-output-file cache-file
                   (lambda (port) (scm->json cache port)))))
             (lambda (key . args) 
               (format #t "Error saving cache: ~a ~a~%" key args)
               #f)))

         (define (find-sway-socket)
           (let* ((uid (getuid))
                  (run-dir (string-append "/run/user/" (number->string uid)))
                  (files (scandir run-dir 
                                  (lambda (f) (string-prefix? "sway-ipc" f)))))
             (if (and files (not (null? files)))
                 (string-append run-dir "/" (first files))
                 #f)))

         (define (get-sway-outputs)
           (let* ((port (open-input-pipe (string-append #$(file-append sway "/bin/swaymsg") " -t get_outputs")))
                  (outputs-data (json->scm port)))
             (close-pipe port)
             (filter-map (lambda (output) 
                           (if (assoc-ref output "active")
                               (let ((name (assoc-ref output "name"))
                                     (mode (assoc-ref output "current_mode")))
                                 (if mode
                                     (list name 
                                           (assoc-ref mode "width")
                                           (assoc-ref mode "height"))
                                     #f))
                               #f))
                         (vector->list outputs-data))))

         (define (image-file? filename)
           (let ((down (string-downcase filename)))
             (or (string-suffix? ".jpg" down)
                 (string-suffix? ".png" down)
                 (string-suffix? ".jpeg" down)
                 (string-suffix? ".bmp" down))))

         (define (find-images dir)
           (let ((found '()))
             (ftw dir (lambda (path stat flag)
                        (when (and (eq? flag 'regular)
                                   (image-file? path))
                          (set! found (cons path found)))
                        #t))
             found))

         (define (solve-assignments outputs images initial-cache-alist)
           (let ((cache (make-hash-table (max 100 (length images))))
                 (cache-updated? #f))
             ;; Populate hash table from initial alist
             (for-each (lambda (entry)
                         (hash-set! cache (car entry) (cdr entry)))
                       initial-cache-alist)
             
             (let loop ((remaining-outputs outputs)
                        (remaining-images images)
                        (assignments '()))
               (if (or (null? remaining-outputs) (null? remaining-images))
                   (begin
                     (when cache-updated?
                       (save-cache (hash-fold (lambda (k v res) (cons (cons k v) res)) '() cache)))
                     assignments)
                   (let* ((batch-size (min 100 (length remaining-images)))
                          (batch-images (take remaining-images batch-size))
                          (uncached-images (filter (lambda (img) (not (hash-ref cache img)))
                                                   batch-images))
                          (new-resolutions (if (null? uncached-images)
                                               '()
                                               (get-batch-resolutions uncached-images)))
                          (new-entries (map (lambda (res)
                                              (match res
                                                ((path w h)
                                                 (let ((val (vector w h)))
                                                   (hash-set! cache path val)
                                                   (set! cache-updated? #t)
                                                   (cons path val)))))
                                            new-resolutions)))
                     
                     (let match-loop ((imgs batch-images)
                                      (curr-rem-outputs remaining-outputs)
                                      (curr-assignments assignments))
                       (if (or (null? imgs) (null? curr-rem-outputs))
                           (loop curr-rem-outputs 
                                 (drop remaining-images batch-size)
                                 curr-assignments)
                           (let* ((img (car imgs))
                                  (res (hash-ref cache img)))
                             (if res
                                 (let ((w (vector-ref res 0))
                                       (h (vector-ref res 1)))
                                   (let ((matching-output (find (lambda (o)
                                                                  (match o
                                                                    ((name out-w out-h)
                                                                     (and (>= w out-w) (>= h out-h)))))
                                                                curr-rem-outputs)))
                                     (if matching-output
                                         (match-loop (cdr imgs)
                                                     (delete matching-output curr-rem-outputs)
                                                     (cons (list (car matching-output) 
                                                                 img 
                                                                 w h) 
                                                           curr-assignments))
                                         (match-loop (cdr imgs)
                                                     curr-rem-outputs
                                                     curr-assignments))))
                                 (match-loop (cdr imgs)
                                             curr-rem-outputs
                                             curr-assignments))))))))))

         (let ((socket (find-sway-socket)))
           (if socket
               (begin
                 (setenv "SWAYSOCK" socket)
                 (let* ((images (find-images wallpaper-dir))
                        (outputs (get-sway-outputs)))
                   
                   (if (null? images)
                       (format #t "No images found in ~a~%" wallpaper-dir)
                       (begin
                         (set! *random-state* (random-state-from-platform))
                         (let* ((shuffled-images (map cdr (sort (map (lambda (x) (cons (random 1000000) x)) images)
                                                               (lambda (a b) (< (car a) (car b))))))
                                (cache (load-cache))
                                (assignments (solve-assignments outputs shuffled-images cache)))
                           
                           (if (not (null? assignments))
                               (let* ((commands (map (lambda (a)
                                                       (match a
                                                         ((name path w h)
                                                          (begin
                                                            (format #t "Setting ~a to ~a (~ax~a)~%" name (basename path) w h)
                                                            (format #f "output ~a bg ~s fill" name path)))))
                                                     assignments))
                                      (combined-cmd (string-join commands "; ")))
                                 (system* #$(file-append sway "/bin/swaymsg") combined-cmd))))))))
               (format #t "Could not find running Sway socket.~%")))))))

;; 2. The Package Wrapper
(define-public random-wallpaper
  (package
   (name "random-wallpaper")
   (version "0.11")
   (source #f)
   (build-system trivial-build-system)
   (inputs (list guile-json-4 imagemagick))
   (arguments
    (list
     #:modules '((guix build utils))
     #:builder
     #~(begin
	 (use-modules (guix build utils))
	 (let ((bin (string-append #$output "/bin")))
	   (mkdir-p bin)
	   (copy-file #$random-wallpaper-script (string-append bin "/random-wallpaper"))
	   #t))))
   (synopsis "Random wallpaper script")
   (description "Sets a random wallpaper using swaymsg, filtering for resolution with caching.")
   (home-page #f)
   (license #f)))

random-wallpaper-script
