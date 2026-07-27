;;; test-neo-weather.el --- Tests for neo-weather -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'cl-lib)

(defconst neo--weather-test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing `test-neo-weather.el'.")

(defvar sunshine-appid nil)
(defvar sunshine-show-icons nil)
(defvar sunshine-buffer-name "*Sunshine Test*")
(defvar calendar-latitude nil)
(defvar calendar-longitude nil)
(defvar neo--weather-test-response nil)
(defvar neo--weather-test-display-error-count 0)

(defface sunshine-forecast-headline-face
  '((t (:foreground "navajo white" :height 1.5)))
  "Upstream-like Sunshine headline face used by the tests.")

(defface sunshine-forecast-date-face
  '((t (:weight ultra-bold :foreground "white")))
  "Upstream-like Sunshine date face used by the tests.")

(defmacro neo/use-package (_name &rest arguments)
  "Apply the custom and config forms from ARGUMENTS."
  (let ((custom-tail (cdr (memq :custom arguments)))
        custom-forms)
    (while (and custom-tail
                (not (keywordp (car custom-tail))))
      (push (pop custom-tail) custom-forms))
    `(progn
       ,@(mapcar (lambda (form)
                   `(setq ,(car form) ,(cadr form)))
                 (nreverse custom-forms))
       ,@(cdr (memq :config arguments)))))

(defun sunshine-make-url (_location _units _appid)
  "Return the legacy URL marker used by the test double."
  "legacy")

(defun sunshine-build-simple-forecast (forecast)
  "Return a simple marker containing FORECAST."
  (list 'built forecast))

(defun sunshine-retrieved (_status _display-type)
  "Build the configured test response."
  (sunshine-build-simple-forecast neo--weather-test-response))

(defun sunshine-display-error ()
  "Record that Sunshine's error display was requested."
  (setq neo--weather-test-display-error-count
        (1+ neo--weather-test-display-error-count)))

(cl-letf (((symbol-function 'auth-source-pick-first-password)
           (lambda (&rest _arguments)
             "test-appid")))
  (load-file
   (expand-file-name "../neo-weather.el" neo--weather-test-dir)))

(defun neo--weather-test-valid-response ()
  "Return a minimal valid OpenWeather forecast response."
  (copy-tree
   '((city
      (name . "Pittsburgh")
      (country . "US"))
     (list .
           [((dt . 1770000000)
             (weather .
                      [((main . "Clear")
                        (icon . "01d"))])
             (main
              (temp_min . 55.0)
              (temp_max . 65.0)
              (pressure . 1015)))]))
   t))

(describe "neo-weather URL compatibility"
  (it "builds an HTTPS coordinate forecast URL"
    (let ((calendar-latitude 40.4406)
          (calendar-longitude -79.9959))
      (expect
       (sunshine-make-url "ignored" 'metric "test-appid")
       :to-equal
       (concat
        "https://api.openweathermap.org/data/2.5/forecast"
        "?lat=40.4406&lon=-79.9959&APPID=test-appid"
        "&mode=json&units=metric&cnt=5"))))

  (it "rejects missing coordinates"
    (let ((calendar-latitude nil)
          (calendar-longitude -79.9959))
      (expect (sunshine-make-url "ignored" 'metric "test-appid")
              :to-throw
              'user-error)))

  (it "rejects out-of-range coordinates"
    (let ((calendar-latitude 40.4406)
          (calendar-longitude 181))
      (expect (sunshine-make-url "ignored" 'metric "test-appid")
              :to-throw
              'user-error)))

  (it "rejects missing credentials without exposing a value"
    (let ((calendar-latitude 40.4406)
          (calendar-longitude -79.9959))
      (expect (sunshine-make-url "ignored" 'metric "")
              :to-throw
              'user-error))))

(describe "neo-weather face compatibility"
  (it "makes the headline foreground theme-aware without changing its height"
    (expect
     (face-attribute 'sunshine-forecast-headline-face :foreground nil nil)
     :to-be
     'unspecified)
    (expect
     (face-attribute 'sunshine-forecast-headline-face :inherit nil nil)
     :to-be
     'font-lock-function-name-face)
    (expect
     (face-attribute 'sunshine-forecast-headline-face :height nil nil)
     :to-equal
     1.5))

  (it "makes the date foreground theme-aware without changing its weight"
    (expect
     (face-attribute 'sunshine-forecast-date-face :foreground nil nil)
     :to-be
     'unspecified)
    (expect
     (face-attribute 'sunshine-forecast-date-face :inherit nil nil)
     :to-be
     'font-lock-keyword-face)
    (expect
     (face-attribute 'sunshine-forecast-date-face :weight nil nil)
     :to-be
     'ultra-bold)))

(describe "neo-weather forecast compatibility"
  (it "accepts a valid response before calling Sunshine"
    (let ((response (neo--weather-test-valid-response)))
      (expect (sunshine-build-simple-forecast response)
              :to-equal
              (list 'built response))))

  (it "rejects a JSON boolean"
    (expect (sunshine-build-simple-forecast t)
            :to-throw
            'neo--weather-invalid-response))

  (it "rejects a JSON null response"
    (expect (sunshine-build-simple-forecast nil)
            :to-throw
            'neo--weather-invalid-response))

  (it "rejects an OpenWeather API error response"
    (expect
     (sunshine-build-simple-forecast
      '((cod . 401)
        (message . "credential-shaped response text")))
     :to-throw
     'neo--weather-invalid-response))

  (it "rejects a malformed city"
    (let ((response (neo--weather-test-valid-response)))
      (setf (alist-get 'city response) t)
      (expect (sunshine-build-simple-forecast response)
              :to-throw
              'neo--weather-invalid-response)))

  (it "rejects a non-vector forecast list"
    (let ((response (neo--weather-test-valid-response)))
      (setf (alist-get 'list response) '(((dt . 1770000000))))
      (expect (sunshine-build-simple-forecast response)
              :to-throw
              'neo--weather-invalid-response)))

  (it "turns an invalid async response into Sunshine's error display"
    (let ((neo--weather-test-response t)
          (neo--weather-test-display-error-count 0)
          captured-message)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (setq captured-message
                         (apply #'format format-string arguments)))))
        (expect (sunshine-retrieved nil 'quick) :to-be nil)
        (expect neo--weather-test-display-error-count :to-equal 1)
        (expect captured-message
                :to-equal
                "Sunshine: OpenWeather returned an invalid forecast response.")))))

(provide 'test-neo-weather)
;;; test-neo-weather.el ends here
