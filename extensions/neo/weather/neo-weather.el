;;; -*- lexical-binding: t -*-

;;; This is weather, a NEO extension
;;;
;;; Sunshine on demand

(require 'auth-source)
(require 'cl-lib)

(declare-function sunshine-display-error "sunshine")

(define-error 'neo--weather-invalid-response
  "OpenWeather returned an invalid forecast response"
  'user-error)

(defun neo--weather-coordinate-valid-p (coordinate minimum maximum)
  "Return non-nil when COORDINATE is a number between MINIMUM and MAXIMUM."
  (and (numberp coordinate)
       (<= minimum coordinate maximum)))

(defun neo--weather-make-url (_location units appid)
  "Build an HTTPS OpenWeather forecast URL for UNITS and APPID.
Use `calendar-latitude' and `calendar-longitude' instead of LOCATION."
  (let ((latitude (and (boundp 'calendar-latitude) calendar-latitude))
        (longitude (and (boundp 'calendar-longitude) calendar-longitude)))
    (unless (neo--weather-coordinate-valid-p latitude -90 90)
      (user-error "Sunshine requires a valid `calendar-latitude'"))
    (unless (neo--weather-coordinate-valid-p longitude -180 180)
      (user-error "Sunshine requires a valid `calendar-longitude'"))
    (unless (and (stringp appid)
                 (string-match-p "\\S-" appid))
      (user-error "Sunshine OpenWeather credentials are not configured"))
    (unless (symbolp units)
      (user-error "Sunshine units must be a symbol"))
    (format
     (concat "https://api.openweathermap.org/data/2.5/forecast"
             "?lat=%s&lon=%s&APPID=%s&mode=json&units=%s&cnt=5")
     (url-hexify-string (number-to-string latitude))
     (url-hexify-string (number-to-string longitude))
     (url-hexify-string appid)
     (url-hexify-string (symbol-name units)))))

(defun neo--weather-alist-p (value)
  "Return non-nil when VALUE is a non-empty alist."
  (and (consp value)
       (cl-every #'consp value)))

(defun neo--weather-non-empty-string-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value)
       (string-match-p "\\S-" value)))

(defun neo--weather-forecast-day-valid-p (day)
  "Return non-nil when DAY contains every field Sunshine reads."
  (let* ((weather (and (neo--weather-alist-p day)
                       (alist-get 'weather day)))
         (condition (and (vectorp weather)
                         (> (length weather) 0)
                         (aref weather 0)))
         (measurements (and (neo--weather-alist-p day)
                            (alist-get 'main day))))
    (and (numberp (and (neo--weather-alist-p day)
                       (alist-get 'dt day)))
         (neo--weather-alist-p condition)
         (neo--weather-non-empty-string-p (alist-get 'main condition))
         (neo--weather-non-empty-string-p (alist-get 'icon condition))
         (neo--weather-alist-p measurements)
         (numberp (alist-get 'temp_min measurements))
         (numberp (alist-get 'temp_max measurements))
         (numberp (alist-get 'pressure measurements)))))

(defun neo--weather-forecast-valid-p (forecast)
  "Return non-nil when FORECAST has the shape expected by Sunshine."
  (let* ((city (and (neo--weather-alist-p forecast)
                    (alist-get 'city forecast)))
         (days (and (neo--weather-alist-p forecast)
                    (alist-get 'list forecast))))
    (and (neo--weather-alist-p city)
         (neo--weather-non-empty-string-p (alist-get 'name city))
         (neo--weather-non-empty-string-p (alist-get 'country city))
         (vectorp days)
         (> (length days) 0)
         (cl-every #'neo--weather-forecast-day-valid-p days))))

(defun neo--weather-validate-forecast (forecast)
  "Return FORECAST when Sunshine can safely consume it.
Signal `neo--weather-invalid-response' for malformed OpenWeather responses."
  (unless (neo--weather-forecast-valid-p forecast)
    (signal 'neo--weather-invalid-response nil))
  forecast)

(defun neo--weather-build-simple-forecast (original forecast)
  "Call ORIGINAL with FORECAST after validating the decoded response."
  (funcall original (neo--weather-validate-forecast forecast)))

(defun neo--weather-retrieved (original status display-type)
  "Call Sunshine's ORIGINAL callback with STATUS and DISPLAY-TYPE.
Convert invalid OpenWeather responses into Sunshine's normal error display."
  (condition-case nil
      (funcall original status display-type)
    (neo--weather-invalid-response
     (message "Sunshine: OpenWeather returned an invalid forecast response.")
     (condition-case nil
         (sunshine-display-error)
       (error nil))
     nil)))

(defun neo--weather-configure-faces ()
  "Make Sunshine forecast faces inherit theme-aware foreground colors."
  (set-face-attribute
   'sunshine-forecast-headline-face nil
   :foreground 'unspecified
   :inherit 'font-lock-function-name-face)
  (set-face-attribute
   'sunshine-forecast-date-face nil
   :foreground 'unspecified
   :inherit 'font-lock-keyword-face))

(neo/use-package sunshine
  :custom
  (sunshine-show-icons t)
  (sunshine-appid
   (auth-source-pick-first-password
    :host "openweather"
    :user "token"))
  :config
  (neo--weather-configure-faces)
  (advice-remove 'sunshine-make-url #'neo--weather-make-url)
  (advice-add 'sunshine-make-url :override #'neo--weather-make-url)
  (advice-remove 'sunshine-build-simple-forecast
                 #'neo--weather-build-simple-forecast)
  (advice-add 'sunshine-build-simple-forecast
              :around
              #'neo--weather-build-simple-forecast)
  (advice-remove 'sunshine-retrieved #'neo--weather-retrieved)
  (advice-add 'sunshine-retrieved :around #'neo--weather-retrieved))

;;; Note, no (provide 'neo-weather) here, extensions are loaded not required.
