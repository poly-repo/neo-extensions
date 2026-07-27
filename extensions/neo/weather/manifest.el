;;; -*- lexical-binding: t -*-
(neo/extension
 :name "weather"
 :title "Sunshine on demand"
 :publisher "neo"
 :description "Check the forecast without looking out the window."
 :categories (neo)
 :keywords (weather forecast)
 :requires ()
 :repository (
              :type "git"
              :url "https://github.com/poly-repo/neo-extensions.git"
              :path "extensions/neo/weather"))
