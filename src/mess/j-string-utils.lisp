
(in-package :j-string-utils)

(tokenize "(abc ; (adfa)) ska "
	  :stop ";" :open "(" :close ")" :assert-match nil
	  :stop-while-open nil :singlets ";")

(tokenize ""
	  :stop ";" :open "(" :close ")" :assert-match nil
	  :stop-while-open nil :singlets ";")