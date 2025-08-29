;;; -*- lexical-binding: t -*-

(setq default-frame-alist
      (seq-union default-frame-alist
		 '((fullscreen . maximized)
		   (vertical-scroll-bars . nil)
		   (horizontal-scroll-bars . nil)
		   (menu-bar-lines . 0)
		   (tool-bar-lines . 0)
		   )))
