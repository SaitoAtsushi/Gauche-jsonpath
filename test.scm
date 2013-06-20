;;;
;;; Test json-path
;;;

(use gauche.test)
(use rfc.json)

(test-start "sjsonpath")
(use sjsonpath)
(test-module 'sjsonpath)

(define test-object-1
  (parse-json-string "{ \"a\": \"a\", \"b\": \"b\", \"c d\": \"e\" }"))

(test* "request elements to simple object."
       '(("a") ("a") ("e") ("a" "b" "e") ("a" "b" "e") ("a" "b" "e"))
       (map (lambda(x) ((sjsonpath x) test-object-1))
            '("$.a" "$['a']" "$.'c d'" "$.*" "$['*']" "$[*]")))

(define test-object-2
  (parse-json-string "[ 1, \"2\", 3.14, true, null ]"))

(test* "request elements to simple array."
       '((1) (null) (1 "2" 3.14 true null) (null))
       (map (lambda(x) ((sjsonpath x) test-object-2))
            '("$[0]" "$[4]" "$[*]" "$[-1:]")))

(define test-object-3
  (parse-json-string "{\"points\": [
     {\"id\": \"i1\", \"x\":  4, \"y\": -5 },
     {\"id\": \"i2\", \"x\": -2, \"y\":  2, \"z\": 1 },
     {\"id\": \"i3\", \"x\":  8, \"y\":  3 },
     {\"id\": \"i4\", \"x\": -6, \"y\": -1 },
     {\"id\": \"i5\", \"x\":  0, \"y\":  2, \"z\": 1 },
     {\"id\": \"i6\", \"x\":  1, \"y\":  4 }
    ] }"))

(test* "request elements to nested object."
       '(((("id" . "i2") ("x" . -2) ("y" . 2) ("z" . 1)))
         (0)
         (-6)
         (4 -2 8 -6 0 1)
         ("i3")
         ("i2" "i5")
         ("i6"))
       (map (lambda(x) ((sjsonpath x) test-object-3))
            '("$.points[1]"
              "$.points[4].x"
              "$.points[?(equal? (assoc-ref @ \"id\" #f) \"i4\")].x"
              "$.points[*].x"
              "$['points'][?(> (+ (* (assoc-ref @ \"x\" 0) (assoc-ref @ \"x\" 0)) (* (assoc-ref @ \"y\") (assoc-ref @ \"y\"))) 50)].id"
              "$.points[?(assoc-ref @ \"z\")].id"
              "$.points[(- (vector-length @) 1)].id")))

(define test-object-4
  (parse-json-string "{ \"menu\": {
    \"header\": \"SVG Viewer\",
    \"items\": [
      {\"id\": \"Open\"},
      {\"id\": \"OpenNew\", \"label\": \"Open New\"},
      null,
      {\"id\": \"ZoomIn\", \"label\": \"Zoom In\"},
      {\"id\": \"ZoomOut\", \"label\": \"Zoom Out\"},
      {\"id\": \"OriginalView\", \"label\": \"Original View\"},
      null,
      {\"id\": \"Quality\"},
      {\"id\": \"Pause\"},
      {\"id\": \"Mute\"},
      null,
      {\"id\": \"Find\", \"label\": \"Find...\"},
      {\"id\": \"FindAgain\", \"label\": \"Find Again\"},
      {\"id\": \"Copy\"},
      {\"id\": \"CopyAgain\", \"label\": \"Copy Again\"},
      {\"id\": \"CopySVG\", \"label\": \"Copy SVG\"},
      {\"id\": \"ViewSVG\", \"label\": \"View SVG\"},
      {\"id\": \"ViewSource\", \"label\": \"View Source\"},
      {\"id\": \"SaveAs\", \"label\": \"Save As\"},
      null,
      {\"id\": \"Help\"},
      {\"id\": \"About\", \"label\": \"About Adobe CVG Viewer...\"}
    ]
   }}"))

(test* "request elements to nested object that include null."
       '(("Open" "Quality" "Pause" "Mute" "Copy" "Help")
         ("CopySVG" "ViewSVG")
         ((("id" . "Open"))
          (("id" . "OpenNew") ("label" . "Open New"))
          (("id" . "ZoomIn") ("label" . "Zoom In"))
          (("id" . "ZoomOut") ("label" . "Zoom Out"))
          (("id" . "OriginalView") ("label" . "Original View"))
          (("id" . "Quality"))
          (("id" . "Pause"))
          (("id" . "Mute"))
          (("id" . "Find") ("label" . "Find..."))
          (("id" . "FindAgain") ("label" . "Find Again"))
          (("id" . "Copy"))
          (("id" . "CopyAgain") ("label" . "Copy Again"))
          (("id" . "CopySVG") ("label" . "Copy SVG"))
          (("id" . "ViewSVG") ("label" . "View SVG"))
          (("id" . "ViewSource") ("label" . "View Source"))
          (("id" . "SaveAs") ("label" . "Save As"))
          (("id" . "Help"))
          (("id" . "About") ("label" . "About Adobe CVG Viewer...")))
         ((("id" . "Open"))))
       (map (lambda(x) ((sjsonpath x) test-object-4))
            '("$.menu.items[?(and (not (eq? @ 'null)) (assoc-ref @ \"id\") (not (assoc-ref @ \"label\")))].id"
              "$.menu.items[?(and (not (eq? @ 'null)) (assoc-ref @ \"label\") (#/SVG/ (assoc-ref @ \"label\")))].id"
              "$.menu.items[?(not (eq? @ 'null))]"
              "$..[0]")))

(define test-object-5
  (parse-json-string "{\"a\": [1,2,3,4], \"b\": [5,6,7,8] }"))
 
(test* "request elements recursively to nested object."
       '((1 5) (4 8) (2 4 6 8))
       (map (lambda(x) ((sjsonpath x) test-object-5))
            '("$..[0]"
            "$..[-1:]"
            "$..[?(and (not (list? @)) (not (vector? @)) (= (modulo @ 2) 0))]")))

(define test-object-6
  (parse-json-string "{
    \"lin\": {\"color\":\"red\", \"x\":2, \"y\":3},
    \"cir\": {\"color\":\"blue\", \"x\":5, \"y\":2, \"r\":1 },
    \"arc\": {\"color\":\"green\", \"x\":2, \"y\":4, \"r\":2, \"phi0\":30, \"dphi\":120 },
    \"pnt\": {\"x\":0, \"y\":7 }
  }"))

(test* "request elements by union operator."
       '((2 5 2) ("red" "blue") ("red" "green"))
       (map (lambda(x) ((sjsonpath x) test-object-6))
            '("$.'?(json-ref @ \"color\")'.x"
              "$['lin','cir'].color"
              "$['lin','arc'].color")))

(define test-object-7
  (parse-json-string " { \"text\": [ \"hello\", \"world2.0\"] }"))

(test* "request elements by predicate of s-expression."
       '(("world2.0") ("hello"))
       (map (lambda(x) ((sjsonpath x) test-object-7))
            '("$.text[?(> (string-length @) 5)]"
              "$.text[?(char=? (string-ref @ 0) #\\h)]")))

(define test-object-8
  (parse-json-string "{
    \"a\": { \"a\":2, \"b\":3 },
    \"b\": { \"a\":4, \"b\":5 },
    \"c\": { \"a\": { \"a\":6, \"b\":7}, \"c\":8}
  }"))

(test* "request elements that be named \"a\" recursively."
       '((("a" . 2) ("b" . 3)) 2 4 (("a" . 6) ("b" . 7)) 6)
       ((sjsonpath "$..a") test-object-8))

(test-end :exit-on-failure #t)
