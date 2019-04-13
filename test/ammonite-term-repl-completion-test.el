;;; ammonite-term-repl-completion-test.el --- Tests for ammonite-term-repl-completion -*- lexical-binding: t; -*-

(require 'ammonite-term-repl-completion)

;; TODO : consider moving to buttercup instead of ert for testing.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests that do not depend on a running REPL

;; Let's say we have defined an object:
"object foobar {
  def foobarMethod  (a:String, b:String): Unit = println(a ++ b)
  def foobarMethod2 (a:String, b:String): Unit = println(a ++ b)
  def foobarMethod3 (a:String, b:String): Unit = println(a ++ b)
}"


(ert-deftest ammonite-term-repl-compl--parse-test1a ()
  (should (equal

           (ammonite-term-repl-compl--parse
            "foobar."
            (concat "@ foobar.\n"
                    "foobarMethod    foobarMethod2   foobarMethod3\n"
                    "@ foobar.")
            "/*amm-term-repl-compl-eoo*/")
           '((:signatures    . nil)
             (:completions   . ("foobarMethod" "foobarMethod2" "foobarMethod3"))
             (:parsing-notes . nil)))))

(ert-deftest ammonite-term-repl-compl--parse-test1b ()
  (should (equal

           (ammonite-term-repl-compl--parse
            "foobar."
            (concat "\n"
                    "foobarMethod    foobarMethod2   foobarMethod3\n"
                    "@ foobar.")
            "/*amm-term-repl-compl-eoo*/")
           '((:signatures    . nil)
             (:completions   . ("foobarMethod" "foobarMethod2" "foobarMethod3"))
             (:parsing-notes . nil)))))

(ert-deftest ammonite-term-repl-compl--parse-test2a ()
  (should (equal

           (ammonite-term-repl-compl--parse
            "foobar.foobarMethod"
            (concat "\n"
                    "def foobarMethod(a: String,b: String): Unit" "\n"
                    "foobarMethod2   foobarMethod3"               "\n"
                    "@ foobar.foobarMethod")
            "/*amm-term-repl-compl-eoo*/")

           '((:signatures    . ("def foobarMethod(a: String,b: String): Unit"))
             (:completions   . ("foobarMethod" "foobarMethod2" "foobarMethod3"))
             (:parsing-notes . nil)))))

(ert-deftest ammonite-term-repl-compl--parse-test2b ()
  (should (equal

           (ammonite-term-repl-compl--parse
            "foobar.foobarMethod"
            (concat "@ foobar.foobarMethod"                       "\n"
                    "def foobarMethod(a: String,b: String): Unit" "\n"
                    "foobarMethod2   foobarMethod3"               "\n"
                    "@ foobar.foobarMethod")
            "/*amm-term-repl-compl-eoo*/")

           '((:signatures    . ("def foobarMethod(a: String,b: String): Unit"))
             (:completions   . ("foobarMethod" "foobarMethod2" "foobarMethod3"))
             (:parsing-notes . nil)))))

(ert-deftest ammonite-term-repl-compl--parse-test3b ()
  ;; Ammonite does not support completing inside {} blocks, so when we
  ;; use tab there we get our input echoed without completions.
  (should (equal

           (ammonite-term-repl-compl--parse
            (concat "@ object Moo {" "\n"
                    "  foobar.")
            (concat "@ object Moo {" "\n"
                    "  foobar."      "\n"
                    "@ object Moo {" "\n"
                    "  foobar.")
            "/*amm-term-repl-compl-eoo*/")

           '((:signatures    . ())
             (:completions   . ())
             (:parsing-notes . ())))))

(ert-deftest ammonite-term-repl-compl--parse-test3b ()
  ;; Ammonite does not support completing inside {} blocks, so when we
  ;; use tab there we get our input echoed without completions.
  (should (equal

           (ammonite-term-repl-compl--parse
            (concat "@ object Moo {" "\n"
                    "  foobar.")
            (concat ""             "\n"
                    "@ object Moo {" "\n"
                    "  foobar.")
            "/*amm-term-repl-compl-eoo*/")

           '((:signatures    . ())
             (:completions   . ())
             (:parsing-notes . ())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests that depend on a running REPL

(ert-deftest test/ammonite-term-repl-compl-for-string-get-test1 ()
  (should (equal '((:signatures)
                   (:completions . ("AnsiColor" "BufferedSource" "Codec" "LowPriorityCodecImplicits" "Source" "StdIn"))
                   (:parsing-notes))
                 (ammonite-term-repl-compl-for-string-get "scala.io."))))

;;; ammonite-term-repl-completion-test.el ends here
