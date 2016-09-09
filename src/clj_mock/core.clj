(ns clj-mock.core)


(defmacro with-mocks [functions-to-mock & execution-forms]
           `())

(defmacro make-mock
  "returns a new mock function"
  [& forms]
  nil)

(defn make-mock-execution)

(defmacro verify [mock & verification-forms]
  `())

(defn inspect-mock [mock]
  nil)

(defn inspect-mock-returns [mock]
  nil)
