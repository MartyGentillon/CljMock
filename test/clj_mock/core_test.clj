(ns clj-mock.core-test
  (:require [clojure.test :refer :all]
            [clj-mock.core :refer :all]
            [clj-mock.test-utils :refer :all]))

(def a-function-to-mock [] nil)
(def another-function-to-mock [] nil)



#_(deftest basic-usage
   (testing "To Mock a function, use with-mocks"
     (with-mocks [a-function-to-mock [mocking-forms]
                  another-function-to-mock [mocking-forms]])))
#_(deftest privates-are-mockable
    (testing "in order to mock a private function, you must... (magic syntax, probably with #'namespace/private-function because otherwise it won't work right...  If we can't figgure that out, we can ban it, with explanation."))
#_(deftest protocols
    (testing "since protocols are handled by the JVM, special handling may be needed, or banishment... that is always possible... Midge uses a special version of defrecord..."))

(deftest mock-creation
  (testing "Mocks are created using the create-mock function.  "
    (testing "In the simplest case, you can just pass the return values.  The values will be returned in the order passed, as long as no mocking forms match."
      (let [new-mock (make-mock [5] 10)]
        (is (= (new-mock) [5]))
        (is (= (new-mock) 10))
        (is (= (new-mock) 10)) "The last value will be returned repeatedly"))
    (testing "calls to functions will not work as expected without the :clj-mock/eval tag, as the function call will be interpteted as a mocking form"
      (let [new-mock (make-mock :clj-mock/eval (+ 5 6))]
        (is (= (new-mock) 11)) "Without the symbol, this would behave unexpectedly.  TODO what is the actual behavior?  does it crash?"))
    (testing "This can be stated more explicitly with interpreted symbol tags."
      (let [new-mock (make-mock :clj-mock/return 5 10 :clj-mock/return)]
        (is (= (new-mock) 5) "The clj-mock/return tells it that it should return the next value.")
        (is (= (new-mock) 10) "symbols can be combined with other value types")))
    (testing "The :clj-mock/throw symbol causes an exception to be thrown."
      (let [new-mock (make-mock :clj-mock/throw (RuntimeException. "error"))]
        (is (thrown-with-msg? RuntimeException "error" (new-mock)))))
    (testing "Finally, you may use the :clj-mock/call symbol to call a function."
      (let [new-mock (make-mock :clj-mock/call (fn [x] (+ x 1)))]
        (is (= (new-mock 1) 2) "The arguments to the mocked function will be passed.")))
    (testing "alternately, when you want specific returns for specific call values, mocking forms may be passed instead.  Mocking forms are vectors where the first element is a matcher, and remaining elements are return values as described above (vectors will work here, as there is no ambiguity)"
      (let [new-mock (make-mock ([1 2] 5)
                                7)]
        (is (= (new-mock 1 2) 5) "A list as a matcher will be pattern matched against the call values.")
        (is (= (new-mock 3 4) 7) "default returns may be passed into make-mock as well, they should come after all mocking forms.")))
    (testing "symbols from other namespaces can be returned."
      (let [new-mock (make-mock :test)]
        (is (= :test (new-mock)))))
    (testing "unrecognized symbols from the clj-mock namespace will result in errors."
      (is (thrown? RuntimeException (make-mock :clj-mock/unknown))))
    (testing "In the case where multiple matchers match a value, the first encoutntered will be used.")
    (testing "The library includes some validation to prevent multiple identical matchers from being passed")
    (testing "TODO need to describe the workings of matchers.")
    (testing "the with-mocks macro allows replacement of symbols with mocks in a condenced format.  It takes a list contining pairs consisting of function-name to mock and a list of mocking forms to use followed by the body during which the mocks will be in effect."
      (with-mocks [a-function-to-mock [[[1] 2]
                                       3]
                   another-function-to-mock [4]]
                  (is (= 2 (a-function-to-mock 1)))
                  (is (= 3 (a-function-to-mock 5)))
                  (is (= 4 (another-function-to-mock)))))))

(deftest inspection
  (testing "mocks keep a record of all calls made to them, this can be returned with the inspect-mock function"
    (let [my-mock (make-mock)]
      (my-mock 1)
      (my-mock 2)
      (is (= [[1] [2]] (inspect-mock my-mock)))))
  (testing "the return value state can be inspected with the inspect-mock-returns function"
    (let [my-mock (make-mock [[1 2] 3 4]
                             5 6)]
      (is (= {[1 2] [3 4]
              :default [5 6]}
             (inspect-mock-returns my-mock))))))


(deftest verification
  (testing "Verification is done simmilarly by passing a mock and verification forms.  The verification will succeed after it has been called at least once"
    (let [my-mock (make-mock "return-val")
          verification (fn [] (verify my-mock [3 4]))]
      (expect-failure (verification))
      (my-mock 3 4)
      (verification)
      (my-mock 3 4)
      (verification)))
  (testing "If you wish to specify exactly how many a value has been seen this can be done with the :clj-mock/once symbol"
    (let [my-mock (make-mock)
          verification (fn [] (verify my-mock :once [2 3]))]
      (my-mock 2 3)
      (verification)
      (my-mock 2 3)
      (expect-failure (verification))))
  (testing "You can also use the :times to indicate multiple calls"
    (let [my-mock (make-mock)
          verification (fn [] (verify my-mock :times 2 [2 3]))]
      (expect-failure (verification))
      (my-mock 2 3)
      (expect-failure (verification))
      (my-mock 2 3)
      (verification)))
  (testing "times also supports ranges"
    (let [my-mock (make-mock)
          verification (fn [] (verify my-mock :times (range 1 2) []))]
      (expect-failure (verification))
      (my-mock)
      (verification)
      (my-mock)
      (verification)
      (my-mock)
      (expect-failure (verification))))
  (testing "You can use :at-least to accept n or more calls"
    (let [my-mock (make-mock)
          verification (fn [] (verify my-mock :at-least 2 []))]
      (my-mock)
      (expect-failure (verification))
      (my-mock)
      (verification)
      (my-mock)
      (verification))) ;; and for ever more
  (testing "You can ensure that a call never occured with the :never symbol"
    (let [my-mock (make-mock)
          verification (fn [] (verify my-mock :never []))]
      (verification)
      (my-mock)
      (expect-failure (verification))))
  (testing "Unexpected calls typically do not result in failure, but this can be changed by including the :strict keyword which only succeeds if all calls are expected."
    (let [my-mock (make-mock)]
      (my-mock 1 2)
      (my-mock 3 4)
      (verify my-mock [1 2])
      (expect-failure (verify my-mock :strict [1 2]))
      (verify my-mock :strict [1 2] [3 4])))
  (testing "order typicalli doesn't matter, but this can be changed by including the :ordered.  This can be combined with :strict to triger failures on unexptected calls"
    (let [my-mock (make-mock)]
      (my-mock 1)
      (my-mock 2)
      (my-mock 3)
      (verify my-mock [3] [2] [1])
      (expect-failure (verify my-mock :ordered [3] [2] [1]))
      (verify my-mock :ordered [1] [3])
      (expect-failure (verify my-mock :ordered :strict [3] [2] [1]))
      (expect-failure (verify my-mock :ordered :strict [1] [2]))
      (verify my-mock :ordered :strict [1] [2] [3]))))

