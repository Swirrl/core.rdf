(ns grafter.core.rdf-test
  (:require [clojure.test :refer :all]
            [grafter.core.rdf :refer :all])
  (:import [java.net URI URL]))

(deftest triple=-test
  (testing "triple= quads"
    (is (triple= (->Quad "http://subject/" "http://predicate/" "http://object/" "http://context/")
                 (->Quad "http://subject/" "http://predicate/" "http://object/" nil)
                 (->Quad "http://subject/" "http://predicate/" "http://object/" "http://context/2"))))

  (testing "not triple="
    (is (not (triple= (->Quad "http://subject/1" "http://predicate/" "http://object/" "http://context/")
                      (->Quad "http://subject/2" "http://predicate/" "http://object/" "http://context/"))))

    ;; TODO make this pass
    (is  (triple= (->Quad (->java-uri "http://subject/1") "http://predicate/" "http://object/" "http://context/")
                  (->Quad "http://subject/1" "http://predicate/" "http://object/" "http://context/")))))

(deftest literal-test
  (let [lit (literal "10" "http://www.w3.org/2001/XMLSchema#byte")]
    (is (= (URI. "http://www.w3.org/2001/XMLSchema#byte") (datatype-uri lit)))
    (is (= "10" (raw-value lit)))))

(def test-data [["http://a1" "http://b1" "http://c1" "http://graph1"]
                ["http://a2" "http://b2" "http://c2" "http://graph2"]])

(def first-quad (->Quad "http://a1" "http://b1" "http://c1" "http://graph1"))

(def second-quad (->Quad "http://a2" "http://b2" "http://c2" "http://graph2"))

(deftest quads-test
  (testing "Quads"
    (testing "support positional destructuring"
      (let [quad (->Quad "http://subject/" "http://predicate/" "http://object/" "http://context/")
            [s p o c] quad]

        (is (= "http://subject/" s))
        (is (= "http://predicate/" p))
        (is (= "http://object/" o))
        (is (= "http://context/" c))))))

(deftest rdf-strings-test
  (testing "RDF Strings"
    (let [en (->LangString "Hello" :en)
          nolang (->LangString "Yo" nil)
          ;; TODO move into sesame module
          ;; sesame-fr (org.openrdf.model.impl.LiteralImpl. "Bonjour" "fr")
          ;; sesame-nolang (org.openrdf.model.impl.LiteralImpl. "Bonjour")
      ]
      (are [expected test-val]
          (is (= expected test-val))

        :en (lang en)
        "Hello" (raw-value en)
        "Hello" (str en)
        rdf:langString (datatype-uri en)


        nil (lang nolang)
        "Yo" (raw-value nolang)
        "Yo" (str nolang)
        xsd:string (datatype-uri nolang)

        ;; :fr (lang sesame-fr)
        ;; "Bonjour" (raw-value sesame-fr)

        ;; ;; NOTE we're currently inconsistent with sesame here...
        ;; "\"Bonjour\"@fr" (str sesame-fr)
        ;;rdf:langString (datatype-uri sesame-fr)
        ))))


(deftest literal-and-literal-datatype->type-test
  (are [clj-val uri klass]
      (let [ret-val (literal->clj-type (literal clj-val uri))]
        (is (= clj-val ret-val))
        (is (= klass (class clj-val))))

    true           "http://www.w3.org/2001/XMLSchema#boolean" Boolean
    (byte 10)      "http://www.w3.org/2001/XMLSchema#byte" Byte
    (short 12)     "http://www.w3.org/2001/XMLSchema#short" Short
    (bigint 9)     "http://www.w3.org/2001/XMLSchema#decimal" clojure.lang.BigInt
    (double 33.33) "http://www.w3.org/2001/XMLSchema#double" Double
    (float 23.8)   "http://www.w3.org/2001/XMLSchema#float" Float
    10             "http://www.w3.org/2001/XMLSchema#long" Long

    ;; Yes this is correct according to the XSD spec. #integer is
    ;; unbounded whereas #int is bounded
    (bigint 3)     "http://www.w3.org/2001/XMLSchema#integer" clojure.lang.BigInt
    (int 42)       "http://www.w3.org/2001/XMLSchema#int" Integer
    "hello"        "http://www.w3.org/2001/XMLSchema#string" String))

(deftest language-string-test
  (let [bonsoir (language "Bonsoir Mademoiselle" :fr)]
    (is (= bonsoir (literal->clj-type bonsoir)))))
