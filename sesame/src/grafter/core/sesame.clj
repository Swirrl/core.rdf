(ns grafter.core.sesame
  "Functions & Protocols for serializing Grafter Statements to (and from)
  any Linked Data format supported by Sesame."
  (:require [grafter.core.rdf :as rdf :refer [->Quad ->clj-type]])
  (:import ;(grafter.url GrafterURL)
   (grafter.core.rdf Quad)
   (java.io File)
   (java.net MalformedURLException URL)
   (java.util GregorianCalendar)
   (javax.xml.datatype DatatypeFactory)
   (org.openrdf.model BNode Literal Resource Statement URI
                      Value)
   (org.openrdf.model.impl BNodeImpl BooleanLiteralImpl
                                   CalendarLiteralImpl
                                   ContextStatementImpl
                                   IntegerLiteralImpl LiteralImpl
                                   NumericLiteralImpl StatementImpl
                                   URIImpl)
   (java.util Date)
   (clojure.lang BigInt Keyword)
   (grafter.core.rdf RDFString)
   ;;(org.openrdf.repository Repository RepositoryConnection)

   ;; (org.openrdf.rio RDFFormat RDFHandler RDFWriter Rio RDFParserFactory RDFParser)
   ;; (org.openrdf.rio.n3 N3ParserFactory)
   ;; (org.openrdf.rio.nquads NQuadsParserFactory)
   ;; (org.openrdf.rio.ntriples NTriplesParserFactory)
   ;; (org.openrdf.rio.rdfjson RDFJSONParserFactory)
   ;; (org.openrdf.rio.rdfxml RDFXMLParserFactory)
   ;; (org.openrdf.rio.trig TriGParserFactory)
   ;; (org.openrdf.rio.trix TriXParserFactory)
   ;; (org.openrdf.rio.turtle TurtleParserFactory)

   ))

(extend-protocol rdf/ClojureTypeConverter
  ;; Numeric Types

  BigInt
  (->clj-type [this]
    this)

  grafter.core.rdf.Literal
  (->clj-type [this]
    this)

  Byte
  (->clj-type [this]
    this)

  Double
  (->clj-type [this]
    this)

  Float
  (->clj-type [this]
    this)

  Integer
  (->clj-type [this]
    this)

  Long
  (->clj-type [this]
    this)

  Short
  (->clj-type [this]
    this)

  java.math.BigDecimal
  (->clj-type [this]
    this)

  java.math.BigInteger
  (->clj-type [this]
    this))

(defprotocol SesameTypeConverter
  ;; TODO rename ->rdf4j-type
  (->sesame-rdf-type [this] "Convert a native type into a Sesame RDF Type"))

(extend-protocol SesameTypeConverter
  ;; Numeric Types

  grafter.core.rdf.Literal
  (->sesame-rdf-type [this]
    (LiteralImpl. (rdf/raw-value this) (URIImpl. (rdf/datatype-uri this))))

  Byte
  (->sesame-rdf-type [this]
    (NumericLiteralImpl. this (URIImpl. "http://www.w3.org/2001/XMLSchema#byte")))

  Short
  (->sesame-rdf-type [this]
    (NumericLiteralImpl. this (URIImpl. "http://www.w3.org/2001/XMLSchema#short")))

  java.math.BigDecimal
  (->sesame-rdf-type [this]
    (NumericLiteralImpl. this (URIImpl. "http://www.w3.org/2001/XMLSchema#decimal")))

  Double
  (->sesame-rdf-type [this]
    (NumericLiteralImpl. this))

  Float
  (->sesame-rdf-type [this]
    (NumericLiteralImpl. this))

  Integer
  (->sesame-rdf-type [this]
    (NumericLiteralImpl. this))

  java.math.BigInteger
  (->sesame-rdf-type [this]
    (NumericLiteralImpl. this (URIImpl. "http://www.w3.org/2001/XMLSchema#integer")))

  Long
  (->sesame-rdf-type [this]
    (NumericLiteralImpl. (long this)))

  clojure.lang.BigInt
  (->sesame-rdf-type [this]
    (IntegerLiteralImpl. (BigInteger. (str this)))))

(defn IStatement->sesame-statement
  "Convert a grafter IStatement into a Sesame statement."
  [statement]
  (try
    (if (rdf/graph statement)
      ;; TODO fix type coercions here...
      (ContextStatementImpl. (->sesame-rdf-type (rdf/subject statement))
                             (->sesame-rdf-type (rdf/predicate statement))
                             (->sesame-rdf-type (rdf/object statement))
                             (->sesame-rdf-type (rdf/graph statement)))
      (StatementImpl. (->sesame-rdf-type (rdf/subject statement))
                      (->sesame-rdf-type (rdf/predicate statement))
                      (->sesame-rdf-type (rdf/object statement))))
    (catch Exception ex
      (throw (ex-info "Error outputing Quad" {:error :statement-conversion-error
                                              :quad statement
                                              :quad-meta (meta statement)} ex)))))

(defn sesame-statement->IStatement
  "Convert a sesame Statement into a grafter Quad."
  [st]
  ;; TODO fix this to work properly with object & context.
  ;; context should return either nil or a URI
  ;; object should be converted to a clojure type.
  (->Quad (rdf/->uri (.getSubject st))
          (rdf/->uri (.getPredicate st))
          (->clj-type (.getObject st))
          (when-let [graph (.getContext st)]
            (->clj-type graph))))


(extend-protocol SesameTypeConverter

  BNode
  (->sesame-rdf-type [this]
    this)

  (->clj-type [this]
    (-> this .getID keyword))

  BNodeImpl
  (->sesame-rdf-type [this]
    this)

  (->clj-type [this]
    (-> this .getID keyword))

  Boolean
  (->sesame-rdf-type [this]
    (BooleanLiteralImpl. this))

  #_(->clj-type [this]
      this)

  BooleanLiteralImpl
  (->sesame-rdf-type [this]
    this)

  Date
  (->sesame-rdf-type [this]
    (let [cal (doto (GregorianCalendar.)
                (.setTime this))]
      (-> (DatatypeFactory/newInstance)
          (.newXMLGregorianCalendar cal)
          CalendarLiteralImpl.)))

  RDFString
  (->sesame-rdf-type [this]
    (LiteralImpl. (str this) ))

  (->clj-type [this]
    this)

  Statement
  (->sesame-rdf-type [this]
    this)

  Quad
  (->sesame-rdf-type [this]
    (IStatement->sesame-statement this))

  Value
  (->sesame-rdf-type [this]
    this)

  Resource
  (->sesame-rdf-type [this]
    this)

  Literal
  (->sesame-rdf-type [this]
    this)

  #_(->clj-type [this]
      (literal-datatype->type this))

  org.openrdf.model.URI
  (->sesame-rdf-type [this]
    this)

  #_(->clj-type [this]
      (->java-uri this))

  java.net.URI
  (->clj-type [this]
    (URIImpl. (.toString this)))

  java.net.URL
  (->sesame-rdf-type [this]
    (URIImpl. (.toString this)))

  clojure.lang.Keyword
  (->sesame-rdf-type [this]
    (BNodeImpl. (name this)))

  String
  ;; Assume URI's are the norm not strings
  (->sesame-rdf-type [this]
    (LiteralImpl. this))

  (->clj-type [this]
    this)

  grafter.core.rdf.RDFString
  (->sesame-rdf-type [t]
    (LiteralImpl. (rdf/raw-value t) (name (rdf/lang t))))

  (->clj-type [t]
    t))


;; TODO consider supporting this one...

#_(extend-protocol ISesameRDFConverter
  GrafterURL

  (->clj-type [uri]
    (->url (str uri)))

  (->sesame-rdf-type [uri]
    (URIImpl. (str uri))))

;; Extend IURIable protocol to sesame URI's.

(extend-protocol rdf/URIable

  URI

  (->uri [this]
    (java.net.URI. (.stringValue this))))


(comment
  (extend-protocol ToGrafterURL
    URI
    (->grafter-url [uri]
      (-> uri
          str
          ->grafter-url))))
