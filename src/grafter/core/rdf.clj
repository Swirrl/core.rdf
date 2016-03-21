(ns grafter.core.rdf
  "Protocols and core Clojure functions and types for portable RDF
  processing."
  (:import [java.net URI URL]
           [java.util Date]))

(defprotocol IURIable
  (->java-uri [url]
    "Convert type into a java.net.URI"))

(extend-protocol IURIable
  String
  (->java-uri [uri]
    (URI. uri))

  URI
  (->java-uri [uri]
    uri)

  URL
  (->java-uri [url]
    (.toURI url)))

(defn ->java-url
  "Convert a URI into a java.net.URL."
  [url]
  (.toURL (->java-uri url)))

(defprotocol RDFString
  (lang [this]))

(defprotocol RDFLiteral
  (raw-value [this])
  (datatype-uri [this]))

(def xsd:string "http://www.w3.org/2001/XMLSchema#string")

(def xml11-2 "http://www.w3.org/TR/xmlschema11-2/#")

(def xsd:boolean "http://www.w3.org/2001/XMLSchema#boolean")

(def xsd:byte "http://www.w3.org/2001/XMLSchema#byte")

(def xsd:short "http://www.w3.org/2001/XMLSchema#short")

(def xsd:decimal "http://www.w3.org/2001/XMLSchema#decimal")

(def xsd:double "http://www.w3.org/2001/XMLSchema#double")

(def xsd:float "http://www.w3.org/2001/XMLSchema#float")

(def xsd:integer "http://www.w3.org/2001/XMLSchema#integer")

(def xsd:int "http://www.w3.org/2001/XMLSchema#int")

(def xml11-2:string "http://www.w3.org/TR/xmlschema11-2/#string")

(def xsd:string "http://www.w3.org/2001/XMLSchema#string")

(def xsd:dateTime "http://www.w3.org/TR/xmlschema11-2/#dateTime")

(def rdf:langString "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")

(defrecord Literal [raw-value datatype-uri]
  RDFLiteral
  (raw-value [this]
    (:raw-value this))

  (datatype-uri [this]
    (->java-uri (:datatype-uri this)))

  RDFString
  (lang [this]
    nil))

(extend-protocol RDFLiteral

  java.math.BigInteger
  (raw-value [t]
    t)

  (datatype-uri [t]
    (->java-uri xsd:integer))

  java.math.BigDecimal
  (raw-value [t]
    t)

  (datatype-uri [t]
    (->java-uri xsd:decimal))

  Boolean
  (raw-value [t]
    t)

  (datatype-uri [t]
    (->java-uri xsd:boolean))

  Byte
  (raw-value [t]
    t)

  (datatype-uri [t]
    (->java-uri xsd:byte))

  Date
  (raw-value [t]
    t)

  (datatype-uri [t]
    (->java-uri xsd:dateTime))

  Double
  (raw-value [t]
    t)

  (datatype-uri [t]
    (->java-uri xsd:double))

  Float
  (raw-value [t]
    t)

  (datatype-uri [t]
    (->java-uri xsd:float))

  Integer
  (raw-value [t]
    t)

  (datatype-uri [t]
    (->java-uri xsd:integer))

  Long
  (raw-value [t]
    t)

  (datatype-uri [t]
    (->java-uri xsd:integer))

  Short
  (raw-value [t]
    t)

  (datatype-uri [t]
    (->java-uri xsd:short))

  String
  (raw-value [t]
    t)

  (datatype-uri [t]
    (->java-uri xsd:string))

  (lang [t]
    nil))

(extend-type String
  RDFString
  (lang [this]
    nil)

  RDFLiteral

  (raw-value [this]
    this)

  (datatype-uri [this]
    (->java-uri xsd:string)))

(defrecord LangString [string lang]
  RDFString
  (lang [this]
    (:lang this))

  Object
  (toString [this]
    ;; TODO consider making this output the same as .toString on a sesame
    ;; Literal.  Advantage is its more consistent with sesame etc... The
    ;; disadvantage is that this implementation makes using str more intuitive
    (:string this))

  RDFLiteral

  (raw-value [this]
    (.toString this))

  (datatype-uri [this]
    (if (:lang this)
      rdf:langString
      xsd:string)))

(defn language
  "Create an RDF langauge string out of a value string and a given
  language tag.  Language tags should be keywords representing the
  country code, e.g.

  (language \"Bonsoir\" :fr)"
  [s lang]
  {:pre [(string? s) (keyword? lang)]}
  (->LangString s lang))

(defn literal
  "You can use this to declare an RDF typed literal value along with
  its URI.  Note that there are implicit coercions already defined for
  many core clojure/java datatypes, so for common datatypes you
  shounld't need this."

  [val datatype-uri]
  (->Literal (str val) (->java-uri datatype-uri)))

(defmulti literal->clj-type
  "A multimethod to convert an RDF literal into a corresponding
  Clojure type.  This method can be extended to provide custom
  conversions."
  (fn [lit]
    (when-let [datatype (datatype-uri lit)]
      (str datatype))))

(defmethod literal->clj-type nil [literal]
  (language (raw-value literal) (lang literal)))

(defmethod literal->clj-type "http://www.w3.org/2001/XMLSchema#boolean" [literal]
  (Boolean/parseBoolean (raw-value literal)))

(defmethod literal->clj-type "http://www.w3.org/2001/XMLSchema#byte" [literal]
  (Byte/parseByte (raw-value literal)))

(defmethod literal->clj-type "http://www.w3.org/2001/XMLSchema#short" [literal]
  (Short/parseShort (raw-value literal)))

(defmethod literal->clj-type "http://www.w3.org/2001/XMLSchema#decimal" [literal]
  ;; Prefer clj's big integer over java's because of hash code issue:
  ;; http://stackoverflow.com/questions/18021902/use-cases-for-bigint-versus-biginteger-in-clojure
  (bigint (java.math.BigInteger. (raw-value literal))))

(defmethod literal->clj-type "http://www.w3.org/2001/XMLSchema#double" [literal]
  (Double/parseDouble (raw-value literal)))

(defmethod literal->clj-type "http://www.w3.org/2001/XMLSchema#float" [literal]
  (Float/parseFloat (raw-value literal)))

(defmethod literal->clj-type "http://www.w3.org/2001/XMLSchema#integer" [literal]
  (bigint (raw-value literal)))

(defmethod literal->clj-type "http://www.w3.org/2001/XMLSchema#int" [literal]
  (java.lang.Integer/parseInt (raw-value literal)))

(defmethod literal->clj-type "http://www.w3.org/2001/XMLSchema#long" [literal]
  (java.lang.Long/parseLong (raw-value literal)))

(defmethod literal->clj-type "http://www.w3.org/TR/xmlschema11-2/#string" [literal]
  (language (raw-value literal) (lang literal)))

(defmethod literal->clj-type "http://www.w3.org/2001/XMLSchema#string" [literal]
  (raw-value literal))

(defmethod literal->clj-type "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString" [literal]
  (language (raw-value literal) (lang literal)))

(defmethod literal->clj-type "http://www.w3.org/2001/XMLSchema#dateTime" [literal]
  (-> literal .calendarValue .toGregorianCalendar .getTime))

(defmethod literal->clj-type :default [literal]
  ;; If we don't have a type conversion for it, let the sesame type
  ;; through, as it's not really up to grafter to fail the processing,
  ;; as they might just want to pass data through rather than
  ;; understand it.
  literal)

(defprotocol Statement ;; Formerly grafter.rdf.protocols/IStatement
  "An RDF triple or quad"
  (subject [statement])
  (predicate [statement])
  (object [statement])
  (graph [this]
    "Returns the graph URI for this object"))

(defn- destructure-quad [quad i default]
  (case i
    0 (:s quad)
    1 (:p quad)
    2 (:o quad)
    3 (or (:g quad) default)
    :else default))

(defrecord Quad  ;; formerly grafter.rdf.protocols.Quad
    [s p o g]
  Statement
  (subject [s] (.s s))
  (predicate [s] (.p s))
  (object [s] (.o s))
  (graph [s] (.g s))

  clojure.lang.Indexed
  (nth [this ^int i]
    (destructure-quad this i nil))

  (nth [this ^int i default]
    (destructure-quad this i default)))

(defn ->Triple
  "Constructs a Quad with a nil graph (context)."
  [s p o]
  (->Quad s p o nil))

(defn map->Triple
  "Constructs a Quad from an {:s :p :o } mapwith a nil graph (context)."
  [m]
  (->Triple (:s m) (:p m) (:o m)))

(defn triple?
  "Predicate function to test if object is a valid RDF triple."
  [t]
  (when (graph t)
    true))

(defn quad?
  "Predicate function to test if the object is a valid RDF quad."
  [q]
  (when-not (graph q)
    true))

(defn triple=
  "Equality test for an RDF triple or quad, that checks whether the
  supplied RDF statements are equal in terms of RDFs semantics
  i.e. two quads will be equal regardless of their graph/context
  providing their subject, predicate and objects are equal.

  Like clojure.core/= this function can be applied to any number of
  statements."
  [& quads]
  (every? #(let [f (first quads)]
             (and (= (str (subject f)) (str (subject %)))
                  (= (str (predicate f)) (str (predicate %)))
                  (= (str (object f)) (str (object %))))
             )
          (next quads)))

(defprotocol QuadReadable ;; formerly grafter.rdf.protocols/ITripleReadable
  "Use the higher level wrapper function ->quads if you just wish to read in some RDF.

  This protocol exists for implementers to hook in additional sources
  of statements.

  Takes a source of statements or quads and converts it into a seq
  of quads.

  A hash of options is passed to each implementation, they may be
  ignored or handled depending on the circumstance."
  ;; formerly to-statements
  (to-quads [this options]))

(defn ->quads  ;; formerly grafter.rdf.protocols.statements
  "Attempts to coerce an arbitrary source of RDF statements into a
  sequence of grafter Statements.

  If the source is a quad store quads from all the named graphs will
  be returned.  Any triples in an unnamed graph will be ignored.

  Takes optional parameters which may be used depending on the
  context e.g. specifiying the format of the source triples.

  The `:format` option is supplied by the wrapping function and may be
  nil, or act as an indicator about the format of the triples to read.
  Implementers can choose whether or not to ignore or require the
  format parameter.

  The `:buffer-size` option can be used to configure the buffer size
  at which statements are parsed from an RDF stream.  Its default
  value of 32 was found to work well in practice, and also aligns with
  chunk size of Clojure's lazy sequences."
  [this & {:keys [format buffer-size] :as options}]
  (to-quads this options))
