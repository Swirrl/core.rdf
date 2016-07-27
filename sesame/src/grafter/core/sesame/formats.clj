(ns grafter.core.rdf.sesame.formats
  (:import [org.openrdf.rio Rio]))

(defn filename->rdf-format
  "Given a filename we attempt to return an appropriate RDFFormat
  object based on the files extension."
  [fname]
  (Rio/getParserFormatForFileName fname))

;; TODO consider making this extensible via a protocol/defmulti
(defn mimetype->rdf-format
  "Given a mimetype string we attempt to return an appropriate
  RDFFormat object based on the files extension."
  [mime-type]
  (if (nil? mime-type)
    (throw (IllegalArgumentException. "Mime type required"))
    (let [base-type (str (mime/base-type mime-type))]
      (condp = base-type
        "application/rdf+xml" RDFFormat/RDFXML
        "application/xml" RDFFormat/RDFXML
        "text/plain" RDFFormat/NTRIPLES
        "application/n-triples" RDFFormat/NTRIPLES
        "text/turtle" RDFFormat/TURTLE
        "application/x-turtle" RDFFormat/TURTLE
        "text/n3" RDFFormat/N3
        "text/rdf+n3" RDFFormat/N3
        "application/trix" RDFFormat/TRIX
        "application/x-trig" RDFFormat/TRIG
        "application/x-binary-rdf" RDFFormat/BINARY
        "text/x-nquads" RDFFormat/NQUADS
        "application/ld+json" RDFFormat/JSONLD
        "application/rdf+json" RDFFormat/RDFJSON
        "application/xhtml+xml" RDFFormat/RDFA
        "application/html" RDFFormat/RDFA
        (Rio/getParserFormatForMIMEType mime-type)))))

(defn- resolve-format-preference
  "Takes an clojure.java.io destination (e.g. URL/File etc...) and a
  format-preference and tries to resolve them in a fallback chain.

  If format-preference does not resolve then we fallback to the destination's
  file extension if there is one. If no format can be resolved we raise an
  exception.

  format-preference can be a keyword e.g. :ttl, a string of an extension e.g
  \"nt\" or a mime-type.
  "
  [dest format-preference]
  (if (instance? RDFFormat format-preference)
    format-preference
    (or (try (mimetype->rdf-format format-preference) (catch Exception nx nil))
        (filename->rdf-format (str "." format-preference))
        (condp = (class dest)
          String (filename->rdf-format dest)
          File   (filename->rdf-format (str dest)))
        (throw (ex-info "Could not infer file format, please supply a :format parameter" {:error :could-not-infer-file-format :object dest})))))
