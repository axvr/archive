FROM clojure:tools-deps

COPY src src
COPY deps.edn .

EXPOSE 8080

CMD [ "clojure", "-X:run", ":port", "8080" ]
