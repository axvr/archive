FROM clojure:openjdk-18-tools-deps-alpine

COPY src src
COPY resources resources
COPY deps.edn .

EXPOSE 8080

CMD [ "clojure", "-X:run", ":port", "8080" ]
