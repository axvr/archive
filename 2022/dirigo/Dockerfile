FROM clojure:openjdk-17-tools-deps-alpine

COPY src src
COPY deps.edn .

EXPOSE 8080

CMD [ "clojure", "-X:run", ":port", "8080" ]
