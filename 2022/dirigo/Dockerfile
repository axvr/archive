FROM clojure:openjdk-18-tools-deps-alpine

COPY src src
COPY resources resources
COPY deps.edn .

EXPOSE 3000

CMD [ "clojure", "-X:run", ":port", "3000" ]
