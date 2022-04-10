(ns org.enqueue.api.test
  (:require [org.enqueue.api.core :as server]
            [org.enqueue.api.db :as db]
            [org.enqueue.api.config :as config]
            [org.enqueue.api.helpers :refer [in?]]
            [cognitect.test-runner.api :as test-runner]))


(defn setup-db []
  (when config/test?
    (db/execute! ["DROP SCHEMA public CASCADE;"])
    (db/execute! ["CREATE SCHEMA public;"])
    (db/migrate)))


(def test-types #{:unit :integration :system})


(def server-uri
  (str "http://localhost:" (:port config/server)))


(defn run-tests
  "Wrapper around Cognitect's test-runner[1] adding an option to choose test
  types to run.

  The `:types` option accepts a collection of test types to run.  The test
  types supported are:
    - :unit         (selected by default)
    - :integration  (use database)
    - :system       (use database and local server)

  Tests are considered unit tests unless there is an ^:integration or ^:system
  meta data tag attached to them.

  Integration and system tests can *only* be run on the :test environment and
  will wipe/prepare the database specified in `config/test/config.edn`.

  Examples:

    ;; Run unit tests.
    (org.enqueue.api.test/run-tests)

    ;; Run unit and system tests.
    (org.enqueue.api.test/run-tests {:types [:unit :system]})

    ;; Run specific tests.  (Using :vars option from the Cognitect test-runner.)
    (org.enqueue.api.test/run-tests
      {:vars [org.enqueue.api.transit-tests/duration-handler
              org.enqueue.api.crypto-test/base64-decode]})

    ;; Run unit, integration and system tests from command line.
    clojure -X:test :types '[:unit :integration :system]'

  [1]: https://github.com/cognitect-labs/test-runner"
  [{:keys [types]
    :or   {types [:unit]}
    :as   options}]
  (when (some #{:integration :system} types)
    (if config/test?
      (do
        (println "Preparing DB...")
        (setup-db))
      (throw
        (ex-info "Cannot run integration or system tests on non-test environments!  Aborting test execution."
                 {:environment config/env
                  :test-types  types}))))
  (let [test-options
        (merge-with into
                    options
                    (if (in? types :unit)
                      {:excludes (remove (set types) test-types)}
                      {:includes (filter (set types) test-types)}))]
    (println "Running tests of types:" types)
    (if (in? types :system)
      (do
        (println "Starting server at" server-uri)
        (server/run {:join? false})
        (test-runner/test test-options)
        (server/stop))
      (test-runner/test test-options))))
