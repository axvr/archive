(ns org.enqueue.api.test
  (:require [org.enqueue.api.db :as db]
            [org.enqueue.api.config :as config]
            [org.enqueue.api.helpers :refer [in?]]
            [cognitect.test-runner.api :as test-runner]))


(defn setup-db []
  (db/execute! ["DROP SCHEMA public CASCADE;"])
  (db/execute! ["CREATE SCHEMA public;"])
  (db/migrate))


(def test-types #{:unit :integration :e2e})


(defn run-tests
  "Wrapper around Cognitect's test-runner[1] adding an option to choose test
  types to run.

  The `:types` option accepts a collection of test types to run.  The test
  types supported are:
    - :unit         (selected by default)
    - :integration  (selected by default)
    - :e2e

  Tests are considered unit tests unless there is an ^:integration or ^:e2e
  meta data tag attached to them.

  E2E tests can *only* be run on the :test environment and will wipe/prepare
  the database specified in `config/test/config.edn`.

  Examples:

    ;; Run unit and integration tests.
    (org.enqueue.api.test/run-tests)

    ;; Run unit and E2E tests.
    (org.enqueue.api.test/run-tests {:types [:unit :e2e]})

    ;; Run specific tests.  (Using :vars option from the Cognitect test-runner.)
    (org.enqueue.api.test/run-tests
      {:vars [org.enqueue.api.transit-tests/duration-handler
              org.enqueue.api.crypto-test/base64-decode]})

    ;; Run unit, integration and E2E tests from command line.
    clojure -X:test :types '[:unit :integration :e2e]'

  [1]: https://github.com/cognitect-labs/test-runner"
  [{:keys [types]
    :or   {types [:unit :integration]}
    :as   options}]
  (when (in? types :e2e)
    (if config/test?
      (do
        (println "Preparing DB...")
        (setup-db))
      (throw (ex-info "Cannot run E2E tests on non-test environments!  Aborting test execution."
                      {:environment config/env
                       :test-types  types}))))
  (println "Running tests of types:" types)
  (test-runner/test
    (merge-with into
                options
                (if (in? types :unit)
                  {:excludes (remove (set types) test-types)}
                  {:includes (filter (set types) test-types)}))))
