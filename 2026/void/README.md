# Nil and void

- _Initial concept: 2026-01-18,_
- _Implementation: 2026-05-01 – 2026-04-25,_
- _Experimentation: 2026-01-18 – Present._

I will write blog post about this, coming soon...

We've all seen code like the below.  It isn't explicit about what the output
looks like.  This is a big reason why people fear dynamically typed languages.
When you can't clearly see the shape of your data, people like to rely on tools
to give them some confidence.

```clj
(defn assoc-some [m k v]
  (if (and k v)
    (assoc m k v)
    m))

(defn build-update-user-req-body [user]
  (-> {:user-id (:id user)}
      (assoc-some :names (build-names user))
      (assoc-some :email (:email-address user))
      (assoc-some :address (build-address user))))

(defn build-names [user]
  (let [{:keys [first-name middle-name last-name]} user]
    (when (or first-name middle-name last-name)
      (-> {}
          (assoc-some :first first-name)
          (assoc-some :middle middle-name)
          (assoc-some :last last-name)))))

(defn build-address [user]
  ...)
```

But what if things were different?  What if we could write code like this
instead?

```clj
(defn build-update-user-req-body [user]
  #voidable
  {:user-id (:id user)
   :names   #supervoidable
            {:first  (:first-name user void)
             :middle (:middle-name user void)
             :last   (:last-name user void)}
   :email   (:email user void)
   :address #supervoidable
            {...}})
```

Explicitly denote what values are optional with `void`.  If a value is "void"
the value or key-value pair will be voided.

This works on all literal syntax for Clojure data structures and can even be
used by macros.

_Public domain.  No rights reserved._
