FROM scratch
COPY nqvm /nq/vm
EXPOSE 7881/tcp
EXPOSE 7881/udp
ENTRYPOINT ["/nq/vm"]
SHELL ["/nq/vm", "--local-repl"]
HEALTHCHECK CMD (nq/health)

---

FROM org.enqueue.vm:1.0
COPY src /nq
COPY conf.nq /nq

# 6881 <- DQ
# 7881 <- NQ

# dequeue.org
# enqueue.org

# nq
# dq

# NQ
# DQ
