---
title: Rationale
---

_The Catalyst programming environment: Rethinking the development and
maintenance of distributed systems in industry._

In today's software centric world, many companies and individuals are building
and maintaining large, sophisticated digital services.  Yet, they are drowning
in a sea of accidental complexity rich with disparate tools.  A given stack may
include:

- your code,
- your own libraries,
- 3rd party libraries,
- code linter,
- code formatter,
- test runner,
- service virtualisation tools for testing,
- 3rd party services,
- logging tools,
- tracing tools,
- performance testing tools,
- some framework for the language,
- compiler,
- language runtime,
- language VM,
- some Linux distribution that the service will run on,
- Docker,
- some Linux distribution to host Docker,
- Ansible or Nix to configure that Linux distribution to run Docker,
- Terraform, Pulumi, AWS CloudFormation, etc. to configure the hardware and security for that Linux distribution to run on,
- Kubernetes to manage a fleet of servers,
- PostgreSQL (also configured via Terraform),
- Kafka or other message queue (also configured via Terraform),
- cloud service provider (AWS, Azure, GCP, etc.),
- Git,
- source code hosting provider (GitHub, GitLab, Bitbucket, etc.),
- a YAML-based CI/CD pipeline (GitHub Actions, Circle CI, Concourse, etc.)
- HTTP/gRPC/etc.,
- schemas,
- API documentation,
- state management,
- and more!

It is vital, but humanly impossible to understand all of this in order to build
reliable, secure and capable systems.  These problems become exponentially
worse when you aim for a multi-service system architecture.  An obscene amount
of time is wasted on configuring, connecting and debugging all this tooling.
Onboarding new team members becomes a large challenge, more specialists are
required to prevent the system collapsing.  So much of this work is redundant
and duplicated across teams and companies that all attempt to solve these
challenges their own way.

It is not uncommon to hear complaints of painfully slow test execution, compile
times, deployment speeds and development velocity being significantly held
back.

We propose to pull more of this responsibility into the programming language
environment to reduce the cognitive load on the programmer, by providing a good
story for the whole software development life cycle.  From code to production.

Smalltalk inspired.

### Definitions

Systems programming language.

Story.

### Prior art

Unfortunately most programming languages we see tend to be striving to be
"general purpose", or are often subtle variations of existing languages.



Catalyst is not designed or intended to be an ultra fast programming language.

Being server focused, pure computational performance, start up times, and
maintaining low memory consumption are not concerns for Catalyst.

Catalyst is *entirely* focused on server software; not command line tools or
desktop/mobile applications, and therefore intentionally provides no facilities
for those.  As such, pure computational performance, start up times, and
maintaining low memory consumption are not concerns.

VM on a VM?  Maximise the amount of the system written in Catalyst?  Stubs
within Catalyst's runtime to mark to the compiler that it needs an
implementation, often to be provided by a Java implementation.



Designed to be:
- Highly secure.
  - Managed VM.
  - Limit access to the platform.
- Highly dynamic.
- Safe and reliable.
- Fast enough.
- Consistent.
- Runtime flexibility.
- Highly scalable.
- Batteries included (wrapping a lot of .NET).
- Avoid accidental complexities of DevOps tooling.
- Avoid accidental complexities of distributed systems.
- Not a general purpose programming language!
- Dev tooling.



| Name  | Layer                                |
| ----  | ------------------------------------ |
| NqC   | Compiler                             |
| NqDL  | Dependency loader                    |
| NqCF  | Compiler foundations                 |
| NqRT  | Runtime                              |
| ...   | Kernel modules                       |
| NqK   | Kernel                               |
| .NET  | CLI (Common Language Infrastructure) |


Smalltalk-like + .NET + Linux kernel + RPi?
