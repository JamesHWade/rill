# Hosting a multi-reader Rill

_Research checked against official documentation on 2026-08-31._

## Recommendation

Start the hosted product on **Render**, with one vertically sized Docker web
service, **oauth2-proxy** in front of Rill, **Auth0 Universal Login** as the
first OpenID Connect provider, the existing **Neon** database, and a separate
Render cron service for feed polling.

This is the best initial balance for Rill:

- Render can build the R image from a Dockerfile and has no platform-imposed
  maximum WebSocket duration. A connection lasts until its instance is
  replaced, so clients still need normal reconnect handling
  ([Docker](https://render.com/docs/docker),
  [WebSockets](https://render.com/docs/websocket)).
- Render has a native cron service with an at-most-one-active-run guarantee,
  which fits a single idempotent poller
  ([cron jobs](https://render.com/docs/cronjobs)).
- It provides a custom domain and managed TLS without adding a load balancer
  product ([custom domains](https://render.com/docs/custom-domains)).
- Authentication remains an application concern. Render's managed OIDC
  feature is workload identity for services calling AWS, Anthropic, or OpenAI;
  it is not an end-user account system
  ([managed OIDC](https://render.com/docs/oidc)).

Keep the web service at **one replica** for the initial product. Render assigns
each new WebSocket connection to a random instance and does not guarantee that
a reconnect returns to the previous instance. A raw Shiny session lives in one
R process, so adding generic replicas would make reconnection behavior
unpredictable. Scale vertically first; do not enable Render autoscaling for the
Shiny service until Rill has a Shiny-aware routing or resumable-session design
([WebSocket reconnection](https://render.com/docs/websocket),
[service scaling](https://render.com/docs/scaling)).

## Proposed deployment boundary

```text
browser
  -> Render managed TLS
  -> oauth2-proxy (only public listener)
       -> Rill/Shiny on loopback

Render cron service
  -> Rscript scripts/poll.R

Rill/Shiny and poller
  -> Neon pooled PostgreSQL
```

Use the same image for the web and polling services, with different commands.
Only oauth2-proxy should listen on Render's public `PORT`; the Shiny upstream
should bind to loopback. oauth2-proxy supports a generic OIDC issuer, forwards
authenticated identity headers, and proxies WebSockets by default
([generic OIDC](https://oauth2-proxy.github.io/oauth2-proxy/configuration/providers/openid_connect/),
[proxy and header options](https://oauth2-proxy.github.io/oauth2-proxy/configuration/overview/)).

Auth0 is a reasonable first identity provider because it uses OIDC, supports
the server-side Authorization Code flow and hosted login, and permits public
sign-up through database connections
([Authorization Code flow](https://auth0.com/docs/get-started/authentication-and-authorization-flow/authorization-code-flow/add-login-auth-code-flow),
[Universal Login](https://auth0.com/docs/authenticate/login/auth0-universal-login),
[database sign-up](https://auth0.com/docs/api/authentication/signup/create-a-new-user)).

Continue using Neon's pooled connection string. Neon uses PgBouncer for the
pooled endpoint and documents it specifically for applications with many or
bursty connections
([Neon connection pooling](https://neon.com/docs/connect/connection-pooling)).
Render Postgres remains a later consolidation option; it supplies managed
PostgreSQL, private in-region URLs, TLS for external connections, backups, and
paid-tier recovery features
([Render Postgres](https://render.com/docs/postgresql-creating-connecting)).

## Provider-neutral identity seam

Do not use email, an Auth0 identifier, or a platform-specific header as the
Reader primary key. Persist an internal Reader ID and attach external
identities to it:

```text
readers
  id UUID primary key

reader_identities
  reader_id UUID references readers(id)
  issuer text
  subject text
  email text nullable
  unique (issuer, subject)
```

The authentication adapter should return a small verified value object such as
`identity(issuer, subject, email, display_name)`. The application resolves
`(issuer, subject)` to `reader_id` before entering the library, feed, document,
or reader-state modules. Auth0 ID tokens carry standard `iss`, `sub`, `aud`,
and expiry claims, and Auth0 documents the required token validation checks
([token structure](https://auth0.com/docs/secure/tokens/id-tokens/id-token-structure),
[validation](https://auth0.com/docs/secure/tokens/id-tokens/validate-id-tokens)).

For the oauth2-proxy adapter, take the issuer from the configured trusted OIDC
issuer and the subject from the proxy's verified user identity. Never expose
the Shiny upstream directly, and never trust a browser-supplied forwarded-user
header. Retain two other adapters:

- `local_identity`: a fixed development Reader.
- `connect_cloud_identity`: the Connect Cloud JWT `sub`, if Rill is ever run
  there for an invited deployment.

This makes the host and IdP replaceable. Moving from Auth0 later adds another
`reader_identities` row rather than changing ownership keys throughout Rill.

For an invited beta, disable public database-connection sign-up and provision
the initial accounts. Auth0 exposes a `Disable Sign Ups` control; enabling
sign-up later changes the admission policy without changing Rill's identity
contract
([database connection configuration](https://auth0.com/docs/authenticate/database-connections/passwordless-authentication-for-db-connect)).

## Platform comparison

### Render

- **R/Docker:** R is not a native runtime, but Docker builds and prebuilt images
  are fully supported ([Docker](https://render.com/docs/docker)).
- **Connections and scale:** inbound WebSockets have no fixed duration;
  replacement or network events can disconnect them. Horizontal scaling is
  available, but new WebSockets are distributed randomly and reconnects are
  not sticky ([WebSockets](https://render.com/docs/websocket),
  [scaling](https://render.com/docs/scaling)).
- **Jobs and data:** native cron, background workers, managed PostgreSQL, and
  secret environment variables are built in
  ([cron](https://render.com/docs/cronjobs),
  [Postgres](https://render.com/docs/postgresql),
  [secrets](https://render.com/docs/configure-environment-variables)). External
  Neon also works through `DATABASE_URL`.
- **Domain/auth:** custom domains receive managed TLS. End-user authentication
  must be supplied by Rill or an authentication proxy
  ([domains](https://render.com/docs/custom-domains)).
- **Cost/operations:** low operational burden. Web compute is billed while the
  instance runs, every scaled instance is billed, and cron is billed for active
  runtime with a $1 monthly minimum
  ([billing FAQ](https://render.com/docs/faq),
  [cron billing](https://render.com/docs/cronjobs)).

**Fit:** recommended for both the invited beta and first open-sign-up version,
provided the Shiny web tier remains a single replica initially.

### Railway

- **R/Docker:** a root Dockerfile is detected and built automatically
  ([Dockerfiles](https://docs.railway.com/builds/dockerfiles)).
- **Connections and scale:** WebSockets are exempt from request timeouts and
  may remain open indefinitely, but horizontal replicas are randomly load
  balanced and Railway explicitly does not support sticky sessions
  ([network limits](https://docs.railway.com/networking/public-networking/specs-and-limits),
  [scaling](https://docs.railway.com/deployments/scaling)).
- **Jobs and data:** cron services are native and skip a scheduled run if the
  previous execution is still active. PostgreSQL is available as a Railway
  service based on its SSL-enabled image; Neon can be supplied as an external
  connection string
  ([cron](https://docs.railway.com/cron-jobs),
  [PostgreSQL](https://docs.railway.com/databases/postgresql)).
- **Domain/auth:** generated and custom domains receive automatic SSL. “Login
  with Railway” is OIDC for applications acting with a Railway user's access to
  the Railway API, not a general Reader identity store
  ([domains](https://docs.railway.com/networking/domains/working-with-domains),
  [Login with Railway](https://docs.railway.com/integrations/oauth/login-and-tokens)).
- **Cost/operations:** very low platform overhead and a $5 Hobby minimum that
  counts toward resource usage; Pro starts at $20. Resource usage is metered
  separately ([pricing](https://railway.com/pricing)).

**Fit:** a close second to Render. Its indefinite WebSockets are attractive,
but lack of sticky sessions creates the same single-replica constraint and its
scheduled-job overlap policy is slightly less useful than Render's delayed
single-run guarantee.

### Fly.io

- **R/Docker:** every Fly application is ultimately packaged as a Docker image
  and configured through `fly.toml`
  ([Fly Launch](https://fly.io/docs/reference/fly-launch/)).
- **Connections and scale:** Fly Proxy load balances connection-oriented
  services using explicit concurrency limits. Sticky routing can be built with
  `fly-replay` or a forced instance ID, and Fly supports automatic start/stop or
  a separate metrics autoscaler
  ([concurrency](https://fly.io/docs/apps/concurrency/),
  [session affinity](https://fly.io/docs/blueprints/sticky-sessions/),
  [autoscaling](https://fly.io/docs/reference/autoscaling/)).
- **Jobs and data:** scheduling is possible, but the recommended Cron Manager
  is another Fly application; simpler scheduled Machines use fuzzy hourly,
  daily, weekly, or monthly intervals. Fly offers Managed Postgres and can use
  external Neon
  ([task scheduling](https://fly.io/docs/blueprints/task-scheduling/),
  [Managed Postgres](https://fly.io/docs/mpg/)).
- **Domain/auth:** custom domains and managed Let's Encrypt certificates are
  supported. Fly's OIDC feature identifies Machines to external cloud
  resources, not application users
  ([domains](https://fly.io/docs/networking/custom-domain/),
  [workload OIDC](https://fly.io/docs/security/openid-connect/)).
- **Cost/operations:** small Machines are inexpensive and billed by provisioned
  state; stopped/suspended Machines avoid CPU and RAM charges. The tradeoff is
  more explicit machine, proxy, region, and scheduler configuration
  ([pricing](https://fly.io/docs/about/pricing/),
  [autostop/autostart](https://fly.io/docs/reference/fly-proxy-autostop-autostart/)).

**Fit:** technically strong if Rill later needs application-controlled session
placement, but unnecessarily operational for the first hosted version.

### Google Cloud Run

- **R/Docker:** any language is supported in a conforming Linux container
  ([container contract](https://docs.cloud.google.com/run/docs/container-contract)).
- **Connections and scale:** WebSockets work, but each connection is still a
  request with a hard maximum duration of 60 minutes. Session affinity is
  best-effort and reconnects can reach another instance
  ([WebSockets](https://docs.cloud.google.com/run/docs/triggering/websockets),
  [session affinity](https://docs.cloud.google.com/run/docs/configuring/session-affinity)).
- **Jobs and data:** Cloud Run Jobs plus Cloud Scheduler are the most capable
  scheduled-job system in this comparison. Cloud SQL provides managed
  PostgreSQL integration; external Neon is also reachable over TLS
  ([scheduled jobs](https://docs.cloud.google.com/run/docs/execute/jobs-on-schedule),
  [Cloud SQL](https://docs.cloud.google.com/sql/docs/postgres/connect-run)).
- **Domain/auth:** Google recommends an external Application Load Balancer for
  custom domains. Identity Platform supports email/password and social-provider
  end-user authentication, but the public application must implement and
  verify its tokens
  ([custom domains](https://docs.cloud.google.com/run/docs/mapping-custom-domains),
  [end-user authentication](https://docs.cloud.google.com/run/docs/authenticating/end-users)).
- **Cost/operations:** compute is metered in 100 ms increments and includes a
  free tier, but a container with an open WebSocket is active and billable.
  Identity Platform, Scheduler, Artifact Registry, and a load balancer make the
  complete product more operationally involved
  ([pricing](https://cloud.google.com/run/pricing),
  [WebSocket billing](https://docs.cloud.google.com/run/docs/triggering/websockets)).

**Fit:** poor for the current stateful Shiny UI because forced hourly
disconnects are a product behavior, despite excellent jobs, autoscaling, and
first-party identity options.

### Posit Connect Cloud

- **R/Shiny:** the strongest native R experience. Connect Cloud deploys Shiny
  directly from GitHub using `manifest.json` and manages worker processes,
  connection limits, and connection timeouts
  ([Shiny deployment](https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html),
  [runtime settings](https://docs.posit.co/connect-cloud/user/manage/content_settings.html)).
- **Jobs and data:** paid plans can schedule _content republishing_, not an
  arbitrary durable feed-polling service. Rill would still need GitHub Actions
  or another scheduler for `scripts/poll.R`. Application secrets can hold an
  external Neon URL
  ([content settings](https://docs.posit.co/connect-cloud/user/manage/content_settings.html)).
- **Domain/auth:** Enhanced and higher plans provide custom domains and managed
  TLS. Private organization content receives a trusted JWT header with a stable
  Connect Cloud `sub`
  ([custom domains](https://docs.posit.co/connect-cloud/user/share/custom-domains.html),
  [visitor identity](https://docs.posit.co/connect-cloud/user/manage/visitor_information.html)).
- **Cost/operations:** minimal Shiny operations, but named authenticated access
  requires Advanced or Enterprise. Every authenticated admin, publisher,
  viewer, or external guest consumes a seat, creating a materially higher
  fixed and per-reader cost posture than application-owned authentication
  ([plans](https://docs.posit.co/connect-cloud/user/account/plans.html)).

**Fit:** excellent for a small invited beta when seat cost is acceptable. It is
not a consumer account platform: private-link viewers are anonymous to the app,
while named viewers must have Connect Cloud accounts and paid seats
([sharing model](https://docs.posit.co/connect-cloud/user/share/index.html)).

## Staged decision

1. **Invited beta:** deploy the recommended Render stack with one web replica,
   Auth0 sign-up disabled, manually provisioned beta accounts, Neon, and the
   Render poller. Connect Cloud is a valid shortcut only if its seat cost is
   preferable to building the authentication boundary now.
2. **Open sign-up:** enable Auth0 self-service sign-up, add account lifecycle
   and abuse controls, and keep ownership keyed by Rill's internal `reader_id`.
   No host or database migration is required.
3. **Scale:** measure concurrent WebSockets and R-process pressure. Vertically
   scale first. Before adding Render replicas, introduce a session-routing or
   resumable-session boundary and exercise reconnects during deploys.
4. **Revisit the host only when evidence demands it:** Fly.io if explicit
   instance routing becomes valuable; Cloud Run if Rill's UI becomes stateless
   enough to tolerate hourly reconnects; Connect Cloud for seat-based private
   deployments rather than open consumer sign-up.

## Deployment proof required

Before inviting Readers, verify one end-to-end scenario in production:

- two authenticated Readers share one Feed fetch but have separate
  Subscriptions, folders, and reading state;
- the application derives identity from a verified `(issuer, subject)` and
  never from a client-supplied actor ID or email;
- one Reader cannot retrieve another Reader's captures or selected Document
  heads;
- the poller overlaps neither itself nor web-process lifecycle;
- an intentional web deploy disconnects and reconnects a Reader without
  crossing identity or provenance boundaries.
