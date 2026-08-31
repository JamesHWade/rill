---
status: proposed
---

# Host authenticated Rill on Render behind OIDC

Run the first Hosted Rill beta as one vertically scaled Render Docker web
process behind oauth2-proxy, using Auth0 Universal Login for the first OpenID
Connect adapter, Neon for PostgreSQL, and a separate Render cron process for
feed polling. Resolve each verified `(issuer, subject)` to an internal Reader
identifier so neither the runtime nor the identity provider owns Rill's domain
identity. This keeps Shiny WebSocket sessions on one process while the product
is small and leaves both host and identity provider replaceable.

## Considered options

Railway has similar single-replica constraints; Fly.io offers stronger routing
control with more operational work; Cloud Run imposes hourly WebSocket
reconnections; and Connect Cloud makes invited Shiny deployment easiest but
prices authenticated Readers as seats. The current evidence is recorded in
[the hosting research](../research/hosted-rill-platforms.md).

## Consequences

Do not add generic web replicas until Rill has Shiny-aware session routing or
resumable sessions. Only the authentication proxy may accept public traffic,
and Rill must never trust identity headers from a directly reachable client.
