# Deploy Rill on Render

Rill ships one production image with two roles:

- `web` starts Shiny on loopback and oauth2-proxy on the public port.
- `poll` runs one feed refresh and exits successfully only when every Feed
  succeeds.

Use one image digest for both Render services. A GitHub Actions run on `main`
publishes the image to GitHub Container Registry and records its digest in the
job summary. Deploy the digest, such as
`ghcr.io/jameshwade/rill@sha256:...`, rather than a mutable tag.

This deployment shape implements the runtime boundary in
[ADR 0001](../adr/0001-hosted-rill-runtime-and-identity.md). Shared source
acquisition remains distinct from each Reader's subscriptions, entry state,
captures, and selected reading copies.

## Run the local composition

Create an Auth0 Regular Web Application. Add
`http://127.0.0.1:10000/oauth2/callback` to **Allowed Callback URLs** and
`http://127.0.0.1:10000/` to **Allowed Logout URLs**. Disable sign-ups on the
database connection for an invited deployment. Enable the Google and GitHub
social connections, but leave Auth0 account linking disabled: Rill owns the
mapping from each external identity to its Reader.

Sign in once with each identity and copy its exact Auth0 `user_id`/OIDC `sub`
from the Auth0 user record. Set the comma-separated values in
`RILL_ALLOWED_OIDC_SUBJECTS`. Rill compares them case-sensitively and never uses
email as an identity key.

Copy `.env.hosted.example` to an ignored `.env`, replace every placeholder,
then start Rill:

```sh
docker compose up --build web
```

Open <http://127.0.0.1:10000>. Only the oauth2-proxy listener is published.
The Shiny listener remains at `127.0.0.1:3838` inside the container.

Run the polling role from the same local image:

```sh
docker compose --profile poll run --rm poll
```

Use `docker compose down` to stop the services. Add `--volumes` only when you
intend to permanently remove the local PostgreSQL data.

## Publish and configure the immutable image

After the container workflow succeeds on `main`, make the repository package
public or give Render read credentials for GitHub Container Registry. Copy the
published `sha256` digest from the workflow summary.

Create one Render image-backed web service with:

- **Image URL:** the exact GitHub Container Registry digest.
- **Docker command:** `web`.
- **Health check path:** `/ready`.
- **Instance count:** one; do not enable autoscaling for stateful Shiny
  sessions.

Create one Render image-backed cron job with the same image digest and the
command `poll`. Choose the polling schedule in Render. Render prevents runs of
one cron job from overlapping, and Rill also uses a PostgreSQL advisory lock so
an accidental second scheduler exits successfully without fetching any Feed.

Set these secret environment variables on the web service:

```text
DATABASE_URL=<Neon pooled URL ending in sslmode=require>
RILL_ACTOR_ID=<stable bootstrap Reader identifier>
RILL_ALLOWED_OIDC_SUBJECTS=<comma-separated exact Auth0 sub values>
RILL_ENV=production
OAUTH2_PROXY_CLIENT_ID=<Auth0 client ID>
OAUTH2_PROXY_CLIENT_SECRET=<Auth0 client secret>
OAUTH2_PROXY_COOKIE_SECRET=<32-byte base64url secret>
OAUTH2_PROXY_OIDC_ISSUER_URL=https://<tenant>.auth0.com/
OAUTH2_PROXY_REDIRECT_URL=https://<rill-domain>/oauth2/callback
OAUTH2_PROXY_COOKIE_SECURE=true
```

Add `https://<rill-domain>/` to the Auth0 application's **Allowed Logout
URLs**. It must match the origin derived from `OAUTH2_PROXY_REDIRECT_URL`.

Set `DATABASE_URL`, `RILL_ACTOR_ID`, `RILL_ENV=production`,
`RILL_POLL_INTERVAL_MINUTES`, and `RILL_POLL_FAILURE_THRESHOLD` on the cron
job. The interval determines when an active Feed is due; the threshold is the
number of failed due Feeds that makes the process exit non-zero. A smaller
number detects broad failures earlier, while an isolated stale Feed remains a
recorded partial success below the threshold. Add Rill's optional provider,
Defuddle, capture, and telemetry variables only to the role that needs them.
Never put credentials in the Dockerfile, Compose file, image URL, or Render
command.

Render supplies `PORT` to the web service. The container binds oauth2-proxy to
`0.0.0.0:$PORT`, while Shiny always binds to
`127.0.0.1:${RILL_SHINY_PORT:-3838}`. The image runs as UID/GID 10001 and has a
container health check that verifies both listeners. `/ping` checks the proxy;
`/ready` is the public Render readiness path.

The web role always enables Rill's OIDC proxy gate, configures oauth2-proxy to
forward the verified `sub` claim rather than email, and strips incoming
identity headers before replacing them. Provider access and ID tokens are not
forwarded into Rill or retained in its minimal proxy session. **Sign out**
clears the oauth2-proxy session, sends the browser through Auth0's logout
endpoint using the public client ID, and returns to the registered Rill URL.

Rill persists the exact Auth0 issuer and `sub` pair separately from its opaque
Reader identifier. The configured subject allowlist bootstraps the initial
private Reader. A verified identity without a binding receives no Library
access and creates one deduplicated pending admission; mutable email and profile
claims never become ownership keys. An operator can approve that admission for
a new or existing Reader without exposing another Reader's Library. Rill does
not yet include admission-management UI, so keep this an invitation-only
operator workflow.

## Verify before inviting a Reader

Confirm all of these against the deployed digest:

1. An unauthenticated visit redirects to Auth0 and returns through the exact
   HTTPS callback URL.
2. `/ready` is healthy and the Shiny port is not externally reachable.
3. A manual `poll` cron run updates the same Neon database used by the web
   service and exits zero.
4. Restarting the web service preserves Library state in Neon.
5. Configured Google and GitHub identities open the bootstrap Reader's Library.
6. An unconfigured identity receives the generic Rill access-denied page and
   creates one pending admission.
7. Supplying a forged `X-Forwarded-User` header to the public URL cannot bypass
   oauth2-proxy, and the loopback Shiny listener is externally unreachable.
8. **Sign out** requires a fresh Auth0 login before the Library opens again.
9. Restarting or deploying the web service reconnects to the same Library with
   no change to the configured Reader identity.
10. An identity admitted to a second Reader starts with an empty Library and
    cannot open the bootstrap Reader's captures or selected reading copies.

The configured allowlist intentionally maps its subjects to one bootstrap
Reader. Additional Readers enter only through explicit, audited admission.
