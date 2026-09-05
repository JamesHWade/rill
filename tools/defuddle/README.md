# Bundled Defuddle CLI

This directory builds the pinned Defuddle CLI and its dependencies into
`inst/defuddle/defuddle.cjs`. The bundle runs with Node.js 18+ or Deno 2+,
including the Deno runtime supplied by Posit Connect Cloud. No npm downloads
occur while articles are being read or prepared.

Rebuild from this directory:

```sh
npm ci --ignore-scripts --no-audit --no-fund
npm run build
```

Commit the bundle, version, licenses, and updated Connect manifest checksums
alongside any dependency change. The build collects licenses from every
package included by esbuild. Canvas is optional in linkedom and is not needed
for article extraction.

Rill invokes the normal Defuddle CLI with its own user agent. Public pages
that reject this request remain unavailable; the extractor does not receive
reader credentials or an authenticated browser session.
