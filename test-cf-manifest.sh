#!/bin/bash

set -euo pipefail

manifest=$1

cp bosh-cli-* /usr/local/bin/bosh
chmod +x  /usr/local/bin/bosh

GITHUB_CLIENT_ID=$(bosh int $manifest --path=/applications/0/env/GITHUB_CLIENT_ID)
GITHUB_CLIENT_SECRET=$(bosh int $manifest --path=/applications/0/env/GITHUB_CLIENT_SECRET)
KNOWN_HOST=$(bosh int $manifest --path=/applications/0/env/KNOWN_HOST)
PRIVATE_KEY=$(bosh int $manifest --path=/applications/0/env/PRIVATE_KEY)
REMOTE=$(bosh int $manifest --path=/applications/0/env/REMOTE)
TEAMS=$(bosh int $manifest --path=/applications/0/env/TEAMS)
DISABLE_ACTION_BUTTONS=$(bosh int $manifest --path=/applications/0/env/DISABLE_ACTION_BUTTONS)
PORT=3000

export GITHUB_CLIENT_ID GITHUB_CLIENT_SECRET KNOWN_HOST PRIVATE_KEY REMOTE TEAMS DISABLE_ACTION_BUTTONS PORT

./scripts/run-with-env.sh
