#!/bin/bash

set -euo pipefail

mkdir -p ~/.ssh
echo "$KNOWN_HOST" > ~/.ssh/known_hosts
echo "$PRIVATE_KEY" > /private-key
chmod 0600 /private-key
# TODO: Fix bug with infinite redirects between login and home page
# and remote this workaround
rm -f /elm/service-worker.js

IFS=',' read -ra teams_arr <<< "$TEAMS"
teams_vars=()
for t in "${teams_arr[@]}"; do
  teams_vars+=("--authorized-team=$t")
done

echo el-patron \
  -p "$PORT" \
  --github-client-id "$GITHUB_CLIENT_ID" \
  --github-client-secret "$GITHUB_CLIENT_SECRET" \
  --private-key /private-key \
  --remote git@github.com:pivotal-cf/pks-releng-ci-locks.git \
  "${teams_vars[@]}"

