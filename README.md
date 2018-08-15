# El Patrón

Application to manage pools of locks 

## Deploy to K8s using helm

1. Create a [Github OAuth App](https://developer.github.com/apps/building-oauth-apps/creating-an-oauth-app/):
    - Homepage URL must be root of whatever domain you use
    - Authorization callback URL must be `http://YOUR-DOMAIN/auth/page/github/callback`
1. Create a file (say `el-patron-secrets.yml`) with these values:
    ```yaml
    elPatron:
      authorizedTeam: <Github ORG/Team Name> # Members of this team will have access to El Patrón
      githubClientId: <OAuth client ID>
      githubClientSecret: <OAuth client secret>
      remote: git@github.com:<OWNER>/<LOCKS_REPO>
      privateKey: <private key with rights to push to locks repo>
    ```
1. Install using helm
```
helm install deploy/kubernetes/helm/el-patron --namespace el-patron -f el-patron-secrets.yml
```

## Run locally

1. Install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
1. Install [elm] (https://guide.elm-lang.org/install.html)
1. Install [create-elm-app](https://github.com/halfzebra/create-elm-app)
1. Clone the code
1. Build elm code
   ```bash
   cd el-patron/elm
   elm app build
   cd ..
   ```
1. Build haskell code
   ```bash
   stack build
   ```
1. Execute
   ```bash
   stack exec el-patron \
   --remote git@github.com:akshaymankar/test-locks \
   --private-key /ssh/id_rsa \
   --github-client-secret <GITHUB-CLIENT-SECRET> \
   --github-client-id <GITHUB-CLIENT_ID> \
   -t 'ORG/TEAM' \
   -e ./elm/build
   ```
    
