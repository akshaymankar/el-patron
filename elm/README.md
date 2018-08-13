# El Patrón

El Patrón help you organize resources like a boss!

## Definitions
* **Resource (aka Lock)**: Any thing that can be given a name and needs to be time shared. In my team we call CI Environments as locks.
* **Pool**: A group of resources. It is usually the type of resources.

## How does it work?

The idea is simple, use git commits as a way to share resources. 

Here is an example (inspired from real life events) to help you understand:

Let's say your team shares different CI environments on 4 cloud providers: GCP, and AWS. And each of the environments are boringly named env1, env2, env3…. The git directory structure for this should look like this:
```
gcp/
   /claimed/
   /unclaimed/
             /env1
             /env2
             ︙
aws/
   /claimed/
   /unclaimed/
             /env1
             /env2
             ︙
︙
```

Now, if a user wanted to use `GCP` environment `env1`, their workflow should be:
1. move file `gcp/unclaimed/env1` to `gcp/claimed/env1`,
2. commit & push
3. use the resource
4. after they're done, make sure the resource doesn't have any residual state
5. move the file back to `unclaimed`
6. commit & push

This guarantees that a resource is only used by one user at a time. 

But you may say as a human user I do not want to deal with residual state. I am sure a pipeline can take care of it, for this one can use a concept of lifecycle. One just has to add a new pool called `gcp-lifecycle`. Then the workflow looks like this:
1. move file `gcp/unclaimed/env1` to `gcp/claimed/env1`,
1. commit & push
1. use the resource
1. move file `gcp/claimed/env1` to `gcp-lifecycle/unclaimed/env1` and let a pipeline do this:
    1. moves `gcp-lifecycle/unclaimed/env1` to `gcp-lifecycle/claimed/env1`, commits, pushes
    1. cleans up `env1`
    1. moves `gcp-life-cycle/claimed/env1` to `gcp/unclaimed/env1`, commits & pushes

## How can I get these pipelines you keep mentioning?

If you use [concourse](https://concourse-ci.org/), it is simple for you, just use these resources:
1. [pool-resource](https://github.com/concourse/pool-resource) to claim locks aka resources
2. [pool-trigger-resource](https://github.com/cfmobile/pool-trigger-resource) to trigger lifecycle pipelines
If you do not use concourse, AFAIK you'd have to write your own tooling.

## So, what does this project do?
If all of this sounds awesome for your resource sharing problem, there is a catch:
* Humans forget process and
* Pipelines fail
So, you might want to a grand view of where the resources are. And after you've seen where they are, instead of messing with git for moving files and dealing with rejected pushes, you can just use El Patrón.

## How do I get it up and running?

### Prerequisites
1. A repository on github
2. A team in a github org for authorization
3. Github oauth client id and secret
4. A private key with read write access to the repository
5. Elm compiler 
6. [create-elm-app](https://github.com/halfzebra/create-elm-app)
7. docker

### How to run

To run el-patron you need to run a frontend app and a backend app. To run the frontend you've to compile it with backend address because the generated code is HTML, CSS and purely client side JS.

#### The frontend
```
git clone git@github.com:akshaymankar/el-patron
cd el-patron
ELM_APP_BACKEND_URL=https://el-patron-api.example.com elm app build
```
This should generate the code in `build` directory. You can use your favourite webserver to host this.

#### The backend

If you can run docker, you can just run this:
```
docker run \
  -v /chamber/of/secrets:/chamber/of/secrets \
  -p 3000:3000 \
  akshay0/el-patron-api:v3 el-patron-api \
  --remote git@github.com/foo/bar \
  --private-key /chamber/of/secrest/id_rsa \
  --frontend https://el-patron.example.com \
  --backend https://el-patron-api.example.com \
  -t org/team1 \
  -t org/team2
```

