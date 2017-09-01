# crayon-hs

A Haskell client to [crayon](https://github.com/torrvision/crayon). Implements a
servant client implementation of the [crayon
API](https://github.com/torrvision/crayon/blob/master/doc/specs.md).

Crayon is "a framework that gives you access to the visualisation power of
[TensorBoard](https://github.com/tensorflow/tensorboard) with any language."

*Currently in experimental development - don't use this yet.*

## Testing

Assuming [docker is
installed](https://docs.docker.com/engine/installation/#supported-platforms),
pull the crayon docker image:

```
docker pull alband/crayon
```

Start the crayon docker image using the  script:

```
scripts/run-docker.sh
```

Build and run crayon-hs with [stack](https://docs.haskellstack.org/en/stable/README/):

```
stack test
```
