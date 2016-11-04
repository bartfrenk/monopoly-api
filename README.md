# Dependencies

Requires the following items to be installed:

- [Haskell Stack](https://docs.haskellstack.org)
- PyYAML (`pip install PyYAML`)
- raml2hmtl (`npm install raml2hmtl`)

The last two are not required to run Monopoly Server, only to build the
documentation (which is incomplete).

# Running

Ensure Haskell Stack and Docker are installed. Running,

```shell
stack setup
make build-docker-pg
make run-server
```

should do the trick. For information on how to load the resources in
`res/samples`, simply try running `make`. This gives a list of all targets, each
with a short description.
