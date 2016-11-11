# Install

Requires the following items to be installed:

- [Haskell Stack](https://docs.haskellstack.org)
- PyYAML (`pip install PyYAML`)
- raml2hmtl (`npm install raml2hmtl`)

The last two are not required to run Monopoly Server, only to build the
documentation (which is incomplete).

```shell
stack setup
```

# Run

To run the latest version:

```shell
make build-images
docker-compose up
```

To load the resources:

```shell
make load-locations
make load-questions
make load-teams
```

For the latter two it is best to `>` to a file, since there is no endpoint to
retrieve the token for the teams and questions from the API.

# Clean

To start with a clean database run:

```shell
docker-compose down -v
```
