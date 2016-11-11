# Dependencies

## Haskell Stack

Build tool for Haskell projects. For installation instructions see
[https://docs.haskellstack.org]https://docs.haskellstack.org). Once installed
run the following in the project root.

```shell
stack setup
```

## Miscellaneous

Both PyYAML and raml2html are required to build the documentation. To install
both globally,

```shell
pip install PyYAML
npm install -g raml2html
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
