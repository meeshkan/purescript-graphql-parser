# purescript-graphql-parser

A parser to AST for languages needing to manipulate graphql.

The two goals of this packages are:

1. Precise conformance with the [June 2018 graphql spec](https://spec.graphql.org/June2018).
1. Speed.

This package is not:

1. A graphql client.
1. A graphql server.

However, if you are writing a graphql client or graphql server, this package can save you from implementing a parser.
