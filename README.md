# Scala3doc

Scala3doc (name subject to change) is the documentation tool for
[Dotty](https://github.com/lampepfl/dotty), which is scheduled to become
Scala 3. It's based on [Dokka](https://github.com/Kotlin/dokka), the
documentation tool for Kotlin. It uses Tasty-Reflect to access definitions,
which is an officially supported way to access Dotty's perspective of a
codebase.

We're aiming to support all the features Scaladoc did, plus new and exciting ones such as:

- Markdown syntax!
- displaying project and API documentation together on one site!
- and more!

## Contributing

We're happy that you'd like to help us!

We have two issue labels you should take a look at: `good first issue` and
`self-contained`. First is easy pickings: you'll be able to contribute without
needing to dive too deep into the project. Second is reverse: it's an issue
that's you may find interesting, complex and self-contained enough that you can
continue chipping away at it without needing to worry too much about merge
conflicts.

## Running the project

Use the following commands to generate documentation for this project and for Dotty, respectively:

```
sbt generateSelfDocumentation
sbt generateDottyLibDocumentation
```

To actually view the documentation, the easiest way is to run the following in project root:

```
cd output
python3 -m http.server 8080
```

And afterwards point your browser to `http://localhost:8080/self` or
`http://localhost:8080/stdLib` for this project and for Dotty documentation
respectively.

It's not strictly necessary to go through an HTTP server, but because of CORS
the documentation won't work completely if you don't.

## Developing

At least two of our contributors use [Metals](https://scalameta.org/metals/) to
work on the project.

For every PR, we build documentation for Scala3doc and Dotty. For example, for
PR 123 you can find them at:

+ https://scala3doc.s3.eu-central-1.amazonaws.com/pr-123/self/main/index.html
+ https://scala3doc.s3.eu-central-1.amazonaws.com/pr-123/stdLib/main/index.html

Note that these correspond to the contents of `output` directory - that's
precisely what they are.

You can also find the result of building the same sites for latest `master` at:

+ https://scala3doc.s3.eu-central-1.amazonaws.com/pr-master/self/main/index.html
+ https://scala3doc.s3.eu-central-1.amazonaws.com/pr-master/stdLib/main/index.html

### Testing

Most tests rely on comparing signatures (of classes, methods, objects etc.) extracted from the generated documentation
to signatures found in source files.
By default it's expected that all signatures from the source files will be present in the documentation
but not vice versa (because the documentation can contain also inherited signatures).
To validate that a signature present in the source does not exist in the documentation
(because they should be hidden from users) add `//unexpected` comment after the signature in the same line.
This will cause an error if a signature with the same name appears in the documentation (even if some elements of the signature are slightly different - to avoid accidentally passing tests).
If the signature in the documentation is expected to slightly differ from how it's defined in the source code
you can add a `//expected: ` comment (also in the same line and followed by a space) followed by the expected signature.
Alternatively you can use `/*<-*/` and `/*->*/` as opening and closing parentheses for parts of a signature present in the source but undesired in the documentation (at least at the current stage of development), e.g.

```
def foo/*<-*/()/*->*/: Int
```

will make the expected signature be

```
def foo: Int
```

instead of

```
def foo(): Int
```


Because of the way how signatures in source are parsed, they're expected to span until the end of a line (including comments except those special ones mentioned above, which change the behaviour of tests) so if a definition contains an implementation, it should be placed in a separate line, e.g.

```
def foo: Int
   = 1

class Bar
{
   //...
}
```

Otherwise the implementation would be treated as a part of the signature.

`MultipleFileTest` and `SingleFileTest` test classes allow you to specify which kinds of signatures from a source file (depending on the keyword used to declare them like `def`, `class`, `object` etc.) are checked and which are ignored in a test.

## Roadmap

1. Publish an initial version of the tool together with an SBT plugin
1. Replace Dottydoc as the dedicated tool for documenting Dotty code

   This includes:
   + supporting Dotty's doc pages
   + releasing together with Dotty as the dedicated documentation tool

1. Support all kinds of Dotty definition and generate documentation for the
   standard library
1. Reach feature parity with Scaladoc

## FAQ

### Why depend on Dokka?

We have two primary reasons for depending on Dokka. One of them is division of
labour - Dokka already has a team of maintainers, and it supports an excellent
API which already allowed us to quite easily generate documentation with it. By
depending on Dokka, we will be able to share a large portion of the maintenance
burden. The second reason is very pragmatic - on our own, it'd be difficult for
us to reach even feature parity with Scaladoc, simply because of workforce
constraints. Meanwhile, Dokka maintainers from VirtusLab reached out to us with
an offer of help, which we were happy to take.

### Why use TASTy?

A documentation tool needs to access compiler information about the project - it
needs to list all definitions, resolve them by name, and query their members.
Tasty Reflect is the dedicated way in Scala 3 of accessing this information.
