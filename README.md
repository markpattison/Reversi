# Fable Reversi

This code forms part of my submission to the [F# Advent Calendar in English 2019](https://sergeytihon.com/2019/11/05/f-advent-calendar-in-english-2019/).

Read the accompanying [blog post](https://markfsharp.wordpress.com/2019/12/16/fable-reversi/).

## Requirements

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.2
* [node.js](https://nodejs.org) with [npm](https://www.npmjs.com/)
* An F# editor like Visual Studio, Visual Studio Code with [Ionide](http://ionide.io/) or [JetBrains Rider](https://www.jetbrains.com/rider/).
* [FAKE](https://fake.build/) and [Paket](https://fsprojects.github.io/Paket/) installed as .NET Core Global Tools
  * `dotnet tool install fake-cli -g`
  * `dotnet tool install paket -g`

## Building and running

To run the app locally use `fake build -t run`.  This will open the app in your browser at http://localhost:8080/.

To run the AI tests use `fake build -t test`.

To bundle for production just use `fake build`.