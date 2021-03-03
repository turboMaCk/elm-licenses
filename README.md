# Elm Licenses

**alpha**

Simple CLI tool for reporting licenses of Elm dependencies.

## Runtime Dependecies

Just Node.js (and curl if you will ran it as I do).

## Usage

This tool is primary meant to be used by CI pipelines.

```
$ curl -L https://github.com/turboMaCk/elm-licenses/releases/download/0.1.0/index.js | node
```

*My `elm.json` is in different directory.*

```
$ curl -L https://github.com/turboMaCk/elm-licenses/releases/download/0.1.0/index.js | node - path/to/elm-app
```

*I'm using custom `$ELM_HOME`*

Just make sure you ran the script in environment with `$ELM_HOME`
set to the correct path and script will pick it up.

*How long does it take to download and run the script?*

On my machine:

```
time curl -L https://github.com/turboMaCk/elm-licenses/releases/download/0.1.0/index.js | node - frontend
...
node - frontend  0.06s user 0.01s system 8% cpu 0.721 total
```
