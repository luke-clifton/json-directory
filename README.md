# JSON Directory

[![](https://img.shields.io/hackage/v/shh.svg?colorB=%23999&label=shh)](http://hackage.haskell.org/package/json-directory)
[![](https://builds.sr.ht/~lukec/json-directory/commits/nix.yml.svg)](https://builds.sr.ht/~lukec/json-directoryh/commits/nix.yml)

Provides utilities for reading JSON structures out of directories. Directory
entries become keys in a map, and the values are sourced from the contets of
each entry.

By default

 * Directories are recured into
 * Files ending with `.json` are read as JSON values
 * Everything else is interpreted as a string

However, these can be modified with rules, that allow for interpreting
any sort of file as JSON.

The [example](./example) directory in this repository would result in
the following JSON value.

```json
{
  "empty": {},
  "a": "This is files will have its contents written into a JSON string.\n",
  "d": {
    "example": "Sometimes it's convenient to embed some raw JSON"
  },
  "b": 42,
  "c": {
    "nested": "You can nest directories to create a tree structure.\n"
  }
}
```

## `jsondir`

This package also includes an executable for turning directories into JSON
blobs.

```
jsondir [--help] [--rule <SUFFIX> <FILTER> ...]
        [--[no-]default{s,-json,-text}] <ROOT> ...

  Turn a directory structure into a JSON value

 --rule <SUFFIX> <FILTER>     Filter the contents of files with the given
                              SUFFIX with FILTER. The default rules use .json
                              files as is, and treat everything else as strings.
                              Can be specified multiple times. Rule are tried in
                              the order specified.
 --[no-]defaults              Enable or disable the default rules. Default on.
 --[no-]default-json          Enable or disable the .json file rule
 --[no-]default-text          Enable or disable the raw file to JSON string rule.
 <ROOT>                       Directory root to turn into a JSON value

 EXAMPLE
  jsondir --rule '.yml' yaml2json ./my-dir
```

