Provides utilities for reading JSON structures out of directories. Directory
entries become keys in a map, and the values are sourced from the contets of
each entry.

 * Directories are recured into
 * Files ending with `.json` are read as JSON values
 * Everything else is interpreted as a string

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
