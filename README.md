# purescript-ordered-set

An `Array` that automatically maintains uniqueness, preserving insertion order.

[![Pursuit](https://pursuit.purescript.org/packages/purescript-ordered-set/badge)](https://pursuit.purescript.org/packages/purescript-ordered-set)

## Installation

```
spago install ordered-set
```

## Why OSet?

`OSet` is a thin wrapper around `Array` that enforces element uniqueness as a structural invariant. Operations like `cons`, `snoc`, `union`, and `append` silently ignore duplicates — you never need to remember to call `nub`.

## When to use OSet instead of Array or Set

### vs Array — deduplication without manual nubbing

You're collecting user-selected tags. With `Array` you need `nubEq` after every append or risk silent duplicates. `OSet` handles this for you:

```purescript
import Data.Set.Ordered as OSet

tags = OSet.singleton "ps"
  # OSet.snoc "nix"
  # OSet.snoc "ps"    -- ignored, already present

-- OSet ["ps", "nix"]
```

### vs Data.Set — when insertion order matters

You're building a breadcrumb trail or processing queue. `Data.Set` from `ordered-collections` sorts by `Ord`, destroying the order items were added. `OSet` preserves insertion order:

```purescript
trail = OSet.empty
  # OSet.snoc "home"
  # OSet.snoc "blog"
  # OSet.snoc "post-42"

OSet.head trail -- Just "home" (first added, not alphabetically first)
```

### vs both — Array-like indexing with Set-like guarantees

You need `head`, `tail`, `take`, `drop`, `index`, `zip` — operations that only make sense on ordered sequences — but also need uniqueness. `OSet` gives you both:

```purescript
queue = OSet.fromFoldable ["a", "b", "c"]
OSet.index queue 1  -- Just "b"
OSet.take 2 queue   -- OSet ["a", "b"]
```

## Design notes

- **No Functor** — `map` can collapse duplicates, so it is not structure-preserving and `OSet` intentionally does not implement `Functor`.
- **`fromFoldable` does not deduplicate** — the caller is expected to provide unique input. Use `Array.nubEq` first if your source may contain duplicates.
- **`Semigroup`/`append` deduplicates** — `xs <> ys` concatenates then removes duplicates with `nubEq`, preserving left-side (first-occurrence) order.

## Documentation

Full API docs are on [Pursuit](https://pursuit.purescript.org/packages/purescript-ordered-set).

## License

AGPL-3.0-or-later
