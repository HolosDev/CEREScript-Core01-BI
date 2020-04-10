Maker
====

Maker is not silver bullet, but very useful.

Maker is defined as
```
data Maker s f a = Maker { mDef :: a, mF :: f, mMaker :: s -> f -> Maybe a }
```

* `s` means source, data.
* `f` means given function.
* `a` means target data type.

`runMaker :: Maker -> s -> a`

## How to use

When execute `runMaker aMaker s`,
`runMaker` takes the function `mMaker` from `aMaker`.
`mMaker` takes the arguments `s` and `f` which are given by `runMaker`, and returns the result `Maybe a`.
When the result of `f s` is `Just a`, then return it.
However, when the source does not fulfill the conditions, the result would be `Nothing`.
Then, `runMaker` returns ready-made result `mDef`.

