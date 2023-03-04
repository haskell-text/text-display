# Comparison Table

<p>

| Package | Dependencies | Purpose | GHC Support | 
|---|---|---|---|
| [`text‑display`][1] | `base`, `bytestring`, `text` | Human-readable display | Starts with 8.8 | 
| [`pretty‑show`][2] | `array`,`base`, `filepath`,`ghc‑prim`, `haskell‑lexer`,`pretty`, `text` | Pretty‑printing Show instancesfor Haskell data structures | 7.10.3 to 8.6.1 | 
| [`text‑show`][3] | `array`, `base‑compat‑batteries`, `bifunctors`,`bytestring`, `bytestring‑builder`, `containers`,`generic‑deriving`, `ghc‑prim`, `text`,`th‑abstraction`, `th‑lift`, | Systematic replacement of Showinstances of `base`, `array`, `bytestring` and `text` with a Text‑based equivalent | 7.8.4 to 9.2.2 | 
| [`pretty‑display`][4] | `base`, `text`, `pretty‑show` | Human‑readable display | 7.8.4 to 8.0.1 | 
| [`display`][5] | `base`, `bytestring`, `text` | Human‑readable display | Unclear | 

</p>


<p>

| Package | Underlying representation |
|---|---|
| [`text‑display`][1]    | Lazy Text Builder |
| [`pretty‑show`][2]     | ADT for generic representation |
| [`text‑show`][3]       | Lazy Text Builder |
| [`pretty‑display`][4]  | Defers to `pretty‑show` |
| [`display`][5]         | ByteString Builder |

</p>

[1]: https://flora.pm/packages/@hackage/text-display
[2]: https://flora.pm/packages/@hackage/pretty-show
[3]: https://flora.pm/packages/@hackage/text-show
[4]: https://flora.pm/packages/@hackage/pretty-display
[5]: https://flora.pm/packages/@hackage/display
