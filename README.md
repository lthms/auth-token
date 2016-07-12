# token

This package provide a subtype for ByteString with explicit scope that is a
purpose specified at the type level. Using *Token*, it is possible to wrap
sensible data in order to avoid mixing them by mistakes.

For instance:

```haskell
accessToken :: Token "access"
accessToken = "abcdefgh"

refreshToken :: Token "refresh"
refreshToken = "aeiabeieaieaiueaeaei"

accessToken == refreshToken -- compile error
```
