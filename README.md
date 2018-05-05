
# Easy Streaming Library

Heavily inspired by [Streamly](https://github.com/composewell/streamly)

## Example

```haskell
streamA :: IsStream t => t m Int
streamA = fromStream $ 1 >: 2 >: 3 >: 4 >: snil

streamB :: (IsStream t, Monad m) => t m (Int, Int)
streamB = do
  x <- streamA
  y <- streamA
  pure (x, y)

main :: IO ()
main = do
  x <- toList (streamB :: SerialT IO (Int, Int))
  y <- toList (streamB :: CoserialT IO (Int, Int))
  mapM_ print (x ++ y)
```
