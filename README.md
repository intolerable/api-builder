# api-builder


Simple library for building JSON API wrappers in Haskell – define a `Builder`, add some types with `FromJSON` instances, an error type, and some routes, and you can easily use your API from Haskell code. Based on a `EitherT StateT StateT` monad transformer stack.

### Stack Overflow example

Define a type for a stack overflow question:

```haskell
data Question = Question { title :: Text
                         , isAnswered :: Bool
                         , score :: Int
                         , tags :: [Text] }
  deriving (Show, Eq)
```

And a wrapper since SO wraps its JSON responses:

```haskell
newtype Questions = Questions [Question]
  deriving (Show, Eq)
```

Add `FromJSON` instances for both the types:

```haskell
instance FromJSON Question where
  parseJSON (Object o) =
    Question <$> o .: "title"
             <*> o .: "is_answered"
             <*> o .: "score"
             <*> o .: "tags"
  parseJSON _ = mempty

instance FromJSON Questions where
  parseJSON (Object o) = Questions <$> o .: "items"
  parseJSON _ = mempty
```

Define a `Builder` for the API endpoint: 
```haskell
stackOverflow :: Builder
stackOverflow = Builder { _name = "StackOverflow API"
                        , _baseURL = "http://api.stackexchange.com"
                        , _customizeRequest = id
                        , _customizeRoute = id }
```
    
And the `Route` to use to get the data:

```haskell
answersRoute :: Route
answersRoute = Route { fragments = [ "2.2", "questions" ]
                     , urlParams = [ "order" =. Just "desc"
                                   , "sort" =. Just "activity"
                                   , "site" =. Just "stackoverflow" ]
                     , httpMethod = "GET" }
```
                         
And a function to actually run the API:

```haskell
getAnswers :: IO (Either (APIError ()) Questions)
getAnswers = runAPI stackOverflow () $ runRoute answersRoute
```
