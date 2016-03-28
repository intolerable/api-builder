# api-builder [![Build Status](https://travis-ci.org/intolerable/api-builder.svg?branch=master)](https://travis-ci.org/intolerable/api-builder)

Simple library for building API wrappers in Haskell – define a `Builder`, add some types with `Receivable` instances, an error type, and some routes, and you can easily use any API from Haskell code. Based on a `EitherT StateT StateT` monad transformer stack.

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

and a `Receivable` instance for `Questions` that just uses the `FromJSON` instance:

```haskell
instance Receivable Questions where
  receive = useFromJSON
```

Define a `Builder` for the API endpoint:

```haskell
stackOverflow :: Builder
stackOverflow = basicBuilder "StackOverflow API" "http://api.stackexchange.com"
```

And the `Route` to use to get the data:

```haskell
answersRoute :: Route
answersRoute = Route { urlPieces = [ "2.2", "questions" ]
                     , urlParams = [ "order" =. Just "desc"
                                   , "sort" =. Just "activity"
                                   , "site" =. Just "stackoverflow" ]
                     , httpMethod = GET }
```

And a function to actually run the API:

```haskell
getAnswers :: IO (Either (APIError ()) Questions)
getAnswers = execAPI stackOverflow () $ runRoute answersRoute

> getAnswers
Right (Questions [Question {title = "Using parse API with codeigniter", isAnswered = True, score = 2, tags = ["php","codeigniter","parse.com","codeigniter-2","php-5.6"]},Question {title = "Object...
```

If you have built code with GHCJS you can open `your-bin-dir/ghcjs-example.jsexe/index.html`
in your browser and see the result of query execution in a browser console.
