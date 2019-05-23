tests
testAsRwUrl
  self sampleUrlStrings
    do: [ :urlString | 
      | url |
      RwUrl fromString: urlString.
      url := urlString asRwUrl.
      self assert: url printString = urlString ]