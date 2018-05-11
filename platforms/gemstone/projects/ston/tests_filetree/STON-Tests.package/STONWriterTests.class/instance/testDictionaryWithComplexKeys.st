tests
testDictionaryWithComplexKeys
  "order dependent test"

  | collection newCollection |
  collection := STON mapClass new
    at: true put: 1;
    at: #(#'foo') put: 2;
    yourself.
  newCollection := self materialize: (self serialize: collection).
  self assert: newCollection = collection