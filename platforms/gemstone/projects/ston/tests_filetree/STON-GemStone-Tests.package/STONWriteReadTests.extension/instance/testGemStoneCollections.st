*ston-gemstone-tests
testGemStoneCollections
  | collections |
  collections := STON listClass
    withAll:
      {(KeyValueDictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself)}.
  self serializeAndMaterialize: collections