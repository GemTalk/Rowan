Extension { #name : 'STONWriteReadTests' }

{ #category : '*ston-gemstone-tests' }
STONWriteReadTests >> testClasses [
	| classes |
	classes := STON listClass withAll: { Integer. Object }.
	self serializeAndMaterialize: classes.
	classes := STON listClass withAll: { Integer class. Object class }.
	self serializeAndMaterialize: classes.
	classes := STON listClass withAll: { Class. Class class. Class class class. Class class class class }.
	self serializeAndMaterialize: classes
]

{ #category : '*ston-gemstone-tests' }
STONWriteReadTests >> testGemStoneCollections [
  | collections |
  collections := STON listClass
    withAll:
      {(KeyValueDictionary new
        at: 1 put: 1;
        at: 2 put: 2;
        yourself)}.
  self serializeAndMaterialize: collections
]
