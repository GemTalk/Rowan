Extension { #name : 'CypressClassDefinition' }

{ #category : '*Cypress-Comparison' }
CypressClassDefinition >> category: aString [

	category := aString

]

{ #category : '*Cypress-Comparison' }
CypressClassDefinition >> classCreationSelector [
  | type |
  type := self subclassType.
  type = ''
    ifTrue: [ ^ 'subclass:' ]
    ifFalse: [ 
      type = 'indexableSubclass'
        ifTrue: [ ^ 'indexableSubclass:' ]
        ifFalse: [ 
          type = 'byteSubclass'
            ifTrue: [ ^ 'byteSubclass:' ]
            ifFalse: [ self error: 'unknown subclass type: ' , type ] ] ]

]

{ #category : '*Cypress-Comparison' }
CypressClassDefinition >> classDefinitionString [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	stream
		nextPut: $(;
		nextPutAll: superclassName;
		space;
		nextPutAll: self classCreationSelector;
		space;
		nextPutAll: self name printString.
	self subclassType = 'byteSubclass'
		ifFalse: 
			[stream
				lf;
				tab;
				nextPutAll: 'instVarNames: #(' , self instanceVariablesString , ')'].
	stream
		lf;
		tab;
		nextPutAll: 'classVars: #(' , self classVariablesString , ')';
		lf;
		tab;
		nextPutAll: 'classInstVars: #(' , self classInstanceVariablesString , ')';
		lf;
		tab;
		nextPutAll: 'poolDictionaries: #(' , self poolDictionariesString , ')';
		lf;
		tab;
		nextPutAll: 'inDictionary: ''<not-defined>''';
		nextPut: $);
		lf;
		tab;
		tab;
		nextPutAll: 'category: ' , self category printString , ';';
		lf;
		tab;
		tab;
		nextPutAll: 'comment: ' , self comment printString.
	^stream contents

]
