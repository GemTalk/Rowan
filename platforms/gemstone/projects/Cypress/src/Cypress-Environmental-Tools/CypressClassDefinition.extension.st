Extension { #name : 'CypressClassDefinition' }

{ #category : '*cypress-environmental-tools' }
CypressClassDefinition >> createOrReviseClass: aSymbolDictionaryName environmentLoader: environmentLoader [
  ^ self subclassType = ''
    ifTrue: [ 
      self
        createOrReviseRegularClass: aSymbolDictionaryName
        environmentLoader: environmentLoader ]
    ifFalse: [ 
      self subclassType = 'byteSubclass'
        ifTrue: [ 
          self
            createOrReviseByteClass: aSymbolDictionaryName
            environmentLoader: environmentLoader ]
        ifFalse: [ 
          self subclassType = 'indexableSubclass'
            ifTrue: [ 
              self
                createOrReviseIndexableClass: aSymbolDictionaryName
                environmentLoader: environmentLoader ]
            ifFalse: [ self error: 'unknown subclass type: ' , self subclassType printString ] ] ]

]

{ #category : '*cypress-environmental-tools' }
CypressClassDefinition >> createOrReviseRegularClass: aSymbolDictionaryName environmentLoader: environmentLoader [
  "To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

  | superClass lookupSymbolList |
  lookupSymbolList := environmentLoader lookupSymbolList.
  superClass := (lookupSymbolList resolveSymbol: self superclassName) value.
  ^ (superClass
    subclass: self name
    instVarNames: (self instVarNames collect: [ :each | each asSymbol ])
    classVars: (self classVarNames collect: [ :each | each asSymbol ])
    classInstVars: (self classInstVarNames collect: [ :each | each asSymbol ])
    poolDictionaries: #()
    inDictionary:
      ((self symbolDictionaryForClassNamed: self name symbolList: lookupSymbolList)
        ifNil: [ (lookupSymbolList resolveSymbol: aSymbolDictionaryName) value ])
    options: #())
    category: category;
    comment: self comment

]

{ #category : '*cypress-environmental-tools' }
CypressClassDefinition >> loadClassDefinition: aSymbolDictionaryName environmentLoader: environmentLoader [
  "Create a new version of the defined class. If the class already exists,
	 copy the behaviors and state from the old version."

  | newClass oldClass lookupSymbolList |
  lookupSymbolList := environmentLoader lookupSymbolList.
  self defaultSymbolDictionaryName: aSymbolDictionaryName.
  (lookupSymbolList resolveSymbol: self name)
    ifNotNil: [ :assoc | oldClass := assoc value ].
  newClass := self
    createOrReviseClass: aSymbolDictionaryName
    environmentLoader: environmentLoader.
  (oldClass isNil or: [ newClass == oldClass ])
    ifTrue: [ ^ self ].
  self classNeedingMigration: newClass.
  self
    recompileWithSubclassesFrom: oldClass
    to: newClass
    symbolList: lookupSymbolList 

]

{ #category : '*cypress-environmental-tools' }
CypressClassDefinition >> symbolDictionaryForClassNamed: aString symbolList: aSymbolList [
  "Answer the SymbolDictionary containing the named class.
	 If there are multiple answers, answer the first.
	 If there are no answers (i.e., the class does not exist), answer
	 the result of evaluating aBlock."

  ^ aSymbolList asArray detect: [ :each | each
        anySatisfy: [ :every | every isBehavior and: [ every name asString = aString asString ] ] ]
  ifNone: [  ]

]
