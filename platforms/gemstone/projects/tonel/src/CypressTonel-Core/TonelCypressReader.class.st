"
I'm a reader for tonel format.
"
Class {
	#name : 'TonelCypressReader',
	#superclass : 'CypressAbstractPackageFiler',
	#instVars : [
		'packageName',
		'definitions',
		'directoryPath'
	],
	#category : 'CypressTonel-Core'
}

{ #category : 'definition creation' }
TonelCypressReader class >> definitionForType: aString [
  aString = TonelWriter classLabel
    ifTrue: [ ^ CypressClassDefinition ].
  aString = TonelWriter extensionLabel
    ifTrue: [ ^ nil ].
  TonelParseError signal: 'Unknown type declaration.'

]

{ #category : 'accessing' }
TonelCypressReader class >> definitionOrders [
	"Used to sort definitions inside a snapshot"
	^ Dictionary new
		at: CypressMethodDefinition put: 1;
		at: CypressClassDefinition put: 2;
		yourself

]

{ #category : 'definition creation' }
TonelCypressReader class >> newClassDefinitionForClassNamed: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString [
  ^ CypressClassDefinition
    name: nameString
    superclassName: superclassString
    category: categoryString
    instVarNames: ivarArray
    classInstVarNames: civarArray
    classVarNames: cvarArray
    poolDictionaryNames: poolArray
    comment: commentString
    type: typeSymbol

]

{ #category : 'definition creation' }
TonelCypressReader class >> newClassDefinitionFrom: anArray [
  | metadata |
  metadata := anArray sixth.
  ^ CypressClassDefinition
    name: (metadata at: #'name')
    superclassName: (metadata at: #'superclass')
    category: (metadata at: #'category')
    instVarNames: (metadata at: #'instvars' ifAbsent: [ #() ])
    classInstVarNames: (metadata at: #'classinstvars' ifAbsent: [ #() ])
    classVarNames: (metadata at: #'classvars' ifAbsent: [ #() ])
    poolDictionaryNames: (metadata at: #'pools' ifAbsent: [ #() ])
    comment: (anArray second ifNil: [ '' ])
    type: (metadata at: #'type' ifAbsent: [ #'normal' ]) asSymbol

]

{ #category : 'definition creation' }
TonelCypressReader class >> newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source [
  ^ CypressMethodDefinition
    className: className
    classIsMeta: meta
    selector: selector
    category: category
    source: source

]

{ #category : 'definition creation' }
TonelCypressReader class >> newTypeDefinitionFrom: anArray [
  | typeClass |
  typeClass := self definitionForType: anArray fourth.
  typeClass = CypressClassDefinition
    ifTrue: [ ^ self newClassDefinitionFrom: anArray ].
  "is extension, no type"
  ^ nil

]

{ #category : 'reading' }
TonelCypressReader class >> on: rootDirectoryPath fileName: packageName [
  | reader |
  reader := self new.
  ^ reader
    packageDirectory:
        rootDirectoryPath , reader fileUtils pathNameDelimiter , packageName;
    packageName: packageName;
    yourself

]

{ #category : 'private' }
TonelCypressReader >> canBeLoaded: aFileReference [
  | fileName |
  fileName := self fileUtils localNameFrom: aFileReference.
  ^ fileName ~= 'package.st' and: [ fileName endsWith: '.st' ]

]

{ #category : 'private' }
TonelCypressReader >> categoriesFrom: aCollection [
  ^ ((aCollection select: [ :each | each isClassDefinition ])
    collect: [ :each | each category asSymbol ]) asSet sortWithBlock: [ :a :b | a < b ]

]

{ #category : 'parser support' }
TonelCypressReader >> definitionForType: aString [
  ^ self class definitionForType: aString

]

{ #category : 'accessing' }
TonelCypressReader >> definitions [
  ^ definitions

]

{ #category : 'accessing' }
TonelCypressReader >> directoryPath [
  ^ directoryPath

]

{ #category : 'accessing' }
TonelCypressReader >> directoryPath: aString [
  directoryPath := aString

]

{ #category : 'initializing - private' }
TonelCypressReader >> initializeForRepository: aCypressFileSystemRepository [
  repository := aCypressFileSystemRepository.
  self directoryPath: repository directoryPath

]

{ #category : 'loading' }
TonelCypressReader >> loadDefinitions [
  "load all definitions in the known directories and files."
  | definitionOrders newDefinitions loadable dir |
  definitionOrders := self class definitionOrders.
  loadable := (self fileUtils directoryEntriesFrom: (dir := self packageDirectory))
               select: [ :each | self canBeLoaded: each ].
  loadable isEmpty ifTrue:[ GsFile gciLogServer:'No loadable files found in ' , dir asString ].
  newDefinitions := ( 
    loadable collect: [ :each | 
    GsFile gciLogServer:'Reading ' , each asString .
      self fileUtils
        readStreamFor: each
        do: [ :s | TonelParser parseStream: s forReader: self ] ])
    flattened
    sort: [ :a :b | (definitionOrders at: a class) < (definitionOrders at: b class) ].
  definitions := newDefinitions sort

]

{ #category : 'loading' }
TonelCypressReader >> loadDefinitionsFromFiles: fileNames [
  "From the known directories and files, load only those files
   whose file names end with one of the Strings in the Array  fileNames ."
  | definitionOrders newDefinitions loadable dir |
  definitionOrders := self class definitionOrders.
  loadable := (self fileUtils directoryEntriesFrom: (dir := self packageDirectory))
        select: [ :each | (self canBeLoaded: each) and:[ 
                  (fileNames detect:[:n | each endsWith: n] ifNone:[nil]) ~~ nil] ].
  loadable isEmpty ifTrue:[ 
      GsFile gciLogServer: 'None of specified  files found in ' , dir asString ].
  newDefinitions := ( 
    loadable collect: [ :each | 
    GsFile gciLogServer:'Reading ' , each asString .
      self fileUtils
        readStreamFor: each
        do: [ :s | TonelParser parseStream: s forReader: self ] ])
    flattened
    sort: [ :a :b | (definitionOrders at: a class) < (definitionOrders at: b class) ].
  definitions := newDefinitions sort

]

{ #category : 'parser support' }
TonelCypressReader >> newClassDefinitionFrom: anArray [
	^self class newClassDefinitionFrom: anArray

]

{ #category : 'parser support' }
TonelCypressReader >> newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source [
  ^ self class
    newMethodDefinitionForClassNamed: className
    classIsMeta: meta
    selector: selector
    category: category
    source: source

]

{ #category : 'parser support' }
TonelCypressReader >> newTraitDefinitionFrom: anArray [
	^ self class newTraitDefinitionFrom: anArray

]

{ #category : 'parser support' }
TonelCypressReader >> newTypeDefinitionFrom: anArray [
	^ self class newTypeDefinitionFrom: anArray

]

{ #category : 'accessing' }
TonelCypressReader >> packageName [

   ^packageName

]

{ #category : 'accessing' }
TonelCypressReader >> packageName: aString [
  packageName := aString.
  self
    packageDirectory:
      (self fileUtils directoryFromPath: packageName relativeTo: self directoryPath)

]

{ #category : 'reading' }
TonelCypressReader >> readPackageStructure [
  self loadDefinitions.
  ^ packageStructure
    fromSnapshot: definitions;
    yourself

]
