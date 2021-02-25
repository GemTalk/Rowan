Class {
	#name : 'RowanQueryService',
	#superclass : 'RowanService',
	#instVars : [
		'queryResults'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'private' }
RowanQueryService >> basicBreakpointMethods [
  | bpMethods bpArray |
  bpMethods := Array new.
  bpArray := (GsNMethod _breakReport: true) at: 2.
  bpArray do: [ :array | bpMethods add: (array at: 5) ].
  ^ (self methodServicesFrom: bpMethods) asSet asArray
]

{ #category : 'queries' }
RowanQueryService >> breakPointMethods [
  queryResults := self basicBreakpointMethods.
  self returnQueryToClient
]

{ #category : 'queries' }
RowanQueryService >> browseClassReferences: className [

	| methods |
	methods := organizer referencesTo: className asSymbol.
	queryResults := self methodServicesFrom: methods first.
	self returnQueryToClient.
]

{ #category : 'queries' }
RowanQueryService >> browseReferencesTo: symbol [

	| methods |
	methods := organizer referencesTo: symbol.
	queryResults := self methodServicesFrom: methods first.
	self returnQueryToClient.
]

{ #category : 'private' }
RowanQueryService >> defaultProjectLogSize [

	^100
]

{ #category : 'queries' }
RowanQueryService >> gitTagListUsing: projectService [
  | answerString readStream |
  Rowan gitTools
    performGitCommand: 'fetch'
    in: projectService repositoryRootPath
    with: '--tags'.
  answerString := Rowan gitTools
    performGitCommand: 'tag'
    in: projectService repositoryRootPath
    with: '--sort=-taggerdate'.
  queryResults := Array new.
  readStream := ReadStream on: answerString.
  [ readStream atEnd ]
    whileFalse: [ queryResults add: (readStream upTo: Character lf) ].
  RowanCommandResult addResult: self
]

{ #category : 'queries' }
RowanQueryService >> hierarchyImplementorsOf: selector inClass: className [

	| methods behavior classes |
	behavior := Rowan globalNamed: className.
	classes := behavior allSuperclasses. 
	classes add: behavior. 
	classes addAll: (organizer allSubclassesOf: behavior). 
	methods := organizer implementorsOf: selector in: classes.
	queryResults := self methodServicesFrom: methods.
	self returnQueryToClient.
]

{ #category : 'queries' }
RowanQueryService >> hierarchySendersOf: selector inClass: className [

	| methods behavior classes |
	behavior := Rowan globalNamed: className.
	classes := behavior allSuperclasses. 
	classes add: behavior. 
	classes addAll: (organizer allSubclassesOf: behavior). 
	methods := organizer sendersOf: selector in: classes.
	queryResults := self methodServicesFrom: methods first.
	self setFirstReferenceUsing: queryResults and: methods. 
	self returnQueryToClient.
]

{ #category : 'queries' }
RowanQueryService >> implementorsOf: selector [

	| methods |
	methods := organizer implementorsOf: selector asSymbol.
	queryResults := self methodServicesFrom: methods.
	self returnQueryToClient.
]

{ #category : 'queries' }
RowanQueryService >> instVarReaders: instVarName in: className [
  | methods symbolAssociation theClasses |
  symbolAssociation := Rowan image symbolList resolveSymbol: className.
  symbolAssociation ifNil: [ ^ self ].
  theClasses := symbolAssociation value allSuperclasses.
  theClasses add: symbolAssociation value.
  theClasses addAll: (organizer allSubclassesOf: symbolAssociation value).
  methods := Array new.
  theClasses
    do: [ :theClass | 
      theClass
        methodsDo: [ :selector :method | 
          (method instVarsRead includes: instVarName asSymbol)
            ifTrue: [ methods add: method ] ] ].
  queryResults := self methodServicesFrom: methods.
  self returnQueryToClient
]

{ #category : 'queries' }
RowanQueryService >> instVarWriters: instVarName in: className [
  | methods symbolAssociation theClasses |
  symbolAssociation := Rowan image symbolList resolveSymbol: className.
  symbolAssociation ifNil: [ ^ self ].
  theClasses := symbolAssociation value allSuperclasses.
  theClasses add: symbolAssociation value.
  theClasses addAll: (organizer allSubclassesOf: symbolAssociation value).
  methods := Array new.
  theClasses
    do: [ :theClass | 
      theClass
        methodsDo: [ :selector :method | 
          (method instVarsWritten includes: instVarName asSymbol)
            ifTrue: [ methods add: method ] ] ].
  queryResults := self methodServicesFrom: methods.
  self returnQueryToClient
]

{ #category : 'queries' }
RowanQueryService >> literalReferences: string [

	| methods compilationResult |
	compilationResult := string evaluate.  
	methods := organizer referencesToLiteral: compilationResult.
	queryResults := self methodServicesFrom: methods first.
	self setFirstReferenceUsing: queryResults and: methods.
	self returnQueryToClient.
]

{ #category : 'queries' }
RowanQueryService >> methodsContaining: string [
  | methods sorted |
  methods := organizer substringSearch: string.
  sorted := SortedCollection
    sortBlock: [ :x :y | 
      x className = y className
        ifTrue: [ x selector < y selector ]
        ifFalse: [ x className < y className ] ].
  1 to: methods first size do: [ :index | 
    | methodService |
    methodService := self methodServiceFrom: (methods first at: index).
    methodService
      firstReference: ((methods at: 2) at: index);   "<<<< FIX HERE"
      searchString: string.
    sorted add: methodService ].
  queryResults := sorted asArray.
  self returnQueryToClient
]

{ #category : 'query support' }
RowanQueryService >> methodServiceFrom: method [
  ^ (self methodServicesFrom: (Array with: method)) first
]

{ #category : 'query support' }
RowanQueryService >> methodServicesFrom: methods [

	| sorted | 
	sorted := SortedCollection sortBlock: [:x :y | x className = y className ifTrue:[x selector < y selector] ifFalse:[x className < y className]].
	sorted addAll: (methods collect:[:gsNMethod | 
			RowanMethodService forSelector: gsNMethod selector class: gsNMethod inClass thisClass meta: gsNMethod inClass isMeta organizer: organizer]).
	^sorted asArray

]

{ #category : 'queries' }
RowanQueryService >> projectBranches: projectName [

	| project  |
	project := (RwProject newNamed: projectName). 
	queryResults := Rowan gitTools gitbranchIn: project repositoryRootPath with: ''.
	RowanCommandResult addResult: self
]

{ #category : 'queries' }
RowanQueryService >> projectLog: projectName [

	queryResults := (Rowan projectNamed: projectName) commitLog: self defaultProjectLogSize.
	RowanCommandResult addResult: self.
]

{ #category : 'queryResults' }
RowanQueryService >> queryResults [

	"for tests. So far, not needed on server." 

	^queryResults
]

{ #category : 'private' }
RowanQueryService >> returnQueryToClient [

	queryResults do:[:service |
		RowanCommandResult addResult: service].
	RowanCommandResult addResult: self.
]

{ #category : 'queries' }
RowanQueryService >> sendersOf: selector [

	| methods |
	methods := organizer sendersOf: selector asSymbol.
	queryResults := self methodServicesFrom: methods first.
	self setFirstReferenceUsing: queryResults and: methods. 
	self returnQueryToClient.
]

{ #category : 'queryResults' }
RowanQueryService >> setFirstReferenceUsing: results and: methods [
  results
    do: [ :methodService | 
      | gsMethod index |
      gsMethod := methods first
        detect: [ :meth | 
          meth selector = methodService selector
            and: [ meth inClass thisClass name asString = methodService className ] ]
        ifNone: [ 
          methodService firstReference: nil.
          nil ].
      gsMethod
        ifNotNil: [ 
          index := methods first indexOf: gsMethod.
          methodService firstReference: (methods last at: index) ] ]
]

{ #category : 'ston' }
RowanQueryService >> stonOn: stonWriter [   
	| instanceVariableNames |
	instanceVariableNames := self class allInstVarNames reject: [:iv | self excludedInstVars includes: iv].
	stonWriter writeObject: self
		streamMap: 
			[:dictionary |
			instanceVariableNames do: 
					[:each |
					(self instVarAt: (self class allInstVarNames indexOf: each asSymbol))
						ifNotNil: [:value | dictionary at: each asSymbol put: value]
						ifNil: [self stonShouldWriteNilInstVars ifTrue: [dictionary at: each asSymbol put: nil]]]]
]

{ #category : 'update' }
RowanQueryService >> updateInternalService: updatedService [

	"when sending services back to the client,
	verify any services held by this object are 
	updated. Services know what internal services
	they contain." 
	1 to: queryResults size do:[:index |
		| service |
		service := queryResults at: index. 
		service = updatedService ifTrue:[ 
			queryResults at: index put: updatedService
		]].
]
