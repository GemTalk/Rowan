Class {
	#name : 'RowanQueryServicesTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

{ #category : 'tests' }
RowanQueryServicesTest >> test_dontUseFileStream [
  | queryService results expectedCount |
  self
    jadeiteIssueTested: #'issue611'
    withTitle: 'Use FileStreamPortable not FileStream'.
  queryService := RowanQueryService new.
  queryService organizer: ClassOrganizer new.
  queryService browseClassReferences: 'FileStream'.
  results := queryService queryResults.
	expectedCount := (System gemVersionAt: 'gsVersion') = '3.2.15'
		ifTrue: [ 1 ]
		ifFalse: [ 
			" 3.6.1"
			2 ].
  self assert: results size equals: expectedCount.
  self assert: results first className equals: 'Stream'.
  self
    assert: results first selector
    equals: #'installStreamImplementationFrom:'
]

{ #category : 'tests' }
RowanQueryServicesTest >> test_hierarchyImplementors [

	| queryService hierarchyClassNames |
	queryService := RowanQueryService new organizer: ClassOrganizer new. 
	queryService hierarchyImplementorsOf: #isEmpty inClass: 'RwModification'.
	hierarchyClassNames := Array with: 'RwModification'. 
	hierarchyClassNames addAll: (RwModification allSuperclasses collect:[:cls | cls name asString]).
	hierarchyClassNames addAll: (RwModification subclasses collect:[:cls | cls name asString]). 
	queryService queryResults do:[:methodService |
		self assert: methodService selector equals: #isEmpty.
		self assert: (hierarchyClassNames includes: methodService className asString)].
]

{ #category : 'tests' }
RowanQueryServicesTest >> test_hierarchyImplementorsGetsAllSubclasses [
	"was missing subclasses of subclasses" 

	| queryService subclassesImplementing |
	self jadeiteIssueTested: #issue242 withTitle: '`implementors in hierarchy` doesn''t include method in the currently selected class?'.
	queryService := RowanQueryService new organizer: ClassOrganizer new. 
	queryService hierarchyImplementorsOf: #initialize inClass: 'RwLoadedThing'.
	subclassesImplementing := Array new. 
	queryService queryResults do:[:methodService |
		| classService |
		self assert: methodService selector equals: #initialize.
		classService := RowanClassService forClassNamed: methodService className.
		methodService className ~= 'Object'
			ifTrue: [
				"Object >> initialize is implemented in 3.6.1, so ignore it"
				subclassesImplementing add:  methodService className.
				classService classHierarchy. 
				self assert: (classService classHierarchyNames includes: 'RwLoadedThing') ] ].
	self assert: (subclassesImplementing includes: 'RwLoadedThing'). 
	self assert: (subclassesImplementing includes: 'RwLoadedProject'). 
	self assert: (subclassesImplementing includes: 'RwGsLoadedSymbolDictPackage'). 
	self assert: (subclassesImplementing includes: 'RwGsLoadedSymbolDictClass'). 
	self assert: (subclassesImplementing includes: 'RwGsLoadedSymbolDictClassExtension').
]

{ #category : 'tests' }
RowanQueryServicesTest >> test_hierarchySenders [

	| queryService hierarchyClassNames |
	queryService := RowanQueryService new organizer: ClassOrganizer new. 
	queryService hierarchySendersOf: #isEmpty inClass: 'RwModification'.
	hierarchyClassNames := Array with: 'RwModification'. 
	hierarchyClassNames addAll: (RwModification allSuperclasses collect:[:cls | cls name asString]).
	hierarchyClassNames addAll: (RwModification allSubclasses collect:[:cls | cls name asString]). 
	queryService queryResults do:[:methodService |
		self assert: (methodService source includesString: 'isEmpty').
		self assert: (hierarchyClassNames includes: methodService className asString)].
]

{ #category : 'tests' }
RowanQueryServicesTest >> test_implementors [

	| queryService |
	queryService := RowanQueryService new organizer: ClassOrganizer new. 
	queryService implementorsOf: #size.
	queryService queryResults do:[:methodService |
		self assert: methodService selector equals: #size.].
]

{ #category : 'tests' }
RowanQueryServicesTest >> test_projectLog [

	| queryService result |
	queryService := RowanQueryService new.
	queryService projectLog: 'Rowan'. 
	result := RowanCommandResult results first. 
	self assert: (result isKindOf: RowanQueryService). 
	self assert: (result queryResults isKindOf: String)
]

{ #category : 'tests' }
RowanQueryServicesTest >> test_senders [

	| queryService |
	queryService := RowanQueryService new organizer: ClassOrganizer new. 
	queryService sendersOf: #isEmpty.
	queryService queryResults do:[:methodService |
		self assert: (methodService source includesString: 'isEmpty')].
]
