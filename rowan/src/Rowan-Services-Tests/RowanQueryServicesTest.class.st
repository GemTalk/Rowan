Class {
	#name : 'RowanQueryServicesTest',
	#superclass : 'RowanServicesTest',
	#category : 'Rowan-Services-Tests'
}

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
RowanQueryServicesTest >> test_hierarchySenders [

	| queryService hierarchyClassNames |
	queryService := RowanQueryService new organizer: ClassOrganizer new. 
	queryService hierarchySendersOf: #isEmpty inClass: 'RwModification'.
	hierarchyClassNames := Array with: 'RwModification'. 
	hierarchyClassNames addAll: (RwModification allSuperclasses collect:[:cls | cls name asString]).
	hierarchyClassNames addAll: (RwModification subclasses collect:[:cls | cls name asString]). 
	queryService queryResults do:[:methodService |
		self assert: (methodService source includesString: 'isEmpty').
		self assert: (hierarchyClassNames includes: methodService className asString)].
]

{ #category : 'tests' }
RowanQueryServicesTest >> test_implementors [

	| queryService hierarchyClassNames |
	queryService := RowanQueryService new organizer: ClassOrganizer new. 
	queryService implementorsOf: #size.
	queryService queryResults do:[:methodService |
		self assert: methodService selector equals: #size.].
]

{ #category : 'tests' }
RowanQueryServicesTest >> test_senders [

	| queryService hierarchyClassNames |
	queryService := RowanQueryService new organizer: ClassOrganizer new. 
	queryService sendersOf: #isEmpty.
	queryService queryResults do:[:methodService |
		self assert: (methodService source includesString: 'isEmpty')].
]
