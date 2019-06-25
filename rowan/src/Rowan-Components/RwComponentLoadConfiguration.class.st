Class {
	#name : 'RwComponentLoadConfiguration',
	#superclass : 'RwProjectLoadConfiguration',
	#instVars : [
		'projectNames'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwComponentLoadConfiguration >> _processConditionalPackageNames: aProjectLoadConfiguration [

	aProjectLoadConfiguration conditionalPackageMatchers keysAndValuesDo: [:platformMatchers :groupMap | 
		(self _platformAttributeMatchIn: platformMatchers)
			ifTrue: [
				groupMap keysAndValuesDo: [:group :map |
					(self groupNames includes: group)
						ifTrue: [ 
							self _addPackageNames: (map at: #packageNames ifAbsent: [ #() ]) for: aProjectLoadConfiguration.
							self configurationNames addAll: (map at: #configurationNames ifAbsent: [ #() ] ).
							self projectNames addAll: (map at: #projectNames ifAbsent: [ #() ] ) ] ] ] ].
]

{ #category : 'visiting' }
RwComponentLoadConfiguration >> acceptVisitor: aVisitor [

	^aVisitor visitComponentLoadConfiguration: self
]

{ #category : 'initialization' }
RwComponentLoadConfiguration >> initializeForExport [

	"for export, the keys in the dictionaris of the conditional packages structure need to be put into canonical order"

	| cp orderedConditionalPackages |
	super initializeForExport.
	cp := self conditionalPackages.
	orderedConditionalPackages := Rowan platform orderedDictionaryClass new.

	(cp keys asSortedCollection: [:a :b | (a at: 1) <= (b at: 1) ]) do: [:ar |
		| dict orderedPackageNames |
		dict := cp at: ar.
		orderedPackageNames := Rowan platform orderedDictionaryClass new.
		dict keys asArray sort do: [:group | orderedPackageNames at: group put: (dict at: group) ].
		orderedConditionalPackages at: ar put: orderedPackageNames ].
	
	conditionalPackages := orderedConditionalPackages
]

{ #category : 'accessing' }
RwComponentLoadConfiguration >> projectNames [

	^ projectNames ifNil: [ #() ]
]

{ #category : 'accessing' }
RwComponentLoadConfiguration >> projectNames: anArray [

	projectNames := anArray
]
