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

{ #category : 'accessing' }
RwComponentLoadConfiguration >> projectNames [

	^ projectNames ifNil: [ #() ]
]

{ #category : 'accessing' }
RwComponentLoadConfiguration >> projectNames: anArray [

	projectNames := anArray
]
