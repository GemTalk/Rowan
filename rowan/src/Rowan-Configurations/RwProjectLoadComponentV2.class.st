Class {
	#name : 'RwProjectLoadComponentV2',
	#superclass : 'RwAbstractProjectLoadComponentV2',
	#instVars : [
		'definedGroupNames',
		'projectNames'
	],
	#category : 'Rowan-Configurations'
}

{ #category : 'private' }
RwProjectLoadComponentV2 >> _processConditionalPackageNames: aProjectLoadConfiguration [

	aProjectLoadConfiguration conditionalPackageMatchers keysAndValuesDo: [:platformMatchers :groupMap | 
		(self _platformAttributeMatchIn: platformMatchers)
			ifTrue: [
				groupMap keysAndValuesDo: [:group :map |
					(self groupNames includes: group)
						ifTrue: [ 
							self _addPackageNames: (map at: #packageNames ifAbsent: [ #() ]) for: aProjectLoadConfiguration.
							self componentNames addAll: (map at: #componentNames ifAbsent: [ #() ] ).
							self projectNames addAll: (map at: #projectNames ifAbsent: [ #() ] ) ] ] ] ].
]

{ #category : 'visiting' }
RwProjectLoadComponentV2 >> acceptVisitor: aVisitor [

	^aVisitor visitComponentLoadConfiguration: self
]

{ #category : 'accessing' }
RwProjectLoadComponentV2 >> definedGroupNames [

	^ definedGroupNames 
		ifNil: [ definedGroupNames :=  Dictionary new]
]

{ #category : 'accessing' }
RwProjectLoadComponentV2 >> definedGroupNames: aColl [

	definedGroupNames := aColl

]

{ #category : 'accessing' }
RwProjectLoadComponentV2 >> defineGroupNamed: groupName [ 

	"add a group that does not include any other groups"

	self defineGroupNamed: groupName toIncludeGroups: #()
]

{ #category : 'accessing' }
RwProjectLoadComponentV2 >> defineGroupNamed: groupName toIncludeGroups: includedGroups [

	"add a group that does not include any other groups"

	self definedGroupNames at: groupName put: includedGroups asArray sort
]

{ #category : 'accessing' }
RwProjectLoadComponentV2 >> projectNames [

	^ projectNames ifNil: [ #() ]
]

{ #category : 'accessing' }
RwProjectLoadComponentV2 >> projectNames: anArray [

	projectNames := anArray
]
