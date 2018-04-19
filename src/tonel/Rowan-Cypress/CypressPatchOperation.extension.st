Extension { #name : 'CypressPatchOperation' }

{ #category : '*rowan-cypress' }
CypressPatchOperation >> <= other [
	^ self definition <= other definition
]

{ #category : '*rowan-cypress' }
CypressPatchOperation >> isAddition [
	^ false
]

{ #category : '*rowan-cypress' }
CypressPatchOperation >> isModification [
	^ false
]

{ #category : '*rowan-cypress' }
CypressPatchOperation >> isRemoval [
	^ false
]
