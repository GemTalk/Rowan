Extension { #name : 'CypressPatchOperation' }

{ #category : '*rowan-cypress-kernel' }
CypressPatchOperation >> <= other [
	^ self definition <= other definition
]

{ #category : '*rowan-cypress-kernel' }
CypressPatchOperation >> isAddition [
	^ false
]

{ #category : '*rowan-cypress-kernel' }
CypressPatchOperation >> isModification [
	^ false
]

{ #category : '*rowan-cypress-kernel' }
CypressPatchOperation >> isRemoval [
	^ false
]
