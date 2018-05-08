Extension { #name : 'CypressPatchOperation' }

{ #category : '*rowan-cypresskernel' }
CypressPatchOperation >> <= other [
	^ self definition <= other definition
]

{ #category : '*rowan-cypresskernel' }
CypressPatchOperation >> isAddition [
	^ false
]

{ #category : '*rowan-cypresskernel' }
CypressPatchOperation >> isModification [
	^ false
]

{ #category : '*rowan-cypresskernel' }
CypressPatchOperation >> isRemoval [
	^ false
]
