Extension { #name : 'CypressPatchOperation' }

{ #category : '*rowan-tode' }
CypressPatchOperation >> <= other [
	^ self definition <= other definition
]

{ #category : '*rowan-tode' }
CypressPatchOperation >> isAddition [
	^ false
]

{ #category : '*rowan-tode' }
CypressPatchOperation >> isModification [
	^ false
]

{ #category : '*rowan-tode' }
CypressPatchOperation >> isRemoval [
	^ false
]
