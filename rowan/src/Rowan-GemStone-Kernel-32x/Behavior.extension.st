Extension { #name : 'Behavior' }

{ #category : '*rowan-gemstone-kernel-32x' }
Behavior >> rwMethodCategories [
	^ self _baseCategorys: 0
]

{ #category : '*rowan-gemstone-kernel-32x' }
Behavior >> areInstancesSpecial [
	"3.6.0/Rowan compat"
	^ self isSpecial
]
