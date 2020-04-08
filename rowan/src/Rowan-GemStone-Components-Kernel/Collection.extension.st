Extension { #name : 'Collection' }

{ #category : '*rowan-gemstone-components-kernel' }
Collection >> sort: aSortBlock [

	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	^ self sortWithBlock: aSortBlock

]
