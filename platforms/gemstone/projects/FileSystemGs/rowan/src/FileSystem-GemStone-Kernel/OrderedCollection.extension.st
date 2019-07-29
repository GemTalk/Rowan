Extension { #name : 'OrderedCollection' }

{ #category : '*filesystem-gemstone-kernel' }
OrderedCollection >> withIndexCollect: elementAndIndexBlock [ 
	"Just like with:collect: except that the iteration index supplies the second argument to the block. Override superclass in order to use addLast:, not at:put:."

	| newCollection |
	newCollection := self species new: self size.
	1 to: self size do:
		[:index |
		newCollection addLast: (
			elementAndIndexBlock
				value: (self at: index)
				value: index)].
	^ newCollection
]
