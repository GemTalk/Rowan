Extension { #name : 'RwAbstractResolvedObjectV2' }

{ #category : '*rowan-strawman1' }
RwAbstractResolvedObjectV2 >> readProjectComponentNames: componentNames groupNames: groupNames [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	^ self
		readProjectComponentNames: componentNames
		groupNames: groupNames
		platformConditionalAttributes: self platformConditionalAttributes
]

{ #category : '*rowan-strawman1' }
RwAbstractResolvedObjectV2 >> readProjectComponentNames: componentNames groupNames: groupNames platformConditionalAttributes: platformConditionalAttributes [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the current load specification"

	"return the receiver with a new set of definitions read from disk"

	self
		subclassResonsibility:
			#'readProjectComponentNames:groupNames:platformConditionalAttributes:'
]
