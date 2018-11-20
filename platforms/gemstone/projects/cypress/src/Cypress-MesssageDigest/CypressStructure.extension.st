Extension { #name : 'CypressStructure' }

{ #category : '*Cypress-MesssageDigest' }
CypressStructure >> addToDigest: aMessageDigestStream [

	self subclassResponsibility: #addToDigest:

]

{ #category : '*Cypress-MesssageDigest' }
CypressStructure >> digest [
	"Answer a digest of the receiver, unless it is a skeleton (never populated).
	 The digest is not constrained, but is typically a ByteArray or an Integer.
	 In the case of a skeleton, answer nil so there is a distinction between
	 no such package and an empty package."

	| stream |
	self isSkeleton ifTrue: [^nil].
	stream := CypressMessageDigestStream characters.
	self addToDigest: stream.
	^stream md5sum

]

{ #category : '*Cypress-MesssageDigest' }
CypressStructure >> isSkeleton [

	^self subclassResponsibility: #isSkeleton

]
