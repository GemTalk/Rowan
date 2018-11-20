Extension { #name : 'CypressMethodStructure' }

{ #category : '*Cypress-MesssageDigest' }
addToDigest: aMessageDigestStream [

	aMessageDigestStream
		tab;
		tab;
		tab;
		tab;
		nextPutAll: self class name;
		cr;
		tab;
		tab;
		tab;
		tab;
		tab;
		nextPutAll: self selector;
		cr;
		tab;
		tab;
		tab;
		tab;
		tab;
		nextPutAll: 'properties:';
		cr;
		tab;
		tab;
		tab;
		tab;
		tab;
		tab.
	self properties _writeCypressJsonOn: aMessageDigestStream indent: 6.
	aMessageDigestStream
		cr;
		tab;
		tab;
		tab;
		tab;
		nextPutAll: 'source:';
		nextPutAll: self source;
		cr

]

{ #category : '*Cypress-MesssageDigest' }
isSkeleton [

	^source isNil
		and: [classStructure isNil
		and: [isMetaclass isNil]]

]
