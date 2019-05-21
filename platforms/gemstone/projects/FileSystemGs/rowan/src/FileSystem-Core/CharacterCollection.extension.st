Extension { #name : 'CharacterCollection' }

{ #category : '*filesystem-core' }
CharacterCollection >> asFileReference [

	^ FileSystem disk referenceTo: self
]

{ #category : '*filesystem-core' }
CharacterCollection >> asPath [
	"convert myself to a path"
	"Examples:
		'.' asPath
		'~/Desktop' asPath
		'/home/foo' asPath
		'../../foo.bar' asPath"
	^ FileSystem disk resolve: self
]

{ #category : '*filesystem-core' }
CharacterCollection >> asPathWith: anObject [ 
	^ anObject pathFromString: self
]

{ #category : '*filesystem-core' }
CharacterCollection >> asResolvedBy: aFileSystem [
	^ aFileSystem resolveString: self
]
