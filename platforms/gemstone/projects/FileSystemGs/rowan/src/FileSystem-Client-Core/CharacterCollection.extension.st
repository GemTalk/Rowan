Extension { #name : 'CharacterCollection' }

{ #category : '*FileSystem-Client-Core' }
CharacterCollection >> asClientFileReference [

	^ FileSystem clientDisk referenceTo: self

]

{ #category : '*FileSystem-Client-Core' }
CharacterCollection >> asClientPath [

	^ FileSystem clientDisk resolve: self

]
