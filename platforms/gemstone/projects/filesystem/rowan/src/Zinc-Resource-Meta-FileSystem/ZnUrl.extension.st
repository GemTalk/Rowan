Extension { #name : 'ZnUrl' }

{ #category : '*Zinc-Resource-Meta-FileSystem' }
ZnUrl >> asFileReference [
	"Convert the receiver into a new FileReference object.
	Note that for a FileReference a trailing slash is not relevant"

	| path |
	self assert: self scheme = #file description: 'Only a file:// URL can be converted to a FileReference'.
	self isSlash
		ifTrue: [ ^ FileSystem root ].
	path := self isDirectoryPath
		ifTrue: [ segments allButLast ]
		ifFalse: [ segments copy ].
	^ FileReference fileSystem: FileSystem disk path: (AbsolutePath withAll: path)
]

{ #category : '*Zinc-Resource-Meta-FileSystem' }
ZnUrl >> retrieveContents [
	"Download and return the resource that I refer to.
	This will typically return a String or a ByteArray.
	If something goes wrong, an exception will be signalled."
	
	" 'http://zn.stfx.eu/zn/numbers.txt' asZnUrl retrieveContents. "
	
	^ self performOperation: #retrieveContents
]

{ #category : '*Zinc-Resource-Meta-FileSystem' }
ZnUrl >> saveContentsToFile: aFilename [
	"Download and return a fileReference to the resource that I refer to.

	If something goes wrong, an exception will be signalled."
	
	" 'http://zn.stfx.eu/zn/numbers.txt' asZnUrl saveContentsToFile: Path * 'numbers.txt'. "
	
	^ self performOperation: #saveContentsToFile: with: aFilename
]
