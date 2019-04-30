"
SUnit tests for FileSystemDirectoryEntry
"
Class {
	#name : 'DirectoryEntryTest',
	#superclass : 'TestCase',
	#category : 'FileSystem-Tests-Core'
}

{ #category : 'accessing' }
DirectoryEntryTest >> entry [
	System sessionIsOnStoneHost ifFalse: [ self error: 'DirectoryEntryTest needs to be run on the stone''s host' ].
	^ FileLocator extent1 resolve entry
]

{ #category : 'tests' }
DirectoryEntryTest >> testCreationTimeIsADateAndTimeInstance [
	"While creation is the message sent to a directory entry, creation returns a DateAndTime object"

	| creation |
	creation := self entry creation.
	self assert: creation class equals: DateAndTime
	
]

{ #category : 'tests' }
DirectoryEntryTest >> testIsDirectory [
	| ref entry |
	ref := FileLocator imageDirectory resolve.
	entry := ref entry.
	self assert: entry isDirectory.
	self assert: FileLocator imageDirectory resolve equals: FileLocator dbfScratchDir resolve
]

{ #category : 'tests' }
DirectoryEntryTest >> testIsFile [
	self assert: self entry isFile.
	self deny: self entry isDirectory
]

{ #category : 'tests' }
DirectoryEntryTest >> testIsNotDirectory [
	self deny: self entry isDirectory
]

{ #category : 'tests' }
DirectoryEntryTest >> testIsNotFile [
	| ref |
	ref := FileLocator imageDirectory resolve.
	self deny: ref entry isFile
]

{ #category : 'tests' }
DirectoryEntryTest >> testModificationTimeIsADateAndTimeInstance [
	"While modification is the message sent to a directory entry, modification returns a DateAndTime object"

	| modification |
	modification := self entry modification.
	self assert: modification class equals: DateAndTime
	
]

{ #category : 'tests' }
DirectoryEntryTest >> testReference [
	| ref entry |
	ref := FileLocator image resolve.
	entry := ref entry.
	self assert: entry reference equals: ref
]

{ #category : 'tests' }
DirectoryEntryTest >> testSize [
	self assert: self entry size _isInteger
]
