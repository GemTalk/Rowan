Class {
	#name : 'StdioTest',
	#superclass : 'TestCase',
	#category : 'Files-Tests-Streams'
}

{ #category : 'testing' }
StdioTest >> clearCache [
	SessionTemps current removeKey: #StdioCreateReadStreamBlock ifAbsent: [].
	SessionTemps current removeKey: #StdioCreateWriteStreamBlock ifAbsent: [].
	self deny: (SessionTemps current includesKey: #StdioCreateReadStreamBlock).
	self deny: (SessionTemps current includesKey: #StdioCreateWriteStreamBlock).
	Stdio cleanStdioHandles.
	self verifyStdInstanceCleared.
]

{ #category : 'testing' }
StdioTest >> testInitialization [
	| stdio |
	self clearCache.
	stdio := Stdio stdout.
	self assert: (stdio file isKindOf: FileSystem fileClass).
	self assert: stdio isWritable.

	stdio := Stdio stderr.
	self assert: (stdio file isKindOf: FileSystem fileClass).
	self assert: stdio isWritable.

	stdio := Stdio stdin.
	self assert: (stdio file isKindOf: FileSystem fileClass).
	self deny: stdio isWritable
]

{ #category : 'testing' }
StdioTest >> testUseMemoryStreams [
	| stdio |
	self clearCache.
	stdio := Stdio stdout.
	self assert: (stdio file isKindOf: FileSystem fileClass).
	self assert: stdio isWritable.

	Stdio useMemoryStreams.
	self verifyStdInstanceCleared.

	stdio := Stdio stdout.
	self assert: (stdio isKindOf: ZnCharacterWriteStream).

	stdio := Stdio stderr.
	self assert: (stdio isKindOf: ZnCharacterWriteStream).

	stdio := Stdio stdin.
	self assert: (stdio isKindOf: ZnCharacterWriteStream).

]

{ #category : 'testing' }
StdioTest >> verifyStdInstanceCleared [
	#(stdin stdout stderr) do: [:ea | self deny: (SessionTemps current includesKey: ea)].
]
