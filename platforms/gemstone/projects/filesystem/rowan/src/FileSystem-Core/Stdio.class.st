"
I am a facade class to access standard output streams:

- standard input
- standard output
- standard error

I initialize standard streams in a lazy fashion, asking to the VM for the #stdioHandles. I cache those standard streams and release them on shutdown.

The possible scenarios, i.e. for each of stdin, stderr and stdout:

    the stream is attached to a terminal (default on posix platforms, PharoConsole.exe on Windows (see below for Pharo.exe))
    the stream is redirected to a normal file, e.g. pharo pharo.image > output.txt
    the stream is redirected to a pipe, e.g. pharo pharo.image | tee output.txt

To know exactly which kind of stream you are using, you can use File >> #fileDescriptorType: (args are 0 1 2 for the stdios).


Windows Subtleties - This is from Pharo
=====================

If launched as a desktop app (not from the console), Pharo.exe will not be linked to external streams because none is created. To overcome that, the default behavior of this class is to create a normal file for the three stdio. This can be modified to:
	not create a file (execute #useNullStreams)
	use a memory stream (execute #useMemoryStreams).
"
Class {
	#name : 'Stdio',
	#superclass : 'Object',
	#category : 'FileSystem-Core-Streams'
}

{ #category : 'initialize' }
Stdio class >> cleanStdioHandles [
	#(stdin stdout stderr) do: [:ea | SessionTemps current removeKey: ea ifAbsent: []]
]

{ #category : 'accessing' }
Stdio class >> createReadStreamBlock [
	^ SessionTemps current at: #StdioCreateReadStreamBlock ifAbsentPut: [ self useDefaultStreams]
]

{ #category : 'accessing' }
Stdio class >> createReadStreamBlock: aBlock [
	SessionTemps current at: #StdioCreateReadStreamBlock put: aBlock.
	self cleanStdioHandles
]

{ #category : 'accessing' }
Stdio class >> createWriteStreamBlock [
	^ SessionTemps current at: #StdioCreateWriteStreamBlock ifAbsentPut: [ self useDefaultStreams]
]

{ #category : 'accessing' }
Stdio class >> createWriteStreamBlock: aBlock [
	SessionTemps current at: #StdioCreateWriteStreamBlock put: aBlock.
	self cleanStdioHandles
]

{ #category : 'accessing' }
Stdio class >> stderr [
	^self stdioWriteStreamFor: #stderr
]

{ #category : 'accessing' }
Stdio class >> stdin [
	^self stdioReadStreamFor: #stdin
]

{ #category : 'stdio' }
Stdio class >> stdioReadStreamFor: aSymbol [
	SessionTemps current at: #StdioCreateReadStreamBlock ifAbsent: [self useDefaultStreams].
	^SessionTemps current at: aSymbol ifAbsentPut: [
		self createReadStreamBlock value: aSymbol]
]

{ #category : 'stdio' }
Stdio class >> stdioWriteStreamFor: aSymbol [
	SessionTemps current at: #StdioCreateWriteStreamBlock ifAbsent: [self useDefaultStreams].
	^SessionTemps current at: aSymbol ifAbsentPut: [
		self createWriteStreamBlock value: aSymbol]
]

{ #category : 'accessing' }
Stdio class >> stdout [
	^self stdioWriteStreamFor: #stdout
]

{ #category : 'stdio' }
Stdio class >> useDefaultStreams [
	<script>
	| aBlock |
	aBlock := [ :aName | StdioStream on: (FileSystem fileClass perform: aName asSymbol) ].
	self createWriteStreamBlock: aBlock.
	self createReadStreamBlock: aBlock
]

{ #category : 'stdio' }
Stdio class >> useMemoryStreams [
	<script>
	self createWriteStreamBlock: 
		[ :aName | (FileSystem memory / aName asString) writeStream ].
	self createReadStreamBlock: 
		[ :aName | 
			EncodedBufferedStreamSpec newRead create on: (FileSystem memory / aName asString)  ].
]
