Class {
	#name : 'FileUnixOptionsTest',
	#superclass : 'FileOptionsTest',
	#category : 'FileSystem-Tests-Disk'
}

{ #category : 'private' }
FileUnixOptionsTest >> openAndAppendAndReadValue [
	^1090
]

{ #category : 'private' }
FileUnixOptionsTest >> openAndAppendValue [
	^1089
]

{ #category : 'private' }
FileUnixOptionsTest >> openReadWriteTruncateValue [
	^578
]

{ #category : 'private' }
FileUnixOptionsTest >> openReadWriteValue [
	^2
]

{ #category : 'private' }
FileUnixOptionsTest >> openWriteValue [
	^577
]

{ #category : 'private' }
FileUnixOptionsTest >> storeClass [
	^UnixStore
]

{ #category : 'tests' }
FileUnixOptionsTest >> testAccessRules [

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.

	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.

	aFileOptions openModeFlag: FileOpenWriteOnlyFlag new.
	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.

	aFileOptions openModeFlag: FileOpenReadWriteFlag new.
	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.
]

{ #category : 'tests' }
FileUnixOptionsTest >> testCreateRules [

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.

	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addPosixFlag: FileCreateFlag new.
	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addPosixFlag: FileExclusiveFlag new.
	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.
	self should: [aFileOptions truncateRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addPosixFlag: FileTruncateFlag new.
	self should: [aFileOptions truncateRule] raise: FileOptionFeatureNotSupported.
]

{ #category : 'tests' }
FileUnixOptionsTest >> testFileType [

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newAppend fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions fileType isNil
]

{ #category : 'tests' }
FileUnixOptionsTest >> testShareRules [

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.

	aFileOptions share: FileDenyReadFlag new.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.

	aFileOptions share: FileDenyWriteFlag new.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.

	aFileOptions share: FileDenyReadWriteFlag new.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.
]

{ #category : 'private' }
FileUnixOptionsTest >> valueAccessor [
	^#defaultPlatformValue
]
