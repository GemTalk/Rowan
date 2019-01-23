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
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent store: self store.

	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.

	aFileOptions mode: FileOpenWriteOnlyFlag new.
	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.

	aFileOptions mode: FileOpenReadWriteFlag new.
	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.
]

{ #category : 'tests' }
FileUnixOptionsTest >> testCreateRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent store: self store.

	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addOption: FileCreateFlag new.
	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addOption: FileExclusiveFlag new.
	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.
	self should: [aFileOptions truncateRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addOption: FileTruncateFlag new.
	self should: [aFileOptions truncateRule] raise: FileOptionFeatureNotSupported.
]

{ #category : 'tests' }
FileUnixOptionsTest >> testFileType [

	| aFileOptions |
	aFileOptions := FileSpec append fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions fileType isNil
]

{ #category : 'tests' }
FileUnixOptionsTest >> testShareRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
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
