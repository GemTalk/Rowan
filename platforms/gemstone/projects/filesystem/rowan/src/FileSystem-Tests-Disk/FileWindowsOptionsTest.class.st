Class {
	#name : 'FileWindowsOptionsTest',
	#superclass : 'FileOptionsTest',
	#category : 'FileSystem-Tests-Disk'
}

{ #category : 'private' }
FileWindowsOptionsTest >> openAndAppendAndReadValue [
	^196610
]

{ #category : 'private' }
FileWindowsOptionsTest >> openAndAppendValue [
	^196609
]

{ #category : 'private' }
FileWindowsOptionsTest >> openReadWriteTruncateValue [
	^655362
]

{ #category : 'private' }
FileWindowsOptionsTest >> openReadWriteValue [
	^2
]

{ #category : 'private' }
FileWindowsOptionsTest >> openWriteValue [
	^655361
]

{ #category : 'private' }
FileWindowsOptionsTest >> storeClass [
	^WindowsStore
]

{ #category : 'tests' }
FileWindowsOptionsTest >> testAccessRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent store: self store.

	self assert: aFileOptions accessRule equals: WindowsStore readOnlyAccessRule.

	aFileOptions openPosixFlag: FileOpenWriteOnlyFlag new.
	self assert: aFileOptions accessRule equals: WindowsStore writeOnlyAccessRule.

	aFileOptions openPosixFlag: FileOpenReadWriteFlag new.
	self assert: aFileOptions accessRule equals: WindowsStore readWriteAccessRule.
]

{ #category : 'tests' }
FileWindowsOptionsTest >> testCreateRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent store: self store.

	self assert: aFileOptions createRule equals: WindowsStore openExistingRule.

	aFileOptions addFlag: FileCreateFlag new.
	self assert: aFileOptions createRule equals: WindowsStore openAlwaysRule.

	aFileOptions addFlag: FileExclusiveFlag new.
	self assert: aFileOptions createRule equals: WindowsStore createNewRule.
	self assert: aFileOptions truncateRule equals: WindowsStore noTruncateRule.

	aFileOptions addFlag: FileTruncateFlag new.
	self assert: aFileOptions truncateRule equals: WindowsStore truncateExistingRule.
]

{ #category : 'tests' }
FileWindowsOptionsTest >> testDefaultReadFileRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent store: self store.

	self assert: aFileOptions createRule equals: WindowsStore openExistingRule.
	self assert: aFileOptions shareRule equals: WindowsStore denyNoneShareRule.
	self assert: aFileOptions accessRule equals: WindowsStore readOnlyAccessRule
]

{ #category : 'tests' }
FileWindowsOptionsTest >> testFileType [

	| aFileOptions |
	aFileOptions := 
		FileSpec new 
			initializeForAppend; 
			beWindowsStore; 
			fileOptions.
	aFileOptions beBinaryFileType.

	self assert: aFileOptions fileType flagValue equals: 1.

	aFileOptions beTextFileType.
	self assert: aFileOptions fileType flagValue equals: 0
]

{ #category : 'tests' }
FileWindowsOptionsTest >> testShareRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions shareRule equals: WindowsStore denyNoneShareRule.

	aFileOptions share: FileDenyReadFlag new.
	self assert: aFileOptions shareRule equals: WindowsStore denyReadShareRule.

	aFileOptions share: FileDenyWriteFlag new.
	self assert: aFileOptions shareRule equals: WindowsStore denyWriteShareRule.

	aFileOptions share: FileDenyReadWriteFlag new.
	self assert: aFileOptions shareRule equals: WindowsStore denyReadWriteShareRule.
]

{ #category : 'private' }
FileWindowsOptionsTest >> valueAccessor [
	^#windowsValue
]
