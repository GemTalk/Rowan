Class {
	#name : 'FileOptionsTest',
	#superclass : 'TestCase',
	#instVars : [
		'stream'
	],
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'Testing' }
FileOptionsTest class >> isAbstract [
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #FileOptionsTest
]

{ #category : 'private' }
FileOptionsTest >> _testPermissionFlag: aPermissionFlagClass in: aFileOptions [
	self assert: aFileOptions accessPermissions equals: (aPermissionFlagClass perform: self valueAccessor)
]

{ #category : 'private' }
FileOptionsTest >> fileOptions [
	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.
	^aFileOptions
]

{ #category : 'private' }
FileOptionsTest >> groupAllOctalValue [
	^'70'
]

{ #category : 'private' }
FileOptionsTest >> otherAllOctalValue [
	^'7'
]

{ #category : 'private' }
FileOptionsTest >> ownerAllOctalValue [
	^'700'
]

{ #category : 'private' }
FileOptionsTest >> store [
	^self storeClass createDefault
]

{ #category : 'private' }
FileOptionsTest >> storeClass [
	self subclassResponsibity
]

{ #category : 'tests' }
FileOptionsTest >> testAppendFlag [

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newAppend fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions appendFlag posixFlagValue equals: (FileAppendFlag perform: self valueAccessor)
]

{ #category : 'tests' }
FileOptionsTest >> testGroupAccessPermissionFlags [
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupAll.
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupAll groupRead.
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupRead groupAll.
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupRead groupWrite groupExecute.
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupRead groupWrite groupExecute groupAll.
	self assert: (self fileOptions groupRead groupWrite groupExecute accessPermissions printStringRadix: 8) equals: self groupAllOctalValue.
]

{ #category : 'tests' }
FileOptionsTest >> testIndividualAccessPermissionFlags [

	self assert: (self fileOptions groupAll accessPermissions printStringRadix: 8) equals: self groupAllOctalValue.
	self _testPermissionFlag: FileGroupAllPermissionFlag in: self fileOptions groupAll.
	self _testPermissionFlag: FileGroupExecutePermissionFlag in: self fileOptions groupExecute.
	self _testPermissionFlag: FileGroupReadPermissionFlag in: self fileOptions groupRead.
	self _testPermissionFlag: FileGroupWritePermissionFlag in: self fileOptions groupWrite.

	self assert: (self fileOptions otherAll accessPermissions printStringRadix: 8) equals: self otherAllOctalValue.
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherAll.
	self _testPermissionFlag: FileOtherExecutePermissionFlag in: self fileOptions otherExecute.
	self _testPermissionFlag: FileOtherReadPermissionFlag in: self fileOptions otherRead.
	self _testPermissionFlag: FileOtherWritePermissionFlag in: self fileOptions otherWrite.

	self assert: (self fileOptions ownerAll accessPermissions printStringRadix: 8) equals: self ownerAllOctalValue.
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerAll.
	self _testPermissionFlag: FileOwnerExecutePermissionFlag in: self fileOptions ownerExecute.
	self _testPermissionFlag: FileOwnerReadPermissionFlag in: self fileOptions ownerRead.
	self _testPermissionFlag: FileOwnerWritePermissionFlag in: self fileOptions ownerWrite.
]

{ #category : 'tests' }
FileOptionsTest >> testOpenAndAppendAndReadMode [

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newAppend read fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions posixFlags size equals: 2.
	self assert: aFileOptions hasFileCreateFlag.
	self assert: aFileOptions hasFileAppendFlag.
	self deny: aFileOptions hasFileTruncateFlag.
	self assert: aFileOptions mode equals: 'a+'.
	self assert: aFileOptions fileOpenModeValue equals: 2.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: self openAndAppendAndReadValue
]

{ #category : 'tests' }
FileOptionsTest >> testOpenAppendMode [

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newAppend fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions isWritable.
	self deny: aFileOptions isReadable.
	self assert: aFileOptions posixFlags size equals: 2.
	self assert: aFileOptions hasFileCreateFlag.
	self assert: aFileOptions hasFileAppendFlag.
	self assert: aFileOptions mode equals: 'a'.
	self assert: aFileOptions fileOpenModeValue equals: 1.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: self openAndAppendValue
]

{ #category : 'tests' }
FileOptionsTest >> testOtherAccessPermissionFlags [
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherAll.
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherAll otherRead.
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherRead otherAll.
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherRead otherWrite otherExecute.
	self _testPermissionFlag: FileOtherAllPermissionFlag in: self fileOptions otherRead otherWrite otherExecute otherAll.
	self assert: (self fileOptions otherRead otherWrite otherExecute accessPermissions printStringRadix: 8) equals: self otherAllOctalValue.
]

{ #category : 'tests' }
FileOptionsTest >> testOwnerAccessPermissionFlags [
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerAll.
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerAll ownerRead.
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerRead ownerAll.
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerRead ownerWrite ownerExecute.
	self _testPermissionFlag: FileOwnerAllPermissionFlag in: self fileOptions ownerRead ownerWrite ownerExecute ownerAll.
	self assert: (self fileOptions ownerRead ownerWrite ownerExecute accessPermissions printStringRadix: 8) equals: self ownerAllOctalValue
]

{ #category : 'tests' }
FileOptionsTest >> testReadOpenMode [

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newRead fileOptions.
	aFileOptions parent store: self store.
	self deny: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions posixFlags isEmpty.
	self deny: aFileOptions hasFileCreateFlag.
	self deny: aFileOptions hasFileTruncateFlag.
	self assert: aFileOptions mode equals: 'r'.
	self assert: aFileOptions fileOpenModeValue equals: 0.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: 0
]

{ #category : 'tests' }
FileOptionsTest >> testReadWriteOpenMode [

	| aFileOptions|
	aFileOptions := EncodedBufferedStreamSpec newReadWrite fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self deny: aFileOptions hasFileCreateFlag.
	self deny: aFileOptions hasFileTruncateFlag.
	self assert: aFileOptions mode equals: 'r+'.
	self assert: aFileOptions fileOpenModeValue equals: 2.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: self openReadWriteValue
]

{ #category : 'tests' }
FileOptionsTest >> testReadWriteTruncateOpenMode [

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newReadWrite truncate create fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions posixFlags size equals: 2.
	self assert: aFileOptions hasFileCreateFlag.
	self assert: aFileOptions hasFileTruncateFlag.
	self assert: aFileOptions mode equals: 'w+'.
	self assert: aFileOptions fileOpenModeValue equals: 2.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: self openReadWriteTruncateValue
]

{ #category : 'tests' }
FileOptionsTest >> testShareFlags [

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newAppend fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions share posixFlagValue equals: (FileDenyNoneFlag perform: self valueAccessor).
	aFileOptions beReadOnlyShared.
	self assert: aFileOptions share posixFlagValue equals: (FileDenyWriteFlag perform: self valueAccessor).
	aFileOptions beWriteOnlyShared.
	self assert: aFileOptions share posixFlagValue equals: (FileDenyReadFlag perform: self valueAccessor).
	aFileOptions beNonShared.
	self assert: aFileOptions share posixFlagValue equals: (FileDenyReadWriteFlag perform: self valueAccessor).
]

{ #category : 'tests' }
FileOptionsTest >> testWriteOpenMode [

	| aFileOptions |
	aFileOptions := EncodedBufferedStreamSpec newWrite fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions isWritable.
	self deny: aFileOptions isReadable.
	self assert: aFileOptions posixFlags size equals: 2.
	self assert: aFileOptions hasFileCreateFlag.
	self assert: aFileOptions hasFileTruncateFlag.
	self assert: aFileOptions mode equals: 'w'.
	self assert: aFileOptions fileOpenModeValue equals: 1.
	self assert: aFileOptions fileOpenFlagsTotalValue equals: self openWriteValue
]
