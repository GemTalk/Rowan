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
	aFileOptions := FileSpec append fileOptions.
	aFileOptions parent store: self store.
	self assert: aFileOptions appendFlag posixFlagValue equals: (FileAppendFlag perform: self valueAccessor)
]

{ #category : 'tests' }
FileOptionsTest >> testOpenAndAppendAndReadMode [

	| aFileOptions |
	aFileOptions := FileSpec appendAndRead fileOptions.
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
	aFileOptions := FileSpec append fileOptions.
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
FileOptionsTest >> testReadOpenMode [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
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
	aFileOptions := FileSpec readWrite fileOptions.
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
	aFileOptions := FileSpec readWriteTruncated fileOptions.
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
	aFileOptions := FileSpec append fileOptions.
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
	aFileOptions := FileSpec write fileOptions.
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
