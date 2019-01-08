Class {
	#name : 'FileOptionsTest',
	#superclass : 'TestCase',
	#instVars : [
		'stream'
	],
	#category : 'FileSystem-Tests-Disk-Base'
}

{ #category : 'tests' }
FileOptionsTest >> testLinixShareRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.

	aFileOptions share: FileDenyReadOption new.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.

	aFileOptions share: FileDenyWriteOption new.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.

	aFileOptions share: FileDenyReadWriteOption new.
	self should: [aFileOptions shareRule] raise: FileOptionFeatureNotSupported.
]

{ #category : 'tests' }
FileOptionsTest >> testLinuxAccessRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.

	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.

	aFileOptions mode: FileOpenWriteOnlyMode new.
	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.

	aFileOptions mode: FileOpenReadWriteMode new.
	self should: [aFileOptions accessRule] raise: FileOptionFeatureNotSupported.
]

{ #category : 'tests' }
FileOptionsTest >> testLinuxAppendAndReadOpenMode [

	| aFileOptions |
	aFileOptions := FileSpec appendAndRead fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions options size equals: 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileAppendOption.
	self deny: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString equals: 'a+'.
	self assert: aFileOptions fileOpenModeValue equals: 2.
	self assert: aFileOptions fileOpenAndOptionsValue equals: 1090
]

{ #category : 'tests' }
FileOptionsTest >> testLinuxAppendOpenMode [

	| aFileOptions |
	aFileOptions := FileSpec append fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.
	self assert: aFileOptions isWritable.
	self deny: aFileOptions isReadable.
	self assert: aFileOptions options size equals: 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileAppendOption.
	self assert: aFileOptions modeString equals: 'a'.
	self assert: aFileOptions fileOpenModeValue equals: 1.
	self assert: aFileOptions fileOpenAndOptionsValue equals: 1089
]

{ #category : 'tests' }
FileOptionsTest >> testLinuxAppendOption [

	| aFileOptions |
	aFileOptions := FileSpec append fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.
	self assert: aFileOptions appendOption optionValue equals: FileAppendOption defaultPlatformValue
]

{ #category : 'tests' }
FileOptionsTest >> testLinuxCreateRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.

	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addOption: FileCreateOption new.
	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addOption: FileExclusiveOption new.
	self should: [aFileOptions createRule] raise: FileOptionFeatureNotSupported.
	self should: [aFileOptions truncateRule] raise: FileOptionFeatureNotSupported.

	aFileOptions addOption: FileTruncateOption new.
	self should: [aFileOptions truncateRule] raise: FileOptionFeatureNotSupported.
]

{ #category : 'tests' }
FileOptionsTest >> testLinuxFileType [

	| aFileOptions |
	aFileOptions := FileSpec append fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.
	self assert: aFileOptions fileType isNil
]

{ #category : 'tests' }
FileOptionsTest >> testLinuxReadOpenMode [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.
	self deny: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions options isEmpty.
	self deny: aFileOptions hasFileCreateOption.
	self deny: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString equals: 'r'.
	self assert: aFileOptions fileOpenModeValue equals: 0.
	self assert: aFileOptions fileOpenAndOptionsValue equals: 0
]

{ #category : 'tests' }
FileOptionsTest >> testLinuxReadWriteOpenMode [

	| aFileOptions|
	aFileOptions := FileSpec readWrite fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self deny: aFileOptions hasFileCreateOption.
	self deny: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString equals: 'r+'.
	self assert: aFileOptions fileOpenModeValue equals: 2.
	self assert: aFileOptions fileOpenAndOptionsValue equals: 2
]

{ #category : 'tests' }
FileOptionsTest >> testLinuxReadWriteTruncateOpenMode [

	| aFileOptions |
	aFileOptions := FileSpec readWriteTruncated fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions options size equals: 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString equals: 'w+'.
	self assert: aFileOptions fileOpenModeValue equals: 2.
	self assert: aFileOptions fileOpenAndOptionsValue equals: 578
]

{ #category : 'tests' }
FileOptionsTest >> testLinuxShareOptions [

	| aFileOptions |
	aFileOptions := FileSpec append fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.
	self assert: aFileOptions share optionValue equals: FileDenyNoneOption defaultPlatformValue.
	aFileOptions beReadOnlyShared.
	self assert: aFileOptions share optionValue equals: FileDenyWriteOption defaultPlatformValue.
	aFileOptions beWriteOnlyShared.
	self assert: aFileOptions share optionValue equals: FileDenyReadOption defaultPlatformValue.
	aFileOptions bePrivate.
	self assert: aFileOptions share optionValue equals: FileDenyReadWriteOption defaultPlatformValue
]

{ #category : 'tests' }
FileOptionsTest >> testLinuxWriteOpenMode [

	| aFileOptions |
	aFileOptions := FileSpec write fileOptions.
	aFileOptions parent fileStore: UnixStore createDefault.
	self assert: aFileOptions isWritable.
	self deny: aFileOptions isReadable.
	self assert: aFileOptions options size equals: 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString equals: 'w'.
	self assert: aFileOptions fileOpenModeValue equals: 1.
	self assert: aFileOptions fileOpenAndOptionsValue equals: 577
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsAccessRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent fileStore: WindowsStore createDefault.
	self assert: aFileOptions accessRule equals: WindowsStore readOnlyAccessRule.

	aFileOptions mode: FileOpenWriteOnlyMode new.
	self assert: aFileOptions accessRule equals: WindowsStore writeOnlyAccessRule.

	aFileOptions mode: FileOpenReadWriteMode new.
	self assert: aFileOptions accessRule equals: WindowsStore readWriteAccessRule.
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsAppendAndReadOpenMode [

	| aFileOptions |
	aFileOptions := FileSpec appendAndRead fileOptions.
	aFileOptions parent fileStore: WindowsStore createDefault.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions options size equals: 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileAppendOption.
	self deny: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString equals: 'a+'.
	self assert: aFileOptions fileOpenModeValue equals: 2.
	self assert: aFileOptions fileOpenAndOptionsValue equals: 196610
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsAppendOpenMode [

	| aFileOptions |
	aFileOptions := FileSpec append fileOptions.
	aFileOptions parent fileStore: WindowsStore createDefault.
	self assert: aFileOptions isWritable.
	self deny: aFileOptions isReadable.
	self assert: aFileOptions options size equals: 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileAppendOption.
	self assert: aFileOptions modeString equals: 'a'.
	self assert: aFileOptions fileOpenModeValue equals: 1.
	self assert: aFileOptions fileOpenAndOptionsValue equals: 196609
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsAppendOption [

	| aFileOptions |
	aFileOptions := FileSpec append fileOptions.
	aFileOptions parent fileStore: WindowsStore createDefault.
	self assert: aFileOptions appendOption optionValue equals: FileAppendOption windowsValue
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsCreateRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent fileStore: WindowsStore createDefault.
	self assert: aFileOptions createRule equals: WindowsStore openExistingRule.

	aFileOptions addOption: FileCreateOption new.
	self assert: aFileOptions createRule equals: WindowsStore openAlwaysRule.

	aFileOptions addOption: FileExclusiveOption new.
	self assert: aFileOptions createRule equals: WindowsStore createNewRule.
	self assert: aFileOptions truncateRule equals: WindowsStore noTruncateRule.

	aFileOptions addOption: FileTruncateOption new.
	self assert: aFileOptions truncateRule equals: WindowsStore truncateExistingRule.
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsFileType [

	| aFileOptions |
	aFileOptions := 
		FileSpec basicNew 
			initializeForAppend; 
			beWindowsStore; 
			fileOptions.
	aFileOptions beBinaryFileType.

	self assert: aFileOptions fileType optionValue equals: 1.

	aFileOptions beTextFileType.
	self assert: aFileOptions fileType optionValue equals: 0
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsReadOpenMode [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent fileStore: WindowsStore createDefault.
	self deny: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions options isEmpty.
	self deny: aFileOptions hasFileCreateOption.
	self deny: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString equals: 'r'.
	self assert: aFileOptions fileOpenModeValue equals: 0.
	self assert: aFileOptions fileOpenAndOptionsValue equals: 0.

	self assert: aFileOptions createRule equals: WindowsStore openExistingRule.
	self assert: aFileOptions shareRule equals: WindowsStore denyNoneShareRule.
	self assert: aFileOptions accessRule equals: WindowsStore readOnlyAccessRule
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsReadWriteTruncateOpenMode [

	| aFileOptions |
	aFileOptions := FileSpec readWriteTruncated fileOptions.
	aFileOptions parent fileStore: WindowsStore createDefault.
	self assert: aFileOptions isWritable.
	self assert: aFileOptions isReadable.
	self assert: aFileOptions options size equals: 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString equals: 'w+'.
	self assert: aFileOptions fileOpenModeValue equals: 2.
	self assert: aFileOptions fileOpenAndOptionsValue equals: 655362
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent fileStore: WindowsStore createDefault.

	self assert: aFileOptions createRule equals: WindowsStore openExistingRule.
	self assert: aFileOptions shareRule equals: WindowsStore denyNoneShareRule.
	self assert: aFileOptions accessRule equals: WindowsStore readOnlyAccessRule
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsShareOptions [

	| aFileOptions |
	aFileOptions := FileSpec append fileOptions.
	aFileOptions parent fileStore: WindowsStore createDefault.
	self assert: aFileOptions share optionValue equals: FileDenyNoneOption windowsValue.
	aFileOptions beReadOnlyShared.
	self assert: aFileOptions share optionValue equals: FileDenyWriteOption windowsValue.
	aFileOptions beWriteOnlyShared.
	self assert: aFileOptions share optionValue equals: FileDenyReadOption windowsValue.
	aFileOptions bePrivate.
	self assert: aFileOptions share optionValue equals: FileDenyReadWriteOption windowsValue
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsShareRules [

	| aFileOptions |
	aFileOptions := FileSpec read fileOptions.
	aFileOptions parent fileStore: WindowsStore createDefault.
	self assert: aFileOptions shareRule equals: WindowsStore denyNoneShareRule.

	aFileOptions share: FileDenyReadOption new.
	self assert: aFileOptions shareRule equals: WindowsStore denyReadShareRule.

	aFileOptions share: FileDenyWriteOption new.
	self assert: aFileOptions shareRule equals: WindowsStore denyWriteShareRule.

	aFileOptions share: FileDenyReadWriteOption new.
	self assert: aFileOptions shareRule equals: WindowsStore denyReadWriteShareRule.
]

{ #category : 'tests' }
FileOptionsTest >> testWindowsWriteOpenMode [

	| aFileOptions |
	aFileOptions := FileSpec write fileOptions.
	aFileOptions parent fileStore: WindowsStore createDefault.
	self assert: aFileOptions isWritable.
	self deny: aFileOptions isReadable.
	self assert: aFileOptions options size equals: 2.
	self assert: aFileOptions hasFileCreateOption.
	self assert: aFileOptions hasFileTruncateOption.
	self assert: aFileOptions modeString equals: 'w'.
	self assert: aFileOptions fileOpenModeValue equals: 1.
	self assert: aFileOptions fileOpenAndOptionsValue equals: 655361
]
