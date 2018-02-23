"
No class-specific documentation for CypFileSystemRepository, hierarchy is: 
Object
  CypAbstractRepository( url properties readerClass writerClass)
    CypFileSystemRepository( directoryPath)

"
Class {
	#name : 'CypFileSystemRepository',
	#superclass : 'CypRepository',
	#instVars : [
		'directoryPath',
		'disk'
	],
	#category : 'Cypress-Core'
}

{ #category : 'accessing' }
CypFileSystemRepository >> directoryPath [

	^directoryPath
]

{ #category : 'initializing - private' }
CypFileSystemRepository >> directoryPath: aString [

	| delim |
	delim := disk pathNameDelimiter.
	aString last = delim last
		ifTrue: [directoryPath := aString]
		ifFalse: [directoryPath := aString , delim]
]

{ #category : 'testing - private' }
CypFileSystemRepository >> doesRepositoryFileExist: fileName [
	"Answer whether the named file exists at the repository level."

	^disk directoryExists: (disk directoryFromPath: fileName
				relativeTo: self directoryPath)
]

{ #category : 'initializing - private' }
CypFileSystemRepository >> ensureDirectoryPathExists [

	disk ensureDirectoryExists: self directoryPath
]

{ #category : 'initializing - private' }
CypFileSystemRepository >> initializeRepositoryDirectory [

	self directoryPath: url pathForDirectory.
	self ensureDirectoryPathExists
]

{ #category : 'initializing' }
CypFileSystemRepository >> initializeUrl: anUrl [

	super initializeUrl: anUrl.
	disk := Rowan fileUtilities.
	self initializeRepositoryDirectory.
	self readPropertiesFile
]

{ #category : 'private' }
CypFileSystemRepository >> packageExtension [
	"The extension that labels a package directory."

	self subclassResponsibility
]

{ #category : 'accessing' }
CypFileSystemRepository >> packageNames [

	^(disk
		directoryEntriesFrom: self directoryPath , '*' , self packageExtension)
			collect: [:each | (disk localNameFrom: each) copyWithoutSuffix: self packageExtension]
]

{ #category : 'accessing' }
CypFileSystemRepository >> propertiesFileName [

	self subclassResponsibility
]

{ #category : 'accessing' }
CypFileSystemRepository >> propertiesParserClass [

	self subclassResponsibility
]

{ #category : 'reading' }
CypFileSystemRepository >> reactToAbsentPropertiesFile [
	"Default right now is to error if the properties file is absent. Subclasses can override this.
	This might be something to make configurable via Cypress configuration."

	^self error: 'Properties file not found.'
]

{ #category : 'reading' }
CypFileSystemRepository >> readDefinitionOfPackageNamed: packageName [

	| fullPath reader |
	fullPath := self fullPathForPackageNamed: packageName.
	reader := self newPackageReader.
	^reader readPackageDefinitionFromDirectory: fullPath
]

{ #category : 'reading' }
CypFileSystemRepository >> readPropertiesFile [

	| filename |
	filename := self propertiesFileName.
	(self doesRepositoryFileExist: filename)
		ifTrue: [self readPropertiesFile: filename]
		ifFalse: [self reactToAbsentPropertiesFile]
]

{ #category : 'reading' }
CypFileSystemRepository >> readPropertiesFile: fileName [

	disk
		readStreamFor: fileName
		in: self directoryPath
		do: [:fileStream | properties := self propertiesParserClass parseStream: fileStream]
]
