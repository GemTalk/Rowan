"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressFileSystemRepository',
	#superclass : 'CypressAbstractRepository',
	#instVars : [
		'directoryPath'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'instance creation' }
CypressFileSystemRepository class >> on: aDirectory [

	^self new
		initializeForDirectory: aDirectory;
		yourself.

]

{ #category : 'accessing - properties' }
CypressFileSystemRepository >> codeFormatProperty [

	^properties 
		at: '_gs_format'
		ifAbsent: ['Cypress']

]

{ #category : 'updating properties' }
CypressFileSystemRepository >> codeFormatProperty: aString [

	self validate: aString isOneOf: #('Cypress' 'FileTree' 'Flexible').
	properties 
		at: '_gs_format'
		put: aString

]

{ #category : 'accessing' }
CypressFileSystemRepository >> description [
  | desc |
  desc := super description.
  ^ desc notEmpty
    ifTrue: [ desc ]
    ifFalse: [ self url asString ]

]

{ #category : 'accessing' }
CypressFileSystemRepository >> directoryPath [

	^directoryPath

]

{ #category : 'initializing - private' }
CypressFileSystemRepository >> directoryPath: aString [
  | delim |
  delim := self fileUtils pathNameDelimiter.
  aString last = delim last
    ifTrue: [ directoryPath := aString ]
    ifFalse: [ directoryPath := aString , delim ]

]

{ #category : 'testing - private' }
CypressFileSystemRepository >> doesRepositoryFileExist: fileName [
	"Answer whether the named file exists at the repository level."

	^self fileUtils
		directoryExists: (self fileUtils
				directoryFromPath: fileName
				relativeTo: self directoryPath)

]

{ #category : 'initializing - private' }
CypressFileSystemRepository >> ensureDirectoryPathExists [

	self fileUtils ensureDirectoryExists: self directoryPath

]

{ #category : 'accessing - private' }
CypressFileSystemRepository >> fileUtils [

	^CypressFileUtilities current

]

{ #category : 'initializing - private' }
CypressFileSystemRepository >> fixupMissingCopyrightProperty [

	self copyrightProperty isEmpty ifFalse: [^self].
	self initializeDefaultCopyrightProperty.

]

{ #category : 'initializing - private' }
CypressFileSystemRepository >> initializeCreationOn: aUrl alias: aString [

	self
		initializeUrl: aUrl andAlias: aString;
		alias: aString;
		writePropertiesFile

]

{ #category : 'initializing - private' }
CypressFileSystemRepository >> initializeDefaultRepositoryProperties [

	super initializeDefaultRepositoryProperties.
	self
		codeFormatProperty: 'Cypress';
		strictCodeFormat: false.

]

{ #category : 'initializing - private' }
CypressFileSystemRepository >> initializeForDirectory: aDirectory [

	self initialize.
	self directoryPath: aDirectory.
	self directoryPath isEmpty ifTrue: [^self].	"Not really valid; not a very good idea."
	self ensureDirectoryPathExists.
	self readPropertiesFile.
	self fixupMissingCopyrightProperty.
	self initializeReaderAndWriterClasses.

]

{ #category : 'initializing - private' }
CypressFileSystemRepository >> initializeForFileTreeRepository [
  self initializeDefaultRepositoryProperties.
  self
    codeFormatProperty: 'FileTree';
    strictCodeFormat: true

]

{ #category : 'initializing - private' }
CypressFileSystemRepository >> initializeReaderAndWriterClasses [

	self isCodeFormatCypress
		ifTrue: 
			[self isCodeFormatStrict
				ifTrue: 
					[readerClass := CypressPackageReader.
					writerClass := CypressPackageWriter]
				ifFalse: 
					[readerClass := CypressFlexiblePackageReader.
					writerClass := CypressPackageWriter]]
		ifFalse: 
			[self isCodeFormatStrict
				ifTrue: 
					[readerClass := CypressFileTreeFormatPackageReader.
					writerClass := CypressStrictFileTreeFormatDoNothingPackageWriter]
				ifFalse: 
					[readerClass := CypressFlexiblePackageReader.
					writerClass := CypressFileTreeFormatPackageWriter]]

]

{ #category : 'initializing - private' }
CypressFileSystemRepository >> initializeRepositoryDirectory [
  self directoryPath: self url pathForDirectory.
  self ensureDirectoryPathExists

]

{ #category : 'initializing - private' }
CypressFileSystemRepository >> initializeUrl: aUrl andAlias: aString [
  super initializeUrl: aUrl andAlias: aString.
  self initializeRepositoryDirectory.
  self readPropertiesFile.
  self codeFormatProperty: self url codeFormat.
  self strictCodeFormat: self url isStrict.
  self fixupMissingCopyrightProperty.
  self initializeReaderAndWriterClasses

]

{ #category : 'testing properties' }
CypressFileSystemRepository >> isCodeFormatCypress [

	^self isCodeFormatProperty: 'Cypress'

]

{ #category : 'testing properties' }
CypressFileSystemRepository >> isCodeFormatFileTree [

	^self isCodeFormatProperty: 'FileTree'

]

{ #category : 'testing properties' }
CypressFileSystemRepository >> isCodeFormatFlexiblyCypress [

	^self isCodeFormatStrict not and: [self isCodeFormatCypress]

]

{ #category : 'testing properties' }
CypressFileSystemRepository >> isCodeFormatFlexiblyFileTree [

	^self isCodeFormatStrict not and: [self isCodeFormatFileTree]

]

{ #category : 'testing properties - private' }
CypressFileSystemRepository >> isCodeFormatProperty: aString [

	^(properties at: '_gs_format') equalsNoCase: aString

]

{ #category : 'testing properties' }
CypressFileSystemRepository >> isCodeFormatStrict [

	^(properties 
		at: '_gs_strict'
		ifAbsent: ['']) equalsNoCase: 'true'

]

{ #category : 'testing properties' }
CypressFileSystemRepository >> isCodeFormatStrictlyCypress [

	^self isCodeFormatStrict and: [self isCodeFormatCypress]

]

{ #category : 'testing properties' }
CypressFileSystemRepository >> isCodeFormatStrictlyFileTree [

	^self isCodeFormatStrict and: [self isCodeFormatFileTree]

]

{ #category : 'accessing' }
CypressFileSystemRepository >> packageNames [

	| extension extensionSize |
	extension := self packageExtension.
	extensionSize := extension size.
	^(self fileUtils
		directoryEntriesFrom: self directoryPath , '*' , extension) collect: 
				[:each |
				| localName |
				localName := self fileUtils localNameFrom: each.
				localName copyFrom: 1 to: localName size - extensionSize]

]

{ #category : 'printing' }
CypressFileSystemRepository >> printDetailsOn: aStream [
  self alias notEmpty
    ifTrue: [ aStream
        nextPutAll: self alias;
        nextPutAll: ': ' ].
  aStream nextPutAll: self url printString

]

{ #category : 'accessing' }
CypressFileSystemRepository >> properties [
  ^ properties

]

{ #category : 'reading' }
CypressFileSystemRepository >> readPropertiesFile [

	self readPropertiesFile: (#('properties.ston' 'properties.json' '.filetree')
				detect: [:each | self doesRepositoryFileExist: each]
				ifNone: [^self]).

]

{ #category : 'reading' }
CypressFileSystemRepository >> readPropertiesFile: fileName [
	"Expect 'properties.ston' for Cypress, but permit 'properties.json' in which case we assume
	 the format should be FileTree.

	 Supported properties are:
		_cypress_copyright	- the (optional, default) copyright notice for the whole repository
		_gs_format			- optional, determines which format will be used for writing packages (and reading, but ...)
							- either Cypress or FileTree (case-insensitive)
		_gs_strict			- optional, determines whether the reader strictly enforces the format
							- either true or false (case-insensitive)
		_gs_fileout			- optional, determines whether to also produce a *.gs fileout when writing a package
							- either true or false (case-insensitive)
	"

	fileName = '.filetree' ifTrue: [^self initializeForFileTreeRepository].
	self fileUtils
		readStreamFor: fileName
		in: self directoryPath
		do: [:fileStream | properties := CypressJsonParser parseStream: fileStream]

]

{ #category : 'updating properties' }
CypressFileSystemRepository >> strictCodeFormat: aBoolean [

	self strictCodeFormatProperty: aBoolean printString


]

{ #category : 'updating properties - private' }
CypressFileSystemRepository >> strictCodeFormatProperty: aString [

	self validate: aString isOneOf: #('true' 'false').
	properties 
		at: '_gs_strict'
		put: aString

]

{ #category : 'updating properties - private' }
CypressFileSystemRepository >> validate: aString isOneOf: someStrings [

	someStrings
		detect: [:each | aString equalsNoCase: each]
		ifNone: [self error: aString printString, ' must be one of ', someStrings printString].

]

{ #category : 'validating - private' }
CypressFileSystemRepository >> validateUrl [
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

	self url fileName isEmpty
		ifFalse: [self error: self printString, ' should not be used with URLs for file names (', self url fileName, ' in ', self url pathForDirectory, ')']

]

{ #category : 'writing' }
CypressFileSystemRepository >> writePropertiesFile [

	self writePropertiesFile: (self isCodeFormatFileTree
				ifTrue: ['properties.json']
				ifFalse: ['properties.ston'])

]

{ #category : 'writing' }
CypressFileSystemRepository >> writePropertiesFile: fileName [

	self fileUtils
		writeStreamFor: fileName
		in: self directoryPath
		do: [:fileStream | properties _writeCypressJsonOn: fileStream]

]
