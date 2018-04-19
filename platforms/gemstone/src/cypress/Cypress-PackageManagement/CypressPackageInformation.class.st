"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

CypressPackageInformation documents potential and actual packages for the Cypress Package Manager. 

Candidate package names come from class categories and from method categories beginning with an asterisk. Category names may contain more information than just the package name, such as logical subdivisions within a package or method categorization in addition to the package name. For example, a Package might be named X or Y-Z or whatever. Classes could be categorized as X, Y-Z, X-A, or Y-Z-A, and methods could be categorized as *X, *Y-Z, *X-A, *Y-Z-A, etc. (The various letters X, Y, Z, and A can represent almost any sequence of characters, in either uppercase, lowercase, or both. Package names are case-insensitive.)

There are four types of CypressPackageInformation objects:
 - Known Package - those which are known to represent real packages (e.g., Y-Z). In general, it is because there is a savedLocation specified.
 - Qualified Name - the name is a Known Package name qualified by further details, and cannot be used to represent a Known Package (e.g., X-accessing).
 - Conflicted Name - the name is a prefix of a Known Package name (e.g. given a Known Package named Y-Z, there can be no package named Y).
 - Unknown - the name could represent a package, but it is not known to do so.

Instance Variables
	advice	<String>	Additional information about the type of the instance, usually used only for Qualified Names and Conflcited Names.
	changesCount	<Integer>	The number of differences between the in-image definitions of the package and the definitions previously saved to disk.
	competingPackageNames	<String>*	0 or more strings naming packages in competition with this one.
	imageCounts	<Integer pair>	The number of classes and the number of methods in the image for the package.
	name	<String>	The name of the package or potential package.
	savedLocation	<String>	The path to the directory in which the package was or should be saved, with a trailing slash (e.g., /usr/src/project/).
	type	<String>	One of 'Known Package', 'Qualified Name', 'Conflicted Name', and 'Unknown'.
	imageDefinitions	<CypressDefinition>*	0 or more definitions from the image.
	savedDefinitions	<CypressDefinition>*	0 or more definitions from the savedLocation storage.


"
Class {
	#name : 'CypressPackageInformation',
	#superclass : 'CypressObject',
	#instVars : [
		'name',
		'type',
		'advice',
		'competingPackageNames',
		'imageDefinitions',
		'savedDefinitions',
		'savedLocation',
		'repository',
		'repositoryDescription',
		'imageCounts',
		'changesCount'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'instance creation' }
CypressPackageInformation class >> named: aString repository: aCypressRepository [
	"Answer an instance of the receiver representing the named package.
	 If the package was saved in a Repository, load up the saved details."

	^self new
		initializeFromName: aString andRepository: aCypressRepository;
		yourself

]

{ #category : 'instance creation' }
CypressPackageInformation class >> new [

	^super new
		initialize;
		yourself

]

{ #category : 'updating - type' }
CypressPackageInformation >> beConflictedWith: somePackageNames [
	"Be designated as representing the prefix of one or more Known Package names."

	type := 'Conflicted Name'.
	competingPackageNames := somePackageNames sortAscending.
	advice := 'Conflicts with the packages named ', self competingPackageNamesString

]

{ #category : 'updating - type' }
CypressPackageInformation >> beKnown [
	"Be known to represent a real package."

	type := 'Known Package'.
	advice := ''.
	competingPackageNames := #()

]

{ #category : 'updating - type' }
CypressPackageInformation >> beQualifiedNameOf: somePackageNames [
	"Be designated as qualifying a Known Package name and therefore not eligible as a package name."

	type := 'Qualified Name'.
	competingPackageNames := somePackageNames sortAscending.
	advice := 'Qualifies the package named ', self competingPackageNamesString

]

{ #category : 'updating - type' }
CypressPackageInformation >> beUnknown [
	"Be designated as possibly representing a package, but not known to do so."

	type := 'Unknown'.
	advice := ''.
	competingPackageNames := #()

]

{ #category : 'accessing' }
CypressPackageInformation >> changesCount [

	^changesCount

]

{ #category : 'accessing' }
CypressPackageInformation >> changesCount: anInteger [

	changesCount := anInteger

]

{ #category : 'accessing' }
CypressPackageInformation >> changesStatus [

	^self hasChanges
		ifTrue: [' (' , self changesCount printString , ')']
		ifFalse: ['']

]

{ #category : 'accessing' }
CypressPackageInformation >> classCount [

	^self imageCounts first

]

{ #category : 'accessing' }
CypressPackageInformation >> competingPackageNames [

	^competingPackageNames

]

{ #category : 'accessing' }
CypressPackageInformation >> competingPackageNamesString [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	self competingPackageNames
		do: [:each | stream nextPutAll: each printString]
		separatedBy: [stream nextPutAll: ', '].
	^stream contents

]

{ #category : 'accessing' }
CypressPackageInformation >> description [

	self isKnown ifTrue: [^self savedLocation].
	self isUnknown ifTrue: [^' <unknown>'].
	^' <', advice, '>'

]

{ #category : 'accessing' }
CypressPackageInformation >> determinedChangesCount [

	| notInImage notInSaved |
	notInImage := self savedDefinitions
				reject: [:each | self imageDefinitions includes: each].
	notInSaved := self imageDefinitions
		reject: [:each | self savedDefinitions includes: each].
	^notInImage size + notInSaved size

]

{ #category : 'testing' }
CypressPackageInformation >> hasChanges [

	^self changesCount > 0

]

{ #category : 'accessing' }
CypressPackageInformation >> imageCounts [

	^imageCounts

]

{ #category : 'accessing' }
CypressPackageInformation >> imageCounts: someIntegers [
	"A pair: the number of classes and number of methods"

	imageCounts := someIntegers

]

{ #category : 'accessing' }
CypressPackageInformation >> imageDefinitionCounts [

	| classCount methodCount |
	classCount := methodCount := 0.
	self imageDefinitions do: 
			[:each |
			each classDefinition: [:classDefinition | classCount := classCount + 1]
				methodDefinition: [:methodDefinition | methodCount := methodCount + 1]].
	^Array with: classCount with: methodCount

]

{ #category : 'accessing' }
CypressPackageInformation >> imageDefinitions [

	^imageDefinitions

]

{ #category : 'accessing' }
CypressPackageInformation >> imageDefinitions: someCypressDefinitions [

	imageDefinitions := someCypressDefinitions

]

{ #category : 'accessing' }
CypressPackageInformation >> imageDefinitionsStatus [

	^self classCount printString , '/' , self methodCount printString

]

{ #category : 'initializing' }
CypressPackageInformation >> initialize [

	self
		beUnknown;
		name: '';
		imageDefinitions: #();
		savedDefinitions: #();
		savedLocation: '';
		repositoryDescription: '';
		imageCounts: #(0 0);
		changesCount: 0

]

{ #category : 'initializing' }
CypressPackageInformation >> initializeFromName: aString andRepository: aCypressRepositoryOrNil [

	self name: aString.
	aCypressRepositoryOrNil isNil ifTrue: [^self].
	self updateKnownPackageRepository: aCypressRepositoryOrNil

]

{ #category : 'testing - type' }
CypressPackageInformation >> isConflicted [

	^type = 'Conflicted Name'

]

{ #category : 'testing - type' }
CypressPackageInformation >> isKnown [

	^type = 'Known Package'

]

{ #category : 'testing - type' }
CypressPackageInformation >> isQualifiedName [

	^type = 'Qualified Name'

]

{ #category : 'testing - type' }
CypressPackageInformation >> isUnknown [

	^type = 'Unknown'

]

{ #category : 'accessing' }
CypressPackageInformation >> methodCount [

	^self imageCounts last

]

{ #category : 'accessing' }
CypressPackageInformation >> name [

	^name

]

{ #category : 'accessing' }
CypressPackageInformation >> name: aString [

	name := aString

]

{ #category : 'printing' }
CypressPackageInformation >> printDetailsOn: aStream [

	aStream
		nextPutAll: self name;
		nextPutAll: ' - ';
		nextPutAll: self description

]

{ #category : 'updating' }
CypressPackageInformation >> readDefinitionsFromRepository [

	^(self repository reader readPackageStructureForPackageNamed: self name)
		packageStructure snapshot
		definitions

]

{ #category : 'updating' }
CypressPackageInformation >> refresh [

	self isKnown ifFalse: [^self].
	self
		updateImageDefinitions;
		updateSavedDefinitions;
		updateChangesCount.

]

{ #category : 'accessing' }
CypressPackageInformation >> repository [

	^repository

]

{ #category : 'accessing' }
CypressPackageInformation >> repository: aCypressFileSystemRepository [

	repository := aCypressFileSystemRepository

]

{ #category : 'unknown' }
CypressPackageInformation >> repositoryDescription [

	^repositoryDescription

]

{ #category : 'unknown' }
CypressPackageInformation >> repositoryDescription: aString [

	repositoryDescription := aString

]

{ #category : 'accessing' }
CypressPackageInformation >> savedDefinitions [

	^savedDefinitions

]

{ #category : 'accessing' }
CypressPackageInformation >> savedDefinitions: someCypressDefinitions [

	savedDefinitions := someCypressDefinitions

]

{ #category : 'accessing' }
CypressPackageInformation >> savedLocation [

	^savedLocation

]

{ #category : 'accessing' }
CypressPackageInformation >> savedLocation: aDirectory [

	savedLocation := aDirectory

]

{ #category : 'accessing' }
CypressPackageInformation >> status [

	| changes |
	(changes := self changesStatus) isEmpty ifTrue: [^self imageDefinitionsStatus].
	^self imageDefinitionsStatus, changes

]

{ #category : 'updating' }
CypressPackageInformation >> updateChangesCount [
	"Must be applied after the image definitions and saved definitions are updated."

	self changesCount: self determinedChangesCount

]

{ #category : 'updating' }
CypressPackageInformation >> updateImageDefinitions [

	self
		imageDefinitions: (CypressPackageDefinition named: self name) snapshot
					definitions;
		imageCounts: self imageDefinitionCounts

]

{ #category : 'updating' }
CypressPackageInformation >> updateKnownPackageRepository: aCypressRepository [
	"Update the receiver to reflect it being a known package."

	self
		beKnown;
		updateRepository: aCypressRepository;
		refresh.

]

{ #category : 'updating' }
CypressPackageInformation >> updateRepository: aCypressRepository [

	self
		repository: aCypressRepository;
		repositoryDescription: self repository description;
		savedLocation: self repository directoryPath

]

{ #category : 'updating' }
CypressPackageInformation >> updateSavedDefinitions [

	self savedDefinitions: self readDefinitionsFromRepository

]
