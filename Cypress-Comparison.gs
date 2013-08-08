! Package: Cypress-Comparison


! Remove existing behavior from package Cypress-Comparison
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Cypress-Comparison'.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
										or: [each first ~= $*]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
%


! Class Declarations

doit
(Object
	subclass: 'CypressPackageManager'
	instVarNames: #( knownPackages knownRepositories packageInformationList )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Comparison';
		comment: '';
		immediateInvariant.
%

doit
(Object
	subclass: 'CypressPackageComparator'
	instVarNames: #( directoryPackageMap diskTimestamps diskSnapshots imageSnapshots snapshotDifferences currentPackageName currentAdditions currentRemovals )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Comparison';
		comment: '';
		immediateInvariant.
%

doit
(CypressObject
	subclass: 'CypressPackageInformation'
	instVarNames: #( name type advice competingPackageNames imageDefinitions savedDefinitions savedLocation repository repositoryDescription imageCounts changesCount )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-Comparison';
		comment: 'CypressPackageInformation documents potential and actual packages for the Cypress Package Manager. 

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
	type	<String>	One of ''Known Package'', ''Qualified Name'', ''Conflicted Name'', and ''Unknown''.
	imageDefinitions	<CypressDefinition>*	0 or more definitions from the image.
	savedDefinitions	<CypressDefinition>*	0 or more definitions from the savedLocation storage.

';
		immediateInvariant.
%

! Class Implementation for CypressPackageManager

! ------------------- Class methods for CypressPackageManager

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageManager
new

	^super new
		initialize;
		yourself
%

category: 'accessing'
set compile_env: 0
classmethod: CypressPackageManager
packageNamePermutationsFor: aString
	"Answer the variations on possible package names from the specified string.
	 Each hyphen may possibly separate the package name from a suffix."

	| names |
	names := OrderedCollection new.
	aString doWithIndex: 
			[:each :index |
			(each = $- and: [index > 1])
				ifTrue: [names add: (aString copyFrom: 1 to: index - 1)]].
	aString last ~= $- ifTrue: [names add: aString].
	^names
%

category: 'accessing'
set compile_env: 0
classmethod: CypressPackageManager
potentialPackageNames
	"Answer a list of 'package names' from classes and methods.
	 The class category is the package name, if the class is in a package at all.
	 The method category begins with an asterisk (*) before the package name,
	 but can be continued with other details (e.g., *PackageName-accessing).
	 This version does NOT recognize method category suffixes."

	| classCategories methodCategories |
	classCategories := Set new.
	methodCategories := Set new.
	System myUserProfile symbolList allSatisfying: 
			[:aClass |
			aClass isBehavior and: 
					[classCategories addAll: (self packageNamePermutationsFor: aClass category).
					aClass categorysDo: 
							[:cat :method |
							cat first = $*
								ifTrue: [methodCategories addAll: (self packageNamePermutationsFor: (cat copyFrom: 2 to: cat size))]].
					false]].
	^(Set new)
		addAll: classCategories;
		addAll: methodCategories;
		remove: 'User Classes';
		remove: 'Kernel';
		sortAscending
%

! ------------------- Instance methods for CypressPackageManager

category: 'comparing'
set compile_env: 0
method: CypressPackageManager
compareDefinitionsFromConflictedPackageInformation: aCypressPackageInformation

	| badDefinitions expectedDefinitions |
	badDefinitions := (CypressPackageStructure
				fromPackage: (CypressPackageDefinition
						named: aCypressPackageInformation name))
					snapshot definitions
				asSet.
	expectedDefinitions := OrderedCollection new.
	aCypressPackageInformation competingPackageNames do: 
			[:each |
			expectedDefinitions
				addAll: (CypressPackageStructure
						fromPackage: (CypressPackageDefinition named: each)) snapshot
						definitions].
	"Coerce the expected definitions into using the conflicted package name."
	expectedDefinitions do: 
			[:each |
			each classDefinition: [:cd | cd category: aCypressPackageInformation name]
				methodDefinition: [:md | ]].
	expectedDefinitions do: [:each | badDefinitions remove: each ifAbsent: []].
	^badDefinitions sortAscending
%

category: 'comparing'
set compile_env: 0
method: CypressPackageManager
comparePackageFrom: aCypressPackageInformation

	^self comparePackagesFrom: (Array with: aCypressPackageInformation)
%

category: 'comparing'
set compile_env: 0
method: CypressPackageManager
comparePackagesFrom: someCypressPackageInformations

	^(someCypressPackageInformations
		inject: CypressPackageComparator new
		into: 
			[:comparer :each |
			comparer comparingPackageNamed: each name fromDirectory: each savedLocation])
				getDifferences
%

category: 'updating - private'
set compile_env: 0
method: CypressPackageManager
determineKnownPackages

	^(packageInformationList select: [:each | each repository notNil])
		inject: Dictionary new
		into: 
			[:dict :each |
			dict
				at: each name put: each savedLocation;
				yourself]
%

category: 'initializing - private'
set compile_env: 0
method: CypressPackageManager
initialize

	self refreshPackageInformation.
%

category: 'initializing - private'
set compile_env: 0
method: CypressPackageManager
initializeConflictingPackageNames

	| conflictingPackages |
	conflictingPackages := Dictionary new.
	packageInformationList do: 
			[:each |
			conflictingPackages
				at: each
				put: (knownPackages keys
						select: [:knownName | knownName ~= each name and: [knownName beginsWith: each name , '-']])].
	conflictingPackages := conflictingPackages reject: [:each | each isEmpty].
	conflictingPackages
		keysAndValuesDo: [:package :conflicts | package beConflictedWith: conflicts]
%

category: 'initializing - private'
set compile_env: 0
method: CypressPackageManager
initializeKnownPackages

	knownPackages := (System myUserProfile objectNamed: #KnownCypressPackages)
				ifNil: [Dictionary new]
%

category: 'initializing - private'
set compile_env: 0
method: CypressPackageManager
initializeKnownRepositories

	knownRepositories := Dictionary new.
	knownPackages asSet
		do: [:each | self repositoryOn: each]
%

category: 'initializing - private'
set compile_env: 0
method: CypressPackageManager
initializePackageInformationList

	| allInterestingNames |
	allInterestingNames := Set new
		addAll: self potentialPackageNames;
		addAll: knownPackages keys;
		sortAscending.
	packageInformationList := allInterestingNames collect: 
					[:each |
					| directory repo |
					directory := knownPackages at: each ifAbsent: [nil].
					repo := directory ifNotNil: [self repositoryOn: directory].
					CypressPackageInformation named: each repository: repo]
%

category: 'initializing - private'
set compile_env: 0
method: CypressPackageManager
initializeQualifiedPackageNames

	| qualifiedPackages |
	qualifiedPackages := Dictionary new.
	packageInformationList do: 
			[:each |
			qualifiedPackages
				at: each
				put: (knownPackages keys
						select: [:knownName | knownName ~= each name and: [each name beginsWith: knownName , '-']])].
	qualifiedPackages := qualifiedPackages reject: [:each | each isEmpty].
	qualifiedPackages
		keysAndValuesDo: [:package :baseNames | package beQualifiedNameOf: baseNames]
%

category: 'updating'
set compile_env: 0
method: CypressPackageManager
loadPackageFrom: aCypressPackageInformation

	| summary loader |
	loader := (CypressSnapshot definitions: aCypressPackageInformation savedDefinitions)
				updatePackage: (CypressPackageDefinition named: aCypressPackageInformation name).
	summary := Dictionary new.

	loader unloadable notEmpty ifTrue: [summary at: 'Unloadable count' put: loader unloadable size].
	loader errors notEmpty ifTrue: [summary at: 'Errors count' put: loader errors size].
	loader requirements notEmpty ifTrue: [summary at: 'Missing Requirements count' put: loader requirements size].

	^summary
%

category: 'updating'
set compile_env: 0
method: CypressPackageManager
lookForLoadedPackagesIn: aDirectory
	"Update any of the packages in the image which have a Cypress file out in
	 the specified directory to reflect the path where the package has theoretically
	 been saved."

	self lookForLoadedPackagesInRepository: (self repositoryOn: aDirectory).
	^nil
%

category: 'updating'
set compile_env: 0
method: CypressPackageManager
lookForLoadedPackagesInRepository: aCypressRepository
	"Update any of the packages in the image which have a Cypress file out in
	 the specified directory to reflect the path where the package has theoretically
	 been saved."

	| packageNames |
	packageNames := aCypressRepository packageNames.
	(self packageInformationList
		select: [:each | packageNames includes: each name])
			do: [:each | each updateKnownPackageRepository: aCypressRepository].
	self saveKnownPackages.
	^nil
%

category: 'updating'
set compile_env: 0
method: CypressPackageManager
lookForUnloadedPackagesIn: aDirectory
	"Load any package names from aDirectory as known packages.
	 This does not load the package contents."

	self lookForUnloadedPackagesInRepository: (self repositoryOn: aDirectory).
	^nil
%

category: 'updating'
set compile_env: 0
method: CypressPackageManager
lookForUnloadedPackagesInRepository: aCypressRepository
	"Add known packages for any Cypress file outs in the specified directory."

	| packageNames existingPackageNames |
	packageNames := aCypressRepository packageNames.
	(self packageInformationList
		select: [:each | packageNames includes: each name])
			do: [:each | each updateKnownPackageRepository: aCypressRepository].
	existingPackageNames := self packageInformationList
				collect: [:each | each name].
	(packageNames reject: [:each | existingPackageNames includes: each])
		do: 
			[:each |
			self packageInformationList
				add: (CypressPackageInformation named: each repository: aCypressRepository)].
	self saveKnownPackages.
	^nil
%

category: 'accessing'
set compile_env: 0
method: CypressPackageManager
packageInformationList

	^packageInformationList
%

category: 'accessing'
set compile_env: 0
method: CypressPackageManager
potentialPackageNames

	^self class potentialPackageNames
%

category: 'accessing'
set compile_env: 0
method: CypressPackageManager
refreshedPackageInformationList

	self refreshPackageInformation.
	^self packageInformationList.
%

category: 'updating'
set compile_env: 0
method: CypressPackageManager
refreshPackageInformation

	self
		initializeKnownPackages;
		initializeKnownRepositories;
		initializePackageInformationList;
		initializeConflictingPackageNames;
		initializeQualifiedPackageNames
%

category: 'initializing - private'
set compile_env: 0
method: CypressPackageManager
repositoryOn: aDirectory

	^knownRepositories
		at: aDirectory
		ifAbsentPut: [CypressFileSystemRepository on: aDirectory].
%

category: 'updating - private'
set compile_env: 0
method: CypressPackageManager
saveKnownPackages

	self updateKnownPackages.
	((System myUserProfile resolveSymbol: #KnownCypressPackages)
		ifNil: 
			[(System myUserProfile objectNamed: #UserGlobals)
				addAssociation: #KnownCypressPackages -> Dictionary new])
			value: knownPackages
%

category: 'updating - private'
set compile_env: 0
method: CypressPackageManager
updateKnownPackages

	knownPackages := self determineKnownPackages
%

category: 'updating'
set compile_env: 0
method: CypressPackageManager
updateSavedLocation: aDirectory for: aCypressPackageInformation
	"Update the specified package to reflect the path and repository where the
	 package should be saved."

	aCypressPackageInformation
		updateKnownPackageRepository: (self repositoryOn: aDirectory).
	self saveKnownPackages.
	^nil
%

category: 'writing - private'
set compile_env: 0
method: CypressPackageManager
writeCypressPackageToDiskFrom: aCypressPackageInformation

	| packageStructure |
	packageStructure := CypressPackageStructure
				fromPackage: (CypressPackageDefinition
						named: aCypressPackageInformation name).
	aCypressPackageInformation repository writer
		writePackageStructure: packageStructure
%

category: 'writing - private'
set compile_env: 0
method: CypressPackageManager
writeGemStoneFileoutForPackageStructure: packageStructure andPackageInformation: aCypressPackageInformation

	| filePath |
	filePath := aCypressPackageInformation savedLocation, aCypressPackageInformation name , '.gs'.
	aCypressPackageInformation repository areGemStoneFileoutsEnabled
		ifTrue: 
			[(GsFile openWriteOnServer: filePath)
				nextPutAll: (String streamContents: [:stream | packageStructure fileOutOn: stream]);
				close]
		ifFalse: [GsFile removeServerFile: filePath]
%

category: 'writing'
set compile_env: 0
method: CypressPackageManager
writePackagesToDiskFrom: someCypressPackageInformations

	| packageStructure |
	^someCypressPackageInformations do: 
			[:each |
			packageStructure := CypressPackageStructure
						fromPackage: (CypressPackageDefinition named: each name).
			each repository writer writePackageStructure: packageStructure.
			self writeGemStoneFileoutForPackageStructure: packageStructure
				andPackageInformation: each.
			each refresh.
			self saveKnownPackages]
%

category: 'writing - private'
set compile_env: 0
method: CypressPackageManager
writePackageStructure: packageStructure to: aCypressRepository

	aCypressRepository writer writePackageStructure: packageStructure
%

category: 'writing'
set compile_env: 0
method: CypressPackageManager
writePackageToDiskFrom: aCypressPackageInformation

	^self writePackagesToDiskFrom: (Array with: aCypressPackageInformation)
%

! Class Implementation for CypressPackageComparator

! ------------------- Class methods for CypressPackageComparator

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageComparator
comparingPackageNamed: packageName fromDirectory: aDirectory

	^(self new)
		comparingPackageNamed: packageName fromDirectory: aDirectory;
		yourself
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageComparator
forCypress

	^(self new)
		comparingPackages: #('Cypress-Definitions' 'Cypress-Mocks' 'Cypress-Structure' 'Cypress-Tests' 'Cypress-GemStoneFileServer' 'Cypress-Comparison')
			fromDirectory: '/opt/git/CypressReferenceImplementation/';
		yourself
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageComparator
new

	^super new
		initialize;
		yourself
%

! ------------------- Instance methods for CypressPackageComparator

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
add: aDefinition to: aDictionary

	aDefinition
		classDefinition: [:classDefinition | self addClassDefinition: classDefinition to: aDictionary]
		methodDefinition: [:methodDefinition | self addMethodDefinition: methodDefinition to: aDictionary]
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
addClassDefinition: classDefinition to: aDictionary

	(aDictionary at: classDefinition className ifAbsentPut: [Dictionary new])
		at: 'class category' put: classDefinition category;
		at: 'class comment' put: classDefinition comment;
		at: 'class definition' put: classDefinition classDefinitionString.
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
addMethodDefinition: methodDefinition to: aDictionary

	((aDictionary at: methodDefinition className ifAbsentPut: [Dictionary new])
		at: (methodDefinition classIsMeta
				ifTrue: ['class methods']
				ifFalse: ['instance methods'])
		ifAbsentPut: [Dictionary new]) at: methodDefinition selector
			put: methodDefinition category -> methodDefinition source
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
applyAddition: aCypressAddition

	self add: aCypressAddition definition to: self currentAdditions
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
applyModification: aCypressModification

	self
		add: aCypressModification modification to: self currentAdditions;
		add: aCypressModification obsoletion to: self currentRemovals.
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
applyRemoval: aCypressRemoval

	self add: aCypressRemoval definition to: self currentRemovals.
%

category: 'comparing'
set compile_env: 0
method: CypressPackageComparator
compare

	diskSnapshots keys do: [:packageName |
		self resetCurrentForPackage: packageName.
		self currentPatchOperations do: [:each | each applyTo: self].
	].
	self resetCurrentForPackage: nil.
%

category: 'initializing'
set compile_env: 0
method: CypressPackageComparator
comparingPackageNamed: packageName fromDirectory: aDirectory

	self comparingPackages: (Array with: packageName) fromDirectory: aDirectory
%

category: 'initializing'
set compile_env: 0
method: CypressPackageComparator
comparingPackages: someNames fromDirectory: aDirectory

	(directoryPackageMap at: aDirectory ifAbsentPut: [OrderedCollection new])
		addAll: someNames.
	someNames do: 
			[:packageName |
			| reader modTime modTimestamp |
			reader := (CypressFileSystemRepository on: aDirectory) reader
						readPackageStructureForPackageNamed: packageName.
			diskSnapshots at: packageName put: reader packageStructure snapshot.
			modTime := System
						performOnServer: 'stat --printf=%y ' , reader packageDirectory.
			modTimestamp := (modTime beginsWith: 'stat:')
						ifTrue: [nil]
						ifFalse: [DateAndTime fromUnixFormatString: modTime].
			diskTimestamps at: packageName put: modTimestamp.
			imageSnapshots at: packageName
				put: (CypressPackageDefinition named: packageName) snapshot]
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
currentAdditions

	currentAdditions ifNil: [self updateCurrentAdditionsAndRemovals].
	^currentAdditions
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
currentDiskSnapshot

	^diskSnapshots at: currentPackageName
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
currentImageSnapshot

	^imageSnapshots at: currentPackageName
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
currentPatchOperations

	^(CypressPatch fromBase: self currentDiskSnapshot toTarget: self currentImageSnapshot) operations.
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
currentRemovals

	currentRemovals ifNil: [self updateCurrentAdditionsAndRemovals].
	^currentRemovals
%

category: 'comparing'
set compile_env: 0
method: CypressPackageComparator
getDifferences

	self compare.
	^self snapshotDifferences
%

category: 'initializing - private'
set compile_env: 0
method: CypressPackageComparator
initialize

	directoryPackageMap := Dictionary new.
	diskTimestamps := Dictionary new.
	diskSnapshots := Dictionary new.
	imageSnapshots := Dictionary new.
	snapshotDifferences := Dictionary new
		at: 'newer' put: (Dictionary with: 'Finished at' -> DateAndTime now);
		at: 'older' put: (Dictionary with: 'Finished at' -> DateAndTime now);
		yourself.
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
resetCurrentForPackage: aStringOrNil

	currentPackageName := aStringOrNil.
	currentAdditions := nil.
	currentRemovals := nil.
%

category: 'accessing'
set compile_env: 0
method: CypressPackageComparator
snapshotDifferences

	^snapshotDifferences
%

category: 'comparing - private'
set compile_env: 0
method: CypressPackageComparator
updateCurrentAdditionsAndRemovals

	| oldTimestamp |
	currentAdditions := (snapshotDifferences at: 'newer')
				at: currentPackageName
				ifAbsentPut: [Dictionary new].
	oldTimestamp := (diskTimestamps at: currentPackageName) ifNil: [^self].
	currentRemovals := (snapshotDifferences at: 'older')
				at: currentPackageName
				ifAbsentPut: [Dictionary with: 'Timestamp' -> oldTimestamp printString].
%

! Class Implementation for CypressPackageInformation

! ------------------- Class methods for CypressPackageInformation

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageInformation
named: aString repository: aCypressRepository
	"Answer an instance of the receiver representing the named package.
	 If the package was saved in a Repository, load up the saved details."

	^self new
		initializeFromName: aString andRepository: aCypressRepository;
		yourself
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressPackageInformation
new

	^super new
		initialize;
		yourself
%

! ------------------- Instance methods for CypressPackageInformation

category: 'updating - type'
set compile_env: 0
method: CypressPackageInformation
beConflictedWith: somePackageNames
	"Be designated as representing the prefix of one or more Known Package names."

	type := 'Conflicted Name'.
	competingPackageNames := somePackageNames sortAscending.
	advice := 'Conflicts with the packages named ', self competingPackageNamesString
%

category: 'updating - type'
set compile_env: 0
method: CypressPackageInformation
beKnown
	"Be known to represent a real package."

	type := 'Known Package'.
	advice := ''.
	competingPackageNames := #()
%

category: 'updating - type'
set compile_env: 0
method: CypressPackageInformation
beQualifiedNameOf: somePackageNames
	"Be designated as qualifying a Known Package name and therefore not eligible as a package name."

	type := 'Qualified Name'.
	competingPackageNames := somePackageNames sortAscending.
	advice := 'Qualifies the package named ', self competingPackageNamesString
%

category: 'updating - type'
set compile_env: 0
method: CypressPackageInformation
beUnknown
	"Be designated as possibly representing a package, but not known to do so."

	type := 'Unknown'.
	advice := ''.
	competingPackageNames := #()
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
changesCount

	^changesCount
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
changesCount: anInteger

	changesCount := anInteger
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
changesStatus

	^self hasChanges
		ifTrue: [' (' , self changesCount printString , ')']
		ifFalse: ['']
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
classCount

	^self imageCounts first
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
competingPackageNames

	^competingPackageNames
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
competingPackageNamesString

	^String streamContents: 
			[:stream |
			self competingPackageNames
				do: [:each | stream nextPutAll: each printString]
				separatedBy: [stream nextPutAll: ', ']]
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
description

	self isKnown ifTrue: [^self savedLocation].
	self isUnknown ifTrue: [^' <unknown>'].
	^' <', advice, '>'
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
determinedChangesCount

	| notInImage notInSaved |
	notInImage := self savedDefinitions
				reject: [:each | self imageDefinitions includes: each].
	notInSaved := self imageDefinitions
		reject: [:each | self savedDefinitions includes: each].
	^notInImage size + notInSaved size
%

category: 'testing'
set compile_env: 0
method: CypressPackageInformation
hasChanges

	^self changesCount > 0
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
imageCounts

	^imageCounts
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
imageCounts: someIntegers
	"A pair: the number of classes and number of methods"

	imageCounts := someIntegers
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
imageDefinitionCounts

	| classCount methodCount |
	classCount := methodCount := 0.
	self imageDefinitions do: 
			[:each |
			each classDefinition: [:classDefinition | classCount := classCount + 1]
				methodDefinition: [:methodDefinition | methodCount := methodCount + 1]].
	^Array with: classCount with: methodCount
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
imageDefinitions

	^imageDefinitions
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
imageDefinitions: someCypressDefinitions

	imageDefinitions := someCypressDefinitions
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
imageDefinitionsStatus

	^self classCount printString , '/' , self methodCount printString
%

category: 'initializing'
set compile_env: 0
method: CypressPackageInformation
initialize

	self
		beUnknown;
		name: '';
		imageDefinitions: #();
		savedDefinitions: #();
		savedLocation: '';
		repositoryDescription: '';
		imageCounts: #(0 0);
		changesCount: 0
%

category: 'initializing'
set compile_env: 0
method: CypressPackageInformation
initializeFromName: aString andRepository: aCypressRepositoryOrNil

	self name: aString.
	aCypressRepositoryOrNil isNil ifTrue: [^self].
	self updateKnownPackageRepository: aCypressRepositoryOrNil
%

category: 'testing - type'
set compile_env: 0
method: CypressPackageInformation
isConflicted

	^type = 'Conflicted Name'
%

category: 'testing - type'
set compile_env: 0
method: CypressPackageInformation
isKnown

	^type = 'Known Package'
%

category: 'testing - type'
set compile_env: 0
method: CypressPackageInformation
isQualifiedName

	^type = 'Qualified Name'
%

category: 'testing - type'
set compile_env: 0
method: CypressPackageInformation
isUnknown

	^type = 'Unknown'
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
methodCount

	^self imageCounts last
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
name

	^name
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
name: aString

	name := aString
%

category: 'printing'
set compile_env: 0
method: CypressPackageInformation
printDetailsOn: aStream

	aStream
		nextPutAll: self name;
		nextPutAll: ' - ';
		nextPutAll: self description
%

category: 'updating'
set compile_env: 0
method: CypressPackageInformation
readDefinitionsFromRepository

	^(self repository reader readPackageStructureForPackageNamed: self name)
		packageStructure snapshot
		definitions
%

category: 'updating'
set compile_env: 0
method: CypressPackageInformation
refresh

	self isKnown ifFalse: [^self].
	self
		updateImageDefinitions;
		updateSavedDefinitions;
		updateChangesCount.
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
repository

	^repository
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
repository: aCypressFileSystemRepository

	repository := aCypressFileSystemRepository
%

category: 'unknown'
set compile_env: 0
method: CypressPackageInformation
repositoryDescription

	^repositoryDescription
%

category: 'unknown'
set compile_env: 0
method: CypressPackageInformation
repositoryDescription: aString

	repositoryDescription := aString
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
savedDefinitions

	^savedDefinitions
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
savedDefinitions: someCypressDefinitions

	savedDefinitions := someCypressDefinitions
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
savedLocation

	^savedLocation
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
savedLocation: aDirectory

	savedLocation := aDirectory
%

category: 'accessing'
set compile_env: 0
method: CypressPackageInformation
status

	| changes |
	(changes := self changesStatus) isEmpty ifTrue: [^self imageDefinitionsStatus].
	^self imageDefinitionsStatus, changes
%

category: 'updating'
set compile_env: 0
method: CypressPackageInformation
updateChangesCount
	"Must be applied after the image definitions and saved definitions are updated."

	self changesCount: self determinedChangesCount
%

category: 'updating'
set compile_env: 0
method: CypressPackageInformation
updateImageDefinitions

	self
		imageDefinitions: (CypressPackageDefinition named: self name) snapshot
					definitions;
		imageCounts: self imageDefinitionCounts
%

category: 'updating'
set compile_env: 0
method: CypressPackageInformation
updateKnownPackageRepository: aCypressRepository
	"Update the receiver to reflect it being a known package."

	self
		beKnown;
		updateRepository: aCypressRepository;
		refresh.
%

category: 'updating'
set compile_env: 0
method: CypressPackageInformation
updateRepository: aCypressRepository

	self
		repository: aCypressRepository;
		repositoryDescription: self repository description;
		savedLocation: self repository directoryPath
%

category: 'updating'
set compile_env: 0
method: CypressPackageInformation
updateSavedDefinitions

	self savedDefinitions: self readDefinitionsFromRepository
%

! Class Extensions

! Class Extension for CypressClassDefinition

! ------------------- Instance methods for CypressClassDefinition

category: '*Cypress-Comparison-accessing'
set compile_env: 0
method: CypressClassDefinition
category: aString

	category := aString
%

category: '*Cypress-Comparison-accessing'
set compile_env: 0
method: CypressClassDefinition
classDefinitionString

	^superclassName, ' subclass: ', self name printString, '
		instVarNames: #(', self instanceVariablesString, ')
		classVars: #(', self classVariablesString, ')
		classInstVars: #(', self classInstanceVariablesString, ')
		poolDictionaries: #(', self poolDictionariesString, ')
		inDictionary: UserGlobals
		options: #()'
%

! Class initializers 

doit
%



! End of Package: Cypress-Comparison


