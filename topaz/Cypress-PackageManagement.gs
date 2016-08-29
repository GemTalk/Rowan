! Package: Cypress-PackageManagement


! Remove existing behavior from package Cypress-PackageManagement
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'Cypress-PackageManagement'.
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
	subclass: 'CypressVersionReference'
	instVarNames: #( name package author branch versionNumber )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'A CypressVersionReference refers to a specific version of a Monticello package.';
		immediateInvariant.
%

doit
(Object
	subclass: 'CypressPackageManager'
	instVarNames: #( knownPackages knownRepositories packageInformationList )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(Object
	subclass: 'CypressAbstractRepository'
	instVarNames: #( url properties readerClass writerClass )
	classVars: #( DefaultCopyrightNotice )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractRepository
	subclass: 'CypressFileSystemRepository'
	instVarNames: #( directoryPath )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractRepository
	subclass: 'CypressAbstractFileoutRepository'
	instVarNames: #( directoryPath )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractFileoutRepository
	subclass: 'CypressTopazRepository'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'This is a "write-only" repository.
It could be made readable, to be able to file-in Topaz scripts, but it''s not the same thing.
';
		immediateInvariant.
%

doit
(CypressAbstractFileoutRepository
	subclass: 'CypressSmalltalkRepository'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: 'This is a "write-only" repository.
It could be made readable, to be able to file-in Smalltalk scripts, but it''s not the same thing.
';
		immediateInvariant.
%

doit
(Object
	subclass: 'CypressPackageManager2'
	instVarNames: #( knownRepositories packageInformationList )
	classVars: #( SavedPackageManagers )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(FileUrl
	subclass: 'CypressAbstractFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressLaxFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressFileTreeFormatFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressFileTreeReadOnlyFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressTopazUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: UserGlobals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractFileUrl
	subclass: 'CypressSmalltalkUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
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
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
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

doit
(CypressObject
	subclass: 'CypressAbstractPackageInformation'
	instVarNames: #( name )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractPackageInformation
	subclass: 'CypressEclipsedPackageInformation'
	instVarNames: #( eclipsedBy )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractPackageInformation
	subclass: 'CypressUnknownPackageInformation'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractPackageInformation
	subclass: 'CypressKnownPackageInformation'
	instVarNames: #( repositories digests )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

doit
(CypressAbstractPackageInformation
	subclass: 'CypressConflictingPackageInformation'
	instVarNames: #( conflictsWith )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-PackageManagement';
		comment: '';
		immediateInvariant.
%

! Class Implementation for CypressVersionReference

! ------------------- Class methods for CypressVersionReference

category: 'instance creation'
set compile_env: 0
classmethod: CypressVersionReference
name: aString

	^(self basicNew)
		initializeName: aString;
		yourself
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressVersionReference
new

	self error: 'Use #name: to initialize the receiver.'
%

! ------------------- Instance methods for CypressVersionReference

category: 'comparing'
set compile_env: 0
method: CypressVersionReference
= aReference

	^self class = aReference class
		and: [self name = aReference name]
%

category: 'accessing'
set compile_env: 0
method: CypressVersionReference
author
	"Answer the author of the receiver."
	
	^ author
%

category: 'accessing'
set compile_env: 0
method: CypressVersionReference
branch
	"Answer the branch of the receiver."
	
	^ branch
%

category: 'comparing'
set compile_env: 0
method: CypressVersionReference
hash

	^self name hash
%

category: 'initialization'
set compile_env: 0
method: CypressVersionReference
initializeName: aString

	name := aString.
	self parseName: aString
%

category: 'private'
set compile_env: 0
method: CypressVersionReference
matches: aResolvedReference
	^ self name = aResolvedReference name
%

category: 'accessing'
set compile_env: 0
method: CypressVersionReference
name
	"Answer the name of this reference."
	
	^ name
%

category: 'accessing'
set compile_env: 0
method: CypressVersionReference
packageName
	"Answer the package of the receiver."

	^ package
%

category: 'initialization'
set compile_env: 0
method: CypressVersionReference
parseName: aString
	| basicName |
	basicName := (aString isEmpty or: [aString last isDigit or: [(aString includes: $() not]])
		ifTrue: [ aString ]
		ifFalse: [ (aString copyUpToLast: $.) copyUpTo: $( ].
	package := basicName copyUpToLast: $-.
	(package includes: $.)
		ifFalse: [ branch := '' ]
		ifTrue: [
			branch := package copyAfter: $..
			package := package copyUpTo: $. ].
	author := (basicName copyAfterLast: $-) copyUpToLast: $..
	versionNumber := (basicName copyAfterLast: $-) copyAfterLast: $..
	(versionNumber notEmpty and: [ versionNumber allSatisfy: [ :each | each isDigit ] ])
		ifTrue: [ versionNumber := versionNumber asInteger ]
		ifFalse: [ versionNumber := 0 ]
%

category: 'printing'
set compile_env: 0
method: CypressVersionReference
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: ' name: '.
	self name printOn: aStream
%

category: 'accessing'
set compile_env: 0
method: CypressVersionReference
versionNumber
	"Answer the version of the receiver."

	^ versionNumber
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
	expectedDefinitions do: [:each | badDefinitions remove: each ifAbsent: []].
	^(badDefinitions collect: [:each | each printString]) sortAscending
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

	loader unloadable notEmpty
		ifTrue: [summary at: 'Unloadable' put: (loader unloadable collect: [:each | each printString])].
	loader errors notEmpty
		ifTrue: [summary at: 'Errors' put: (loader errors collect: [:each | each printString])].
	loader requirements notEmpty
		ifTrue: [summary at: 'Missing Requirements' put: loader requirements asArray].

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

! Class Implementation for CypressAbstractRepository

! ------------------- Class methods for CypressAbstractRepository

category: 'instance creation'
set compile_env: 0
classmethod: CypressAbstractRepository
createOn: aUrl alias: aString

	^self createOn: aUrl alias: aString
%

category: 'accessing'
set compile_env: 0
classmethod: CypressAbstractRepository
defaultCopyrightNotice

	^DefaultCopyrightNotice
%

category: 'accessing'
set compile_env: 0
classmethod: CypressAbstractRepository
defaultCopyrightNotice: aString

	DefaultCopyrightNotice := aString
%

category: 'initializing'
set compile_env: 0
classmethod: CypressAbstractRepository
initialize

	self initializeDefaultCopyrightNotice
%

category: 'initializing'
set compile_env: 0
classmethod: CypressAbstractRepository
initializeDefaultCopyrightNotice

	self defaultCopyrightNotice isNil ifFalse: [^self].
	self defaultCopyrightNotice: 'This work is protected by copyright. All rights reserved.'
%

category: 'instance creation'
set compile_env: 0
classmethod: CypressAbstractRepository
onUrl: aUrl alias: aString

	^(aUrl repositoryClass new)
		initializeUrl: aUrl andAlias: aString;
		yourself
%

! ------------------- Instance methods for CypressAbstractRepository

category: 'accessing properties'
set compile_env: 0
method: CypressAbstractRepository
alias

	^properties 
		at: 'alias'
		ifAbsent: ['']
%

category: 'accessing properties'
set compile_env: 0
method: CypressAbstractRepository
alias: aString

	properties 
		at: 'alias'
		put: aString
%

category: 'accessing properties'
set compile_env: 0
method: CypressAbstractRepository
copyrightProperty

	^properties 
		at: '_cypress_copyright'
		ifAbsent: ['']
%

category: 'accessing properties'
set compile_env: 0
method: CypressAbstractRepository
copyrightProperty: aString

	properties 
		at: '_cypress_copyright'
		put: aString
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractRepository
defaultCopyrightNotice

	^self class defaultCopyrightNotice
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractRepository
description

	^self alias
%

category: 'initializing - private'
set compile_env: 0
method: CypressAbstractRepository
initialize

	self initializeDefaultRepositoryProperties.
	readerClass := CypressPackageReader.
	writerClass := CypressPackageWriter.
%

category: 'initializing - private'
set compile_env: 0
method: CypressAbstractRepository
initializeDefaultCopyrightProperty

	self copyrightProperty: self defaultCopyrightNotice
%

category: 'initializing - private'
set compile_env: 0
method: CypressAbstractRepository
initializeDefaultRepositoryProperties

	properties := Dictionary new.
	self initializeDefaultCopyrightProperty
%

category: 'initializing - private'
set compile_env: 0
method: CypressAbstractRepository
initializeUrl: aUrl andAlias: aString

	self
		initialize;
		url: aUrl;
		alias: aString;
		validateUrl
%

category: 'accessing properties'
set compile_env: 0
method: CypressAbstractRepository
packageExtension

	^properties 
		at: 'packageExtension'
		ifAbsent: ['.package']
%

category: 'printing'
set compile_env: 0
method: CypressAbstractRepository
printDetailsOn: aStream

	aStream nextPutAll: self alias
%

category: 'printing'
set compile_env: 0
method: CypressAbstractRepository
printOn: aStream

	aStream
		nextPutAll: self class name;
		nextPutAll: '('.
	self printDetailsOn: aStream.
	aStream nextPutAll: ')'
%

category: 'reading'
set compile_env: 0
method: CypressAbstractRepository
reader

	^readerClass forRepository: self
%

category: 'reading'
set compile_env: 0
method: CypressAbstractRepository
readPackageStructureForPackageNamed: packageName

	^(self reader)
		readPackageStructureForPackageNamed: packageName;
		packageStructure
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractRepository
url

	^url
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractRepository
url: aString

	url := aString
%

category: 'validating - private'
set compile_env: 0
method: CypressAbstractRepository
validateUrl
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."
%

category: 'writing'
set compile_env: 0
method: CypressAbstractRepository
writePackageStructure: aPackageStructure

	^self writer writePackageStructure: aPackageStructure
%

category: 'writing'
set compile_env: 0
method: CypressAbstractRepository
writer

	^writerClass forRepository: self
%

! Class Implementation for CypressFileSystemRepository

! ------------------- Class methods for CypressFileSystemRepository

category: 'instance creation'
set compile_env: 0
classmethod: CypressFileSystemRepository
on: aDirectory

	^self new
		initializeForDirectory: aDirectory;
		yourself.
%

! ------------------- Instance methods for CypressFileSystemRepository

category: 'accessing - properties'
set compile_env: 0
method: CypressFileSystemRepository
codeFormatProperty

	^properties 
		at: '_gs_format'
		ifAbsent: ['Cypress']
%

category: 'updating properties'
set compile_env: 0
method: CypressFileSystemRepository
codeFormatProperty: aString

	self validate: aString isOneOf: #('Cypress' 'FileTree' 'Flexible').
	properties 
		at: '_gs_format'
		put: aString
%

category: 'accessing'
set compile_env: 0
method: CypressFileSystemRepository
description

	| desc |
	desc := super description.
	^desc notEmpty
		ifTrue: [desc]
		ifFalse: [self directoryPath]
%

category: 'accessing'
set compile_env: 0
method: CypressFileSystemRepository
directoryPath

	^directoryPath
%

category: 'initializing - private'
set compile_env: 0
method: CypressFileSystemRepository
directoryPath:  aString

	directoryPath := aString
%

category: 'testing - private'
set compile_env: 0
method: CypressFileSystemRepository
doesRepositoryFileExist: fileName
	"Answer whether the named file exists at the repository level."

	^self fileUtils
		directoryExists: (self fileUtils
				directoryFromPath: fileName
				relativeTo: self directoryPath)
%

category: 'initializing - private'
set compile_env: 0
method: CypressFileSystemRepository
ensureDirectoryPathExists

	self fileUtils ensureDirectoryExists: self directoryPath
%

category: 'accessing - private'
set compile_env: 0
method: CypressFileSystemRepository
fileUtils

	^CypressFileUtilities current
%

category: 'initializing - private'
set compile_env: 0
method: CypressFileSystemRepository
fixupMissingCopyrightProperty

	self copyrightProperty isEmpty ifFalse: [^self].
	self initializeDefaultCopyrightProperty.
%

category: 'initializing - private'
set compile_env: 0
method: CypressFileSystemRepository
initializeCreationOn: aUrl alias: aString

	self
		initializeUrl: aUrl andAlias: aString;
		alias: aString;
		writePropertiesFile
%

category: 'initializing - private'
set compile_env: 0
method: CypressFileSystemRepository
initializeDefaultRepositoryProperties

	super initializeDefaultRepositoryProperties.
	self
		codeFormatProperty: 'Cypress';
		strictCodeFormat: false.
%

category: 'initializing - private'
set compile_env: 0
method: CypressFileSystemRepository
initializeForDirectory: aDirectory

	self initialize.
	self directoryPath: aDirectory.
	self directoryPath isEmpty ifTrue: [^self].	"Not really valid; not a very good idea."
	self ensureDirectoryPathExists.
	self readPropertiesFile.
	self fixupMissingCopyrightProperty.
	self initializeReaderAndWriterClasses.
%

category: 'initializing - private'
set compile_env: 0
method: CypressFileSystemRepository
initializeForFileTreeRepository

	self initializeDefaultRepositoryProperties.
	self
		alias: 'FileTree read-only repository on ', self directoryPath;
		codeFormatProperty: 'FileTree';
		strictCodeFormat: true.
%

category: 'initializing - private'
set compile_env: 0
method: CypressFileSystemRepository
initializeReaderAndWriterClasses

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
%

category: 'initializing - private'
set compile_env: 0
method: CypressFileSystemRepository
initializeUrl: aUrl andAlias: aString

	super initializeUrl: aUrl andAlias: aString.
	self directoryPath: self url pathForDirectory.
	self ensureDirectoryPathExists.
	self readPropertiesFile.
	self codeFormatProperty: self url codeFormat.
	self strictCodeFormat: self url isStrict.
	self fixupMissingCopyrightProperty.
	self initializeReaderAndWriterClasses.
%

category: 'testing properties'
set compile_env: 0
method: CypressFileSystemRepository
isCodeFormatCypress

	^self isCodeFormatProperty: 'Cypress'
%

category: 'testing properties'
set compile_env: 0
method: CypressFileSystemRepository
isCodeFormatFileTree

	^self isCodeFormatProperty: 'FileTree'
%

category: 'testing properties'
set compile_env: 0
method: CypressFileSystemRepository
isCodeFormatFlexiblyCypress

	^self isCodeFormatStrict not and: [self isCodeFormatCypress]
%

category: 'testing properties'
set compile_env: 0
method: CypressFileSystemRepository
isCodeFormatFlexiblyFileTree

	^self isCodeFormatStrict not and: [self isCodeFormatFileTree]
%

category: 'testing properties - private'
set compile_env: 0
method: CypressFileSystemRepository
isCodeFormatProperty: aString

	^(properties at: '_gs_format') equalsNoCase: aString
%

category: 'testing properties'
set compile_env: 0
method: CypressFileSystemRepository
isCodeFormatStrict

	^(properties 
		at: '_gs_strict'
		ifAbsent: ['']) equalsNoCase: 'true'
%

category: 'testing properties'
set compile_env: 0
method: CypressFileSystemRepository
isCodeFormatStrictlyCypress

	^self isCodeFormatStrict and: [self isCodeFormatCypress]
%

category: 'testing properties'
set compile_env: 0
method: CypressFileSystemRepository
isCodeFormatStrictlyFileTree

	^self isCodeFormatStrict and: [self isCodeFormatFileTree]
%

category: 'accessing'
set compile_env: 0
method: CypressFileSystemRepository
packageNames

	^(self fileUtils directoryEntriesFrom: self directoryPath , '*', self packageExtension)
		collect: [:each | (self fileUtils localNameFrom: each) copyWithoutSuffix: self packageExtension]
%

category: 'printing'
set compile_env: 0
method: CypressFileSystemRepository
printDetailsOn: aStream

	self alias notEmpty
		ifTrue: 
			[aStream
				nextPutAll: self alias;
				nextPutAll: ': '].
	aStream nextPutAll: self url printString
%

category: 'reading'
set compile_env: 0
method: CypressFileSystemRepository
readPropertiesFile

	self readPropertiesFile: (#('properties.ston' 'properties.json' '.filetree')
				detect: [:each | self doesRepositoryFileExist: each]
				ifNone: [^self]).
%

category: 'reading'
set compile_env: 0
method: CypressFileSystemRepository
readPropertiesFile: fileName
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
%

category: 'updating properties'
set compile_env: 0
method: CypressFileSystemRepository
strictCodeFormat: aBoolean

	self strictCodeFormatProperty: aBoolean printString

%

category: 'updating properties - private'
set compile_env: 0
method: CypressFileSystemRepository
strictCodeFormatProperty: aString

	self validate: aString isOneOf: #('true' 'false').
	properties 
		at: '_gs_strict'
		put: aString
%

category: 'updating properties - private'
set compile_env: 0
method: CypressFileSystemRepository
validate: aString isOneOf: someStrings

	someStrings
		detect: [:each | aString equalsNoCase: each]
		ifNone: [self error: aString printString, ' must be one of ', someStrings printString].
%

category: 'validating - private'
set compile_env: 0
method: CypressFileSystemRepository
validateUrl
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

	self url fileName isEmpty
		ifFalse: [self error: self printString, ' should not be used with URLs for file names (', self url fileName, ' in ', self url pathForDirectory, ')']
%

category: 'writing'
set compile_env: 0
method: CypressFileSystemRepository
writePropertiesFile

	self writePropertiesFile: (self isCodeFormatFileTree
				ifTrue: ['properties.json']
				ifFalse: ['properties.ston'])
%

category: 'writing'
set compile_env: 0
method: CypressFileSystemRepository
writePropertiesFile: fileName

	self fileUtils
		writeStreamFor: fileName
		in: self directoryPath
		do: [:fileStream | properties writeCypressJsonOn: fileStream]
%

! Class Implementation for CypressAbstractFileoutRepository

! ------------------- Class methods for CypressAbstractFileoutRepository

category: 'instance creation'
set compile_env: 0
classmethod: CypressAbstractFileoutRepository
on: aDirectory

	^self new
		initializeForDirectory: aDirectory;
		yourself.
%

! ------------------- Instance methods for CypressAbstractFileoutRepository

category: 'accessing'
set compile_env: 0
method: CypressAbstractFileoutRepository
description

	| desc |
	desc := super description.
	^desc notEmpty
		ifTrue: [desc]
		ifFalse: [self directoryPath]
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractFileoutRepository
directoryPath

	^directoryPath
%

category: 'initializing - private'
set compile_env: 0
method: CypressAbstractFileoutRepository
directoryPath:  aString

	directoryPath := aString
%

category: 'initializing - private'
set compile_env: 0
method: CypressAbstractFileoutRepository
ensureDirectoryPathExists

	self fileUtils ensureDirectoryExists: self directoryPath
%

category: 'accessing - private'
set compile_env: 0
method: CypressAbstractFileoutRepository
fileUtils

	^CypressFileUtilities current
%

category: 'initializing - private'
set compile_env: 0
method: CypressAbstractFileoutRepository
initializeCreationOn: aUrl alias: aString

	self
		initializeUrl: aUrl andAlias: aString;
		alias: aString
%

category: 'initializing - private'
set compile_env: 0
method: CypressAbstractFileoutRepository
initializeForDirectory: aDirectory

	self initialize.
	self directoryPath: aDirectory.
	self directoryPath isEmpty ifTrue: [^self].	"Not really valid; not a very good idea."
	self ensureDirectoryPathExists.
	self initializeReaderAndWriterClasses.
%

category: 'initializing - private'
set compile_env: 0
method: CypressAbstractFileoutRepository
initializeReaderAndWriterClasses

	self suubclassResponsibility: #initializeReaderAndWriterClasses
%

category: 'initializing - private'
set compile_env: 0
method: CypressAbstractFileoutRepository
initializeUrl: aUrl andAlias: aString

	super initializeUrl: aUrl andAlias: aString.
	self directoryPath: self url pathForDirectory.
	self ensureDirectoryPathExists.
	self initializeReaderAndWriterClasses.
%

category: 'printing'
set compile_env: 0
method: CypressAbstractFileoutRepository
printDetailsOn: aStream

	self alias notEmpty
		ifTrue: 
			[aStream
				nextPutAll: self alias;
				nextPutAll: ': '].
	aStream nextPutAll: self url printString
%

category: 'validating - private'
set compile_env: 0
method: CypressAbstractFileoutRepository
validateUrl
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

	self url fileName isEmpty
		ifFalse: [self error: self printString, ' should not be used with URLs for file names (', self url fileName, ' in ', self url pathForDirectory, ')']
%

! Class Implementation for CypressTopazRepository

! ------------------- Instance methods for CypressTopazRepository

category: 'initializing - private'
set compile_env: 0
method: CypressTopazRepository
initializeReaderAndWriterClasses

	readerClass := CypressDoNothingPackageReader.
	writerClass := CypressTopazFileoutWriter.
%

! Class Implementation for CypressSmalltalkRepository

! ------------------- Instance methods for CypressSmalltalkRepository

category: 'initializing - private'
set compile_env: 0
method: CypressSmalltalkRepository
initializeReaderAndWriterClasses

	readerClass := CypressDoNothingPackageReader.
	writerClass := CypressSmalltalkFileoutWriter.
%

! Class Implementation for CypressPackageManager2

! ------------------- Class methods for CypressPackageManager2

category: 'Instance Creation'
set compile_env: 0
classmethod: CypressPackageManager2
create

	^self new
		initializeFromImage;
		yourself.
%

category: 'Initializing'
set compile_env: 0
classmethod: CypressPackageManager2
initialize

	self savedPackageManagers: IdentityDictionary new
%

category: 'Accessing'
set compile_env: 0
classmethod: CypressPackageManager2
named: aKey
	"Answer the Package Manager previously saved under aKey.
	 It is an error if there was not one saved under that key."

	^self
		named: aKey
		or: [self error: 'No previously saved Package Manager under the key ', aKey printString]
%

category: 'Accessing'
set compile_env: 0
classmethod: CypressPackageManager2
named: aKey or: aBlock
	"Answer the Package Manager previously saved under aKey.
	 Answer the result of evaluating aBlock, if there was not one saved under that key."

	^self savedPackageManagers at: aKey ifAbsent: aBlock
%

category: 'Instance Creation'
set compile_env: 0
classmethod: CypressPackageManager2
new

	^super new
		initialize;
		yourself.
%

category: 'Accessing - private'
set compile_env: 0
classmethod: CypressPackageManager2
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

category: 'Accessing - private'
set compile_env: 0
classmethod: CypressPackageManager2
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

category: 'Updating'
set compile_env: 0
classmethod: CypressPackageManager2
removePackageManagerSavedAs: aKey
	"Remove the Package Manager previously saved under aKey, if there was one.
	 Answer it or nil if there was not one saved under that key."

	^self savedPackageManagers removeKey: aKey ifAbsent: []
%

category: 'Accessing'
set compile_env: 0
classmethod: CypressPackageManager2
savedPackageManagers

	^SavedPackageManagers
%

category: 'Initializing - private'
set compile_env: 0
classmethod: CypressPackageManager2
savedPackageManagers: anIdentityDictionary

	SavedPackageManagers := anIdentityDictionary
%

! ------------------- Instance methods for CypressPackageManager2

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
addRepository: aRepository to: aKnownPackageInformation

	aKnownPackageInformation addRepository: aRepository.
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
addUnknownPackageNamed: aString

	self packageInformationList
		at: aString
		put: (CypressUnknownPackageInformation named: aString).
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
assignRepository: aRepository to: aPackageInformation

	self assignRepository: aRepository toAll: (Array with: aPackageInformation)
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
assignRepository: aRepository toAll: somePackageInformations
	"Assign to those having no repository information and add to those with.
	 Those without a repository need to be converted to Known Package Information instances."

	self knownRepositories at: aRepository url put: aRepository.
	(somePackageInformations reject: [:each | each isKnown])
		do: [:each | self addRepository: aRepository to: (self convertToKnown: each)].
	(somePackageInformations select: [:each | each isKnown])
		do: [:each | self addRepository: aRepository to: each].
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
convert: anUnknownPackageInformation toConflictingWith: aKnownPackageInformation

	| conflicting |
	conflicting := CypressConflictingPackageInformation
				fromUnknown: anUnknownPackageInformation
				conflictingWith: aKnownPackageInformation.
	self replace: anUnknownPackageInformation with: conflicting.
	^conflicting
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
convert: anUnknownPackageInformation toEclipsedBy: aKnownPackageInformation

	| eclipsed |
	eclipsed := CypressEclipsedPackageInformation
				fromUnknown: anUnknownPackageInformation
				eclipsedBy: aKnownPackageInformation.
	self replace: anUnknownPackageInformation with: eclipsed.
	^eclipsed
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
convertToKnown: aPackageInformation

	| known |
	known := CypressKnownPackageInformation fromUnknown: aPackageInformation.
	self replace: aPackageInformation with: known.
	(self findPackagesEclipsedBy: known) do: [:each | self convert: each toEclipsedBy: known].
	(self findPackagesConflictingWith: known) do: [:each | self convert: each toConflictingWith: known].
	^known.
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
createRepositoryNamed: aName under: aDirectory alias: aString schema: schemaName

	^CypressFileSystemRepository
		createOn: (schemaName
				, (CypressFileUtilities current
					directoryFromPath: aName
						relativeTo: aDirectory)
				, '/') asUrl
		alias: aString
%

category: 'Accessing - private'
set compile_env: 0
method: CypressPackageManager2
findPackagesConflictingWith: aKnownPackageInformation

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name
				and: [aKnownPackageInformation name beginsWith: each name , '-']]
%

category: 'Accessing - private'
set compile_env: 0
method: CypressPackageManager2
findPackagesEclipsedBy: aKnownPackageInformation

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name
				and: [each name beginsWith: aKnownPackageInformation name , '-']]
%

category: 'Initializing - private'
set compile_env: 0
method: CypressPackageManager2
initialize

	self
		knownRepositories: Dictionary new;
		packageInformationList: Dictionary new
%

category: 'Initializing - private'
set compile_env: 0
method: CypressPackageManager2
initializeFromImage

	self initializePackageInformationList.
%

category: 'Initializing - private'
set compile_env: 0
method: CypressPackageManager2
initializePackageInformationList

	self
		packageInformationList: (self potentialPackageNames
				inject: Dictionary new
				into: 
					[:dict :each |
					dict
						at: each put: (CypressUnknownPackageInformation named: each);
						yourself])
%

category: 'Accessing'
set compile_env: 0
method: CypressPackageManager2
knownRepositories

	^knownRepositories
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
knownRepositories: someNamedRepositories

	knownRepositories := someNamedRepositories
%

category: 'Loading'
set compile_env: 0
method: CypressPackageManager2
loadPackageFrom: aKnownPackageInformation inRepository: aRepository

	| snapshot summary loader |
	snapshot := (aRepository readPackageStructureForPackageNamed: aKnownPackageInformation name)
				snapshot.
	loader := snapshot updatePackage: (CypressPackageDefinition
						named: aKnownPackageInformation name).
	summary := Dictionary new.
	loader unloadable notEmpty
		ifTrue: 
			[summary
				at: 'Unloadable'
				put: (loader unloadable collect: [:each | each printString])].
	loader errors notEmpty
		ifTrue: 
			[summary
				at: 'Errors'
				put: (loader errors collect: [:each | each printString])].
	loader requirements notEmpty
		ifTrue: [summary
				at: 'Missing Requirements'
				put: loader requirements asArray].
	^summary.
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
lookForPackagesInRepository: aRepository
	"Find the packages in the repository and update the list
	 of package information accordingly."

	| packageNames inImage exImage |
	packageNames := aRepository packageNames.
	inImage := self packageInformationList asArray select: [:each | packageNames includes: each name].
	exImage := packageNames reject: [:each | self packageInformationList anySatisfy: [:info | info name = each]].
	exImage := exImage collect: [:each | CypressUnknownPackageInformation named: each].

	self assignRepository: aRepository toAll: inImage, exImage
%

category: 'Accessing'
set compile_env: 0
method: CypressPackageManager2
packageInformationList

	^packageInformationList
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
packageInformationList: someNamedPackageInformations

	packageInformationList := someNamedPackageInformations
%

category: 'Accessing'
set compile_env: 0
method: CypressPackageManager2
packageInformationNamed: aString

	^self packageInformationNamed: aString
		or: [self error: 'No package information for ' , aString printString]
%

category: 'Accessing'
set compile_env: 0
method: CypressPackageManager2
packageInformationNamed: aString or: aBlock

	^self packageInformationList
		at: aString
		ifAbsent: aBlock.
%

category: 'Accessing - private'
set compile_env: 0
method: CypressPackageManager2
potentialPackageNames

	^self class potentialPackageNames
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
replace: oldPackageInformation with: newPackageInformation

	self packageInformationList
		at: oldPackageInformation name
		put: newPackageInformation.
%

category: 'Initializing - private'
set compile_env: 0
method: CypressPackageManager2
repositoryOn: url alias: aString
	"Answer a repository instance for the specified URL.
	 The characteristics will come from the properties file at the URL location,
	 or will default according to the schema, if there is no properties file."

	^self knownRepositories
		at: url
		ifAbsentPut: [CypressAbstractRepository onUrl: url alias: aString].
%

category: 'Updating'
set compile_env: 0
method: CypressPackageManager2
saveAs: aKey
	"Save the receiver in the class' collection of named managers
	 under the specified key.
	 It will quietly replace anything already under that key."


	self savedPackageManagers at: aKey put: self.
%

category: 'Accessing - private'
set compile_env: 0
method: CypressPackageManager2
savedPackageManagers

	^self class savedPackageManagers
%

category: 'Writing'
set compile_env: 0
method: CypressPackageManager2
writeChangesToAllRepositoriesFor: aPackageInformation

	aPackageInformation writeChangesToAllRepositories.
%

! Class Implementation for CypressAbstractFileUrl

! ------------------- Class methods for CypressAbstractFileUrl

category: 'instance creation'
set compile_env: 0
classmethod: CypressAbstractFileUrl
absoluteFromText: aString
	"Return a URL from a string and handle
	a String without a scheme as a FileUrl."

	"Url absoluteFromText: 'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'" 
	"Url absoluteFromText: 'msw://chaos.resnet.gatech.edu:9000/testbook?top'"
	"Url absoluteFromText: 'telnet:chaos.resnet.gatech.edu'"
	"Url absoluteFromText: 'file:/etc/passwd'"

	| remainder index scheme fragment newUrl |
	"trim surrounding whitespace"
	remainder := aString trimSeparators.	

	"extract the fragment, if any"
	index := remainder indexOf: $#.
	index > 0 ifTrue: [
		fragment := remainder copyFrom: index + 1 to: remainder size.
		remainder := remainder copyFrom: 1 to: index - 1].

	"choose class based on the scheme name, and let that class do the bulk of the parsing"
	scheme := self schemeNameForString: remainder.
	newUrl := (self urlClassForScheme: scheme) new privateInitializeFromText: remainder.
	newUrl privateFragment: fragment.
	^newUrl
%

category: 'parsing'
set compile_env: 0
classmethod: CypressAbstractFileUrl
urlClassForScheme: scheme

	scheme isNil ifTrue: [^FileUrl].
	^super urlClassForScheme: scheme
%

! ------------------- Instance methods for CypressAbstractFileUrl

category: 'accessing'
set compile_env: 0
method: CypressAbstractFileUrl
codeFormat

	^self subclassResponsibility: #codeFormat.
%

category: 'testing'
set compile_env: 0
method: CypressAbstractFileUrl
isStrict

	^self subclassResponsibility: #isStrict.
%

category: 'accessing'
set compile_env: 0
method: CypressAbstractFileUrl
repositoryClass

	^CypressFileSystemRepository
%

! Class Implementation for CypressLaxFileUrl

! ------------------- Class methods for CypressLaxFileUrl

category: 'constants'
set compile_env: 0
classmethod: CypressLaxFileUrl
schemeName

	^'cypresslax'
%

! ------------------- Instance methods for CypressLaxFileUrl

category: 'accessing'
set compile_env: 0
method: CypressLaxFileUrl
codeFormat

	^'Cypress'
%

category: 'testing'
set compile_env: 0
method: CypressLaxFileUrl
isStrict

	^false
%

! Class Implementation for CypressFileTreeFormatFileUrl

! ------------------- Class methods for CypressFileTreeFormatFileUrl

category: 'constants'
set compile_env: 0
classmethod: CypressFileTreeFormatFileUrl
schemeName

	^'cypressft'
%

! ------------------- Instance methods for CypressFileTreeFormatFileUrl

category: 'accessing'
set compile_env: 0
method: CypressFileTreeFormatFileUrl
codeFormat

	^'FileTree'
%

category: 'testing'
set compile_env: 0
method: CypressFileTreeFormatFileUrl
isStrict

	^false
%

! Class Implementation for CypressFileUrl

! ------------------- Class methods for CypressFileUrl

category: 'constants'
set compile_env: 0
classmethod: CypressFileUrl
schemeName

	^'cypress'
%

! ------------------- Instance methods for CypressFileUrl

category: 'accessing'
set compile_env: 0
method: CypressFileUrl
codeFormat

	^'Cypress'
%

category: 'testing'
set compile_env: 0
method: CypressFileUrl
isStrict

	^true
%

! Class Implementation for CypressFileTreeReadOnlyFileUrl

! ------------------- Class methods for CypressFileTreeReadOnlyFileUrl

category: 'constants'
set compile_env: 0
classmethod: CypressFileTreeReadOnlyFileUrl
schemeName

	^'cypressfiletree'
%

! ------------------- Instance methods for CypressFileTreeReadOnlyFileUrl

category: 'accessing'
set compile_env: 0
method: CypressFileTreeReadOnlyFileUrl
codeFormat

	^'FileTree'
%

category: 'testing'
set compile_env: 0
method: CypressFileTreeReadOnlyFileUrl
isStrict

	^true
%

! Class Implementation for CypressTopazUrl

! ------------------- Class methods for CypressTopazUrl

category: 'constants'
set compile_env: 0
classmethod: CypressTopazUrl
schemeName

	^'topaz'
%

! ------------------- Instance methods for CypressTopazUrl

category: 'accessing'
set compile_env: 0
method: CypressTopazUrl
codeFormat

	^'Topaz'
%

category: 'testing'
set compile_env: 0
method: CypressTopazUrl
isStrict

	^true
%

category: 'accessing'
set compile_env: 0
method: CypressTopazUrl
repositoryClass

	^CypressTopazRepository
%

! Class Implementation for CypressSmalltalkUrl

! ------------------- Class methods for CypressSmalltalkUrl

category: 'constants'
set compile_env: 0
classmethod: CypressSmalltalkUrl
schemeName

	^'chunk'
%

! ------------------- Instance methods for CypressSmalltalkUrl

category: 'accessing'
set compile_env: 0
method: CypressSmalltalkUrl
codeFormat

	^'Chunk'
%

category: 'testing'
set compile_env: 0
method: CypressSmalltalkUrl
isStrict

	^true
%

category: 'accessing'
set compile_env: 0
method: CypressSmalltalkUrl
repositoryClass

	^CypressSmalltalkRepository
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

! Class Implementation for CypressAbstractPackageInformation

! ------------------- Class methods for CypressAbstractPackageInformation

category: 'Instance Creation'
set compile_env: 0
classmethod: CypressAbstractPackageInformation
named: aString

	^self new
		initializeWithName: aString;
		yourself
%

! ------------------- Instance methods for CypressAbstractPackageInformation

category: 'Initializing - private'
set compile_env: 0
method: CypressAbstractPackageInformation
initialize
%

category: 'Initializing - private'
set compile_env: 0
method: CypressAbstractPackageInformation
initializeWithName: aString

	self initialize.
	self name: aString
%

category: 'Testing'
set compile_env: 0
method: CypressAbstractPackageInformation
isKnown

	^false
%

category: 'Accessing'
set compile_env: 0
method: CypressAbstractPackageInformation
name

	^name
%

category: 'Updating'
set compile_env: 0
method: CypressAbstractPackageInformation
name: aString

	name := aString
%

category: 'Printing'
set compile_env: 0
method: CypressAbstractPackageInformation
printDetailsOn: aStream

	aStream nextPutAll: self name
%

category: 'Accessing'
set compile_env: 0
method: CypressAbstractPackageInformation
repositories

	^#()
%

! Class Implementation for CypressEclipsedPackageInformation

! ------------------- Class methods for CypressEclipsedPackageInformation

category: 'Instance Creation'
set compile_env: 0
classmethod: CypressEclipsedPackageInformation
fromUnknown: unknownPackageInformation eclipsedBy: knownPackageInformation

	^(self named: unknownPackageInformation name)
		eclipsedBy: knownPackageInformation;
		yourself.
%

! ------------------- Instance methods for CypressEclipsedPackageInformation

category: 'Accessing'
set compile_env: 0
method: CypressEclipsedPackageInformation
eclipsedBy

	^eclipsedBy
%

category: 'Updating'
set compile_env: 0
method: CypressEclipsedPackageInformation
eclipsedBy: aPackageInformation

	eclipsedBy := aPackageInformation
%

! Class Implementation for CypressUnknownPackageInformation

! Class Implementation for CypressKnownPackageInformation

! ------------------- Class methods for CypressKnownPackageInformation

category: 'Instance Creation'
set compile_env: 0
classmethod: CypressKnownPackageInformation
fromUnknown: aPackageInformation

	^self named: aPackageInformation name
%

! ------------------- Instance methods for CypressKnownPackageInformation

category: 'Updating'
set compile_env: 0
method: CypressKnownPackageInformation
addRepository: aRepository

	self repositories at: aRepository url put: aRepository.
	self updateDigestsFromImageAndRepository: aRepository.
%

category: 'Accessing - digests'
set compile_env: 0
method: CypressKnownPackageInformation
determineDigestFromImage

	^self packageStructure digest
%

category: 'Accessing - digests'
set compile_env: 0
method: CypressKnownPackageInformation
determineDigestFromRepository: aRepository

	^ (aRepository readPackageStructureForPackageNamed: self name) digest.
%

category: 'Accessing - digests'
set compile_env: 0
method: CypressKnownPackageInformation
digestFor: source

	^self digestFor: source or: [nil]
%

category: 'Accessing - digests'
set compile_env: 0
method: CypressKnownPackageInformation
digestFor: source or: aBlock

	^self digests at: source ifAbsent: aBlock
%

category: 'Accessing - digests'
set compile_env: 0
method: CypressKnownPackageInformation
digests

	^digests
%

category: 'Initializing - private'
set compile_env: 0
method: CypressKnownPackageInformation
digests: anIdentityDictionary
	"Key: Source of package definitions (#IMAGE or a Repository url)
	 Value: (e.g. MD5) Digest of the package's defintions or an empty string.
	 A nil digest means there are no definitions for the given source."

   digests := anIdentityDictionary
%

category: 'Accessing - digests'
set compile_env: 0
method: CypressKnownPackageInformation
imageDigest

	^self digestFor: #IMAGE.
%

category: 'Initializing - private'
set compile_env: 0
method: CypressKnownPackageInformation
initialize

	super initialize.
	self
		repositories: IdentityDictionary new;
		digests: IdentityDictionary new.
%

category: 'Testing'
set compile_env: 0
method: CypressKnownPackageInformation
isKnown

	^true
%

category: 'Accessing - digests'
set compile_env: 0
method: CypressKnownPackageInformation
packageStructure

	^CypressPackageStructure
		fromPackage: (CypressPackageDefinition named: self name)
%

category: 'Updating'
set compile_env: 0
method: CypressKnownPackageInformation
removeRepository: aRepository

	self repositories removeKey: aRepository url ifAbsent: [].
	self digests removeKey: aRepository url ifAbsent: [].
	self updateDigestsFromImage.
%

category: 'Accessing'
set compile_env: 0
method: CypressKnownPackageInformation
repositories

	^repositories
%

category: 'Initializing - private'
set compile_env: 0
method: CypressKnownPackageInformation
repositories: anIdentityDictionary
	"Key: Repository url
	 Value: Repository"

	repositories := anIdentityDictionary
%

category: 'Accessing - digests'
set compile_env: 0
method: CypressKnownPackageInformation
repositoryDigests

	^self repositories collect: [:each | self digestFor: each url]
%

category: 'Updating - digests'
set compile_env: 0
method: CypressKnownPackageInformation
updateDigestsFromAllRepositories

	self repositories do: [:each | self updateDigestsFromRepository: each]
%

category: 'Updating - digests'
set compile_env: 0
method: CypressKnownPackageInformation
updateDigestsFromImage

	self digests at: #IMAGE put: self determineDigestFromImage
%

category: 'Updating - digests'
set compile_env: 0
method: CypressKnownPackageInformation
updateDigestsFromImageAndAllRepositories

	self
		updateDigestsFromImage;
		updateDigestsFromAllRepositories
%

category: 'Updating - digests'
set compile_env: 0
method: CypressKnownPackageInformation
updateDigestsFromImageAndRepository: aRepository

	self
		updateDigestsFromImage;
		updateDigestsFromRepository: aRepository
%

category: 'Updating - digests'
set compile_env: 0
method: CypressKnownPackageInformation
updateDigestsFromRepository: aRepository

	self digests at: aRepository url put: (self determineDigestFromRepository: aRepository).
%

category: 'Writing'
set compile_env: 0
method: CypressKnownPackageInformation
writeChangesToAllRepositories

	| imageDigest changedDigests changedRepositories |
	self updateDigestsFromImageAndAllRepositories.
	imageDigest := self imageDigest.
	changedDigests := self repositoryDigests reject: [:each | each = imageDigest].
	changedRepositories := changedDigests keys collect: [:each | self repositories at: each].
	self writePackageToRepositories: changedRepositories.
%

category: 'Writing'
set compile_env: 0
method: CypressKnownPackageInformation
writePackageToRepositories: someRepositories

	| packageStructure |
	packageStructure := self packageStructure.
	^someRepositories
		do: [:each | each writePackageStructure: packageStructure]
%

! Class Implementation for CypressConflictingPackageInformation

! ------------------- Class methods for CypressConflictingPackageInformation

category: 'Instance Creation'
set compile_env: 0
classmethod: CypressConflictingPackageInformation
fromUnknown: unknownPackageInformation conflictingWith: knownPackageInformation

	^(self named: unknownPackageInformation name)
		conflictsWith: knownPackageInformation;
		yourself.
%

! ------------------- Instance methods for CypressConflictingPackageInformation

category: 'Accessing'
set compile_env: 0
method: CypressConflictingPackageInformation
conflictsWith

	^conflictsWith
%

category: 'Updating'
set compile_env: 0
method: CypressConflictingPackageInformation
conflictsWith: someCypressKnownPackageInformations

	conflictsWith := someCypressKnownPackageInformations
%

! Class Extensions

! Class initializers 

doit
CypressAbstractRepository initialize.
CypressPackageManager2 initialize.
%



! End of Package: Cypress-PackageManagement


