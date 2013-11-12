" Package: Cypress-PackageManagement"!


" Class Declarations "!

(Object
	subclass: 'CypressVersionReference'
	instVarNames: #( name package author branch versionNumber )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: 'A CypressVersionReference refers to a specific version of a Monticello package.'!

(Object
	subclass: 'CypressPackageManager'
	instVarNames: #( knownPackages knownRepositories packageInformationList )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(Object
	subclass: 'CypressAbstractRepository'
	instVarNames: #( url properties readerClass writerClass )
	classVars: #( DefaultCopyrightNotice )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractRepository
	subclass: 'CypressFileSystemRepository'
	instVarNames: #( directoryPath )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractRepository
	subclass: 'CypressAbstractFileoutRepository'
	instVarNames: #( directoryPath )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractFileoutRepository
	subclass: 'CypressTopazRepository'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: 'This is a "write-only" repository.
It could be made readable, to be able to file-in Topaz scripts, but it''s not the same thing.
'!

(CypressAbstractFileoutRepository
	subclass: 'CypressSmalltalkRepository'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: 'This is a "write-only" repository.
It could be made readable, to be able to file-in Smalltalk scripts, but it''s not the same thing.
'!

(Object
	subclass: 'CypressPackageManager2'
	instVarNames: #( knownRepositories packageInformationList )
	classVars: #( SavedPackageManagers )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(FileUrl
	subclass: 'CypressAbstractFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractFileUrl
	subclass: 'CypressLaxFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractFileUrl
	subclass: 'CypressFileTreeFormatFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractFileUrl
	subclass: 'CypressFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractFileUrl
	subclass: 'CypressFileTreeReadOnlyFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractFileUrl
	subclass: 'CypressTopazUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractFileUrl
	subclass: 'CypressSmalltalkUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressObject
	subclass: 'CypressPackageInformation'
	instVarNames: #( name type advice competingPackageNames imageDefinitions savedDefinitions savedLocation repository repositoryDescription imageCounts changesCount )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
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

'!

(CypressObject
	subclass: 'CypressAbstractPackageInformation'
	instVarNames: #( name )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractPackageInformation
	subclass: 'CypressEclipsedPackageInformation'
	instVarNames: #( eclipsedBy )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractPackageInformation
	subclass: 'CypressUnknownPackageInformation'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractPackageInformation
	subclass: 'CypressKnownPackageInformation'
	instVarNames: #( repositories digests )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

(CypressAbstractPackageInformation
	subclass: 'CypressConflictingPackageInformation'
	instVarNames: #( conflictsWith )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-PackageManagement';
		comment: ''!

" Class Implementation for CypressVersionReference"!

" ------------------- Class methods for CypressVersionReference"!

!CypressVersionReference class methodsFor: 'instance creation'!
name: aString

	^(self basicNew)
		initializeName: aString;
		yourself
! !
!CypressVersionReference class methodsFor: 'instance creation'!
new

	self error: 'Use #name: to initialize the receiver.'
! !
" ------------------- Instance methods for CypressVersionReference"!

!CypressVersionReference methodsFor: 'comparing'!
= aReference

	^self class = aReference class
		and: [self name = aReference name]
! !
!CypressVersionReference methodsFor: 'accessing'!
author
	"Answer the author of the receiver."
	
	^ author
! !
!CypressVersionReference methodsFor: 'accessing'!
branch
	"Answer the branch of the receiver."
	
	^ branch
! !
!CypressVersionReference methodsFor: 'comparing'!
hash

	^self name hash
! !
!CypressVersionReference methodsFor: 'initialization'!
initializeName: aString

	name := aString.
	self parseName: aString
! !
!CypressVersionReference methodsFor: 'private'!
matches: aResolvedReference
	^ self name = aResolvedReference name
! !
!CypressVersionReference methodsFor: 'accessing'!
name
	"Answer the name of this reference."
	
	^ name
! !
!CypressVersionReference methodsFor: 'accessing'!
packageName
	"Answer the package of the receiver."

	^ package
! !
!CypressVersionReference methodsFor: 'initialization'!
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
! !
!CypressVersionReference methodsFor: 'printing'!
printOn: aStream

	super printOn: aStream.
	aStream nextPutAll: ' name: '.
	self name printOn: aStream
! !
!CypressVersionReference methodsFor: 'accessing'!
versionNumber
	"Answer the version of the receiver."

	^ versionNumber
! !
" Class Implementation for CypressPackageManager"!

" ------------------- Class methods for CypressPackageManager"!

!CypressPackageManager class methodsFor: 'instance creation'!
new

	^super new
		initialize;
		yourself
! !
!CypressPackageManager class methodsFor: 'accessing'!
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
	^names! !
!CypressPackageManager class methodsFor: 'accessing'!
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
		sortAscending! !
" ------------------- Instance methods for CypressPackageManager"!

!CypressPackageManager methodsFor: 'comparing'!
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
	^(badDefinitions collect: [:each | each printString]) sortAscending! !
!CypressPackageManager methodsFor: 'comparing'!
comparePackageFrom: aCypressPackageInformation

	^self comparePackagesFrom: (Array with: aCypressPackageInformation)! !
!CypressPackageManager methodsFor: 'comparing'!
comparePackagesFrom: someCypressPackageInformations

	^(someCypressPackageInformations
		inject: CypressPackageComparator new
		into: 
			[:comparer :each |
			comparer comparingPackageNamed: each name fromDirectory: each savedLocation])
				getDifferences! !
!CypressPackageManager methodsFor: 'updating - private'!
determineKnownPackages

	^(packageInformationList select: [:each | each repository notNil])
		inject: Dictionary new
		into: 
			[:dict :each |
			dict
				at: each name put: each savedLocation;
				yourself]! !
!CypressPackageManager methodsFor: 'initializing - private'!
initialize

	self refreshPackageInformation.
! !
!CypressPackageManager methodsFor: 'initializing - private'!
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
		keysAndValuesDo: [:package :conflicts | package beConflictedWith: conflicts]! !
!CypressPackageManager methodsFor: 'initializing - private'!
initializeKnownPackages

	knownPackages := (System myUserProfile objectNamed: #KnownCypressPackages)
				ifNil: [Dictionary new]! !
!CypressPackageManager methodsFor: 'initializing - private'!
initializeKnownRepositories

	knownRepositories := Dictionary new.
	knownPackages asSet
		do: [:each | self repositoryOn: each]! !
!CypressPackageManager methodsFor: 'initializing - private'!
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
					CypressPackageInformation named: each repository: repo]! !
!CypressPackageManager methodsFor: 'initializing - private'!
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
		keysAndValuesDo: [:package :baseNames | package beQualifiedNameOf: baseNames]! !
!CypressPackageManager methodsFor: 'updating'!
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
! !
!CypressPackageManager methodsFor: 'updating'!
lookForLoadedPackagesIn: aDirectory
	"Update any of the packages in the image which have a Cypress file out in
	 the specified directory to reflect the path where the package has theoretically
	 been saved."

	self lookForLoadedPackagesInRepository: (self repositoryOn: aDirectory).
	^nil! !
!CypressPackageManager methodsFor: 'updating'!
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
	^nil! !
!CypressPackageManager methodsFor: 'updating'!
lookForUnloadedPackagesIn: aDirectory
	"Load any package names from aDirectory as known packages.
	 This does not load the package contents."

	self lookForUnloadedPackagesInRepository: (self repositoryOn: aDirectory).
	^nil! !
!CypressPackageManager methodsFor: 'updating'!
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
	^nil! !
!CypressPackageManager methodsFor: 'accessing'!
packageInformationList

	^packageInformationList! !
!CypressPackageManager methodsFor: 'accessing'!
potentialPackageNames

	^self class potentialPackageNames! !
!CypressPackageManager methodsFor: 'accessing'!
refreshedPackageInformationList

	self refreshPackageInformation.
	^self packageInformationList.
! !
!CypressPackageManager methodsFor: 'updating'!
refreshPackageInformation

	self
		initializeKnownPackages;
		initializeKnownRepositories;
		initializePackageInformationList;
		initializeConflictingPackageNames;
		initializeQualifiedPackageNames! !
!CypressPackageManager methodsFor: 'initializing - private'!
repositoryOn: aDirectory

	^knownRepositories
		at: aDirectory
		ifAbsentPut: [CypressFileSystemRepository on: aDirectory].
! !
!CypressPackageManager methodsFor: 'updating - private'!
saveKnownPackages

	self updateKnownPackages.
	((System myUserProfile resolveSymbol: #KnownCypressPackages)
		ifNil: 
			[(System myUserProfile objectNamed: #UserGlobals)
				addAssociation: #KnownCypressPackages -> Dictionary new])
			value: knownPackages! !
!CypressPackageManager methodsFor: 'updating - private'!
updateKnownPackages

	knownPackages := self determineKnownPackages! !
!CypressPackageManager methodsFor: 'updating'!
updateSavedLocation: aDirectory for: aCypressPackageInformation
	"Update the specified package to reflect the path and repository where the
	 package should be saved."

	aCypressPackageInformation
		updateKnownPackageRepository: (self repositoryOn: aDirectory).
	self saveKnownPackages.
	^nil! !
!CypressPackageManager methodsFor: 'writing - private'!
writeCypressPackageToDiskFrom: aCypressPackageInformation

	| packageStructure |
	packageStructure := CypressPackageStructure
				fromPackage: (CypressPackageDefinition
						named: aCypressPackageInformation name).
	aCypressPackageInformation repository writer
		writePackageStructure: packageStructure! !
!CypressPackageManager methodsFor: 'writing'!
writePackagesToDiskFrom: someCypressPackageInformations

	| packageStructure |
	^someCypressPackageInformations do: 
			[:each |
			packageStructure := CypressPackageStructure
						fromPackage: (CypressPackageDefinition named: each name).
			each repository writer writePackageStructure: packageStructure.
			each refresh.
			self saveKnownPackages]! !
!CypressPackageManager methodsFor: 'writing - private'!
writePackageStructure: packageStructure to: aCypressRepository

	aCypressRepository writer writePackageStructure: packageStructure! !
!CypressPackageManager methodsFor: 'writing'!
writePackageToDiskFrom: aCypressPackageInformation

	^self writePackagesToDiskFrom: (Array with: aCypressPackageInformation)! !
" Class Implementation for CypressAbstractRepository"!

" ------------------- Class methods for CypressAbstractRepository"!

!CypressAbstractRepository class methodsFor: 'instance creation'!
createOn: aUrl alias: aString

	^self createOn: aUrl alias: aString! !
!CypressAbstractRepository class methodsFor: 'accessing'!
defaultCopyrightNotice

	^DefaultCopyrightNotice! !
!CypressAbstractRepository class methodsFor: 'accessing'!
defaultCopyrightNotice: aString

	DefaultCopyrightNotice := aString! !
!CypressAbstractRepository class methodsFor: 'initializing'!
initialize

	self initializeDefaultCopyrightNotice! !
!CypressAbstractRepository class methodsFor: 'initializing'!
initializeDefaultCopyrightNotice

	self defaultCopyrightNotice isNil ifFalse: [^self].
	self defaultCopyrightNotice: 'This work is protected by copyright. All rights reserved.'
! !
!CypressAbstractRepository class methodsFor: 'instance creation'!
onUrl: aUrl alias: aString

	^(aUrl repositoryClass new)
		initializeUrl: aUrl andAlias: aString;
		yourself! !
" ------------------- Instance methods for CypressAbstractRepository"!

!CypressAbstractRepository methodsFor: 'accessing properties'!
alias

	^properties 
		at: 'alias'
		ifAbsent: ['']
! !
!CypressAbstractRepository methodsFor: 'accessing properties'!
alias: aString

	properties 
		at: 'alias'
		put: aString
! !
!CypressAbstractRepository methodsFor: 'accessing properties'!
copyrightProperty

	^properties 
		at: '_cypress_copyright'
		ifAbsent: ['']
! !
!CypressAbstractRepository methodsFor: 'accessing properties'!
copyrightProperty: aString

	properties 
		at: '_cypress_copyright'
		put: aString
! !
!CypressAbstractRepository methodsFor: 'accessing'!
defaultCopyrightNotice

	^self class defaultCopyrightNotice! !
!CypressAbstractRepository methodsFor: 'accessing'!
description

	^self alias! !
!CypressAbstractRepository methodsFor: 'initializing - private'!
initialize

	self initializeDefaultRepositoryProperties.
	readerClass := CypressPackageReader.
	writerClass := CypressPackageWriter.! !
!CypressAbstractRepository methodsFor: 'initializing - private'!
initializeDefaultCopyrightProperty

	self copyrightProperty: self defaultCopyrightNotice
! !
!CypressAbstractRepository methodsFor: 'initializing - private'!
initializeDefaultRepositoryProperties

	properties := Dictionary new.
	self initializeDefaultCopyrightProperty
! !
!CypressAbstractRepository methodsFor: 'initializing - private'!
initializeUrl: aUrl andAlias: aString

	self
		initialize;
		url: aUrl;
		alias: aString;
		validateUrl
! !
!CypressAbstractRepository methodsFor: 'accessing properties'!
packageExtension

	^properties 
		at: 'packageExtension'
		ifAbsent: ['.package']
! !
!CypressAbstractRepository methodsFor: 'printing'!
printDetailsOn: aStream

	aStream nextPutAll: self alias! !
!CypressAbstractRepository methodsFor: 'printing'!
printOn: aStream

	aStream
		nextPutAll: self class name;
		nextPutAll: '('.
	self printDetailsOn: aStream.
	aStream nextPutAll: ')'! !
!CypressAbstractRepository methodsFor: 'reading'!
reader

	^readerClass forRepository: self! !
!CypressAbstractRepository methodsFor: 'reading'!
readPackageStructureForPackageNamed: packageName

	^(self reader)
		readPackageStructureForPackageNamed: packageName;
		packageStructure! !
!CypressAbstractRepository methodsFor: 'accessing'!
url

	^url
! !
!CypressAbstractRepository methodsFor: 'accessing'!
url: aString

	url := aString
! !
!CypressAbstractRepository methodsFor: 'validating - private'!
validateUrl
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."! !
!CypressAbstractRepository methodsFor: 'writing'!
writePackageStructure: aPackageStructure

	^self writer writePackageStructure: aPackageStructure! !
!CypressAbstractRepository methodsFor: 'writing'!
writer

	^writerClass forRepository: self! !
" Class Implementation for CypressFileSystemRepository"!

" ------------------- Class methods for CypressFileSystemRepository"!

!CypressFileSystemRepository class methodsFor: 'instance creation'!
on: aDirectory

	^self new
		initializeForDirectory: aDirectory;
		yourself.
! !
" ------------------- Instance methods for CypressFileSystemRepository"!

!CypressFileSystemRepository methodsFor: 'accessing - properties'!
codeFormatProperty

	^properties 
		at: '_gs_format'
		ifAbsent: ['Cypress']
! !
!CypressFileSystemRepository methodsFor: 'updating properties'!
codeFormatProperty: aString

	self validate: aString isOneOf: #('Cypress' 'FileTree' 'Flexible').
	properties 
		at: '_gs_format'
		put: aString
! !
!CypressFileSystemRepository methodsFor: 'accessing'!
description

	| desc |
	desc := super description.
	^desc notEmpty
		ifTrue: [desc]
		ifFalse: [self directoryPath]! !
!CypressFileSystemRepository methodsFor: 'accessing'!
directoryPath

	^directoryPath! !
!CypressFileSystemRepository methodsFor: 'initializing - private'!
directoryPath:  aString

	directoryPath := aString! !
!CypressFileSystemRepository methodsFor: 'testing - private'!
doesRepositoryFileExist: fileName
	"Answer whether the named file exists at the repository level."

	^self fileUtils
		directoryExists: (self fileUtils
				directoryFromPath: fileName
				relativeTo: self directoryPath)! !
!CypressFileSystemRepository methodsFor: 'initializing - private'!
ensureDirectoryPathExists

	self fileUtils ensureDirectoryExists: self directoryPath! !
!CypressFileSystemRepository methodsFor: 'accessing - private'!
fileUtils

	^CypressFileUtilities current
! !
!CypressFileSystemRepository methodsFor: 'initializing - private'!
fixupMissingCopyrightProperty

	self copyrightProperty isEmpty ifFalse: [^self].
	self initializeDefaultCopyrightProperty.
! !
!CypressFileSystemRepository methodsFor: 'initializing - private'!
initializeCreationOn: aUrl alias: aString

	self
		initializeUrl: aUrl andAlias: aString;
		alias: aString;
		writePropertiesFile! !
!CypressFileSystemRepository methodsFor: 'initializing - private'!
initializeDefaultRepositoryProperties

	super initializeDefaultRepositoryProperties.
	self
		codeFormatProperty: 'Cypress';
		strictCodeFormat: false.
! !
!CypressFileSystemRepository methodsFor: 'initializing - private'!
initializeForDirectory: aDirectory

	self initialize.
	self directoryPath: aDirectory.
	self directoryPath isEmpty ifTrue: [^self].	"Not really valid; not a very good idea."
	self ensureDirectoryPathExists.
	self readPropertiesFile.
	self fixupMissingCopyrightProperty.
	self initializeReaderAndWriterClasses.
! !
!CypressFileSystemRepository methodsFor: 'initializing - private'!
initializeForFileTreeRepository

	self initializeDefaultRepositoryProperties.
	self
		alias: 'FileTree read-only repository on ', self directoryPath;
		codeFormatProperty: 'FileTree';
		strictCodeFormat: true.
! !
!CypressFileSystemRepository methodsFor: 'initializing - private'!
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
					writerClass := CypressFileTreeFormatPackageWriter]]! !
!CypressFileSystemRepository methodsFor: 'initializing - private'!
initializeUrl: aUrl andAlias: aString

	super initializeUrl: aUrl andAlias: aString.
	self directoryPath: self url pathForDirectory.
	self ensureDirectoryPathExists.
	self readPropertiesFile.
	self codeFormatProperty: self url codeFormat.
	self strictCodeFormat: self url isStrict.
	self fixupMissingCopyrightProperty.
	self initializeReaderAndWriterClasses.
! !
!CypressFileSystemRepository methodsFor: 'testing properties'!
isCodeFormatCypress

	^self isCodeFormatProperty: 'Cypress'
! !
!CypressFileSystemRepository methodsFor: 'testing properties'!
isCodeFormatFileTree

	^self isCodeFormatProperty: 'FileTree'
! !
!CypressFileSystemRepository methodsFor: 'testing properties'!
isCodeFormatFlexiblyCypress

	^self isCodeFormatStrict not and: [self isCodeFormatCypress]
! !
!CypressFileSystemRepository methodsFor: 'testing properties'!
isCodeFormatFlexiblyFileTree

	^self isCodeFormatStrict not and: [self isCodeFormatFileTree]
! !
!CypressFileSystemRepository methodsFor: 'testing properties - private'!
isCodeFormatProperty: aString

	^(properties at: '_gs_format') equalsNoCase: aString
! !
!CypressFileSystemRepository methodsFor: 'testing properties'!
isCodeFormatStrict

	^(properties 
		at: '_gs_strict'
		ifAbsent: ['']) equalsNoCase: 'true'
! !
!CypressFileSystemRepository methodsFor: 'testing properties'!
isCodeFormatStrictlyCypress

	^self isCodeFormatStrict and: [self isCodeFormatCypress]
! !
!CypressFileSystemRepository methodsFor: 'testing properties'!
isCodeFormatStrictlyFileTree

	^self isCodeFormatStrict and: [self isCodeFormatFileTree]
! !
!CypressFileSystemRepository methodsFor: 'accessing'!
packageNames

	^(self fileUtils directoryEntriesFrom: self directoryPath , '*', self packageExtension)
		collect: [:each | (self fileUtils localNameFrom: each) copyWithoutSuffix: self packageExtension]! !
!CypressFileSystemRepository methodsFor: 'printing'!
printDetailsOn: aStream

	self alias notEmpty
		ifTrue: 
			[aStream
				nextPutAll: self alias;
				nextPutAll: ': '].
	aStream nextPutAll: self url printString! !
!CypressFileSystemRepository methodsFor: 'reading'!
readPropertiesFile

	self readPropertiesFile: (#('properties.ston' 'properties.json' '.filetree')
				detect: [:each | self doesRepositoryFileExist: each]
				ifNone: [^self]).
! !
!CypressFileSystemRepository methodsFor: 'reading'!
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
		do: [:fileStream | properties := CypressJsonParser parseStream: fileStream]! !
!CypressFileSystemRepository methodsFor: 'updating properties'!
strictCodeFormat: aBoolean

	self strictCodeFormatProperty: aBoolean printString

! !
!CypressFileSystemRepository methodsFor: 'updating properties - private'!
strictCodeFormatProperty: aString

	self validate: aString isOneOf: #('true' 'false').
	properties 
		at: '_gs_strict'
		put: aString
! !
!CypressFileSystemRepository methodsFor: 'updating properties - private'!
validate: aString isOneOf: someStrings

	someStrings
		detect: [:each | aString equalsNoCase: each]
		ifNone: [self error: aString printString, ' must be one of ', someStrings printString].
! !
!CypressFileSystemRepository methodsFor: 'validating - private'!
validateUrl
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

	self url fileName isEmpty
		ifFalse: [self error: self printString, ' should not be used with URLs for file names (', self url fileName, ' in ', self url pathForDirectory, ')']! !
!CypressFileSystemRepository methodsFor: 'writing'!
writePropertiesFile

	self writePropertiesFile: (self isCodeFormatFileTree
				ifTrue: ['properties.json']
				ifFalse: ['properties.ston'])! !
!CypressFileSystemRepository methodsFor: 'writing'!
writePropertiesFile: fileName

	self fileUtils
		writeStreamFor: fileName
		in: self directoryPath
		do: [:fileStream | properties writeCypressJsonOn: fileStream]! !
" Class Implementation for CypressAbstractFileoutRepository"!

" ------------------- Class methods for CypressAbstractFileoutRepository"!

!CypressAbstractFileoutRepository class methodsFor: 'instance creation'!
on: aDirectory

	^self new
		initializeForDirectory: aDirectory;
		yourself.
! !
" ------------------- Instance methods for CypressAbstractFileoutRepository"!

!CypressAbstractFileoutRepository methodsFor: 'accessing'!
description

	| desc |
	desc := super description.
	^desc notEmpty
		ifTrue: [desc]
		ifFalse: [self directoryPath]! !
!CypressAbstractFileoutRepository methodsFor: 'accessing'!
directoryPath

	^directoryPath! !
!CypressAbstractFileoutRepository methodsFor: 'initializing - private'!
directoryPath:  aString

	directoryPath := aString! !
!CypressAbstractFileoutRepository methodsFor: 'initializing - private'!
ensureDirectoryPathExists

	self fileUtils ensureDirectoryExists: self directoryPath! !
!CypressAbstractFileoutRepository methodsFor: 'accessing - private'!
fileUtils

	^CypressFileUtilities current
! !
!CypressAbstractFileoutRepository methodsFor: 'initializing - private'!
initializeCreationOn: aUrl alias: aString

	self
		initializeUrl: aUrl andAlias: aString;
		alias: aString! !
!CypressAbstractFileoutRepository methodsFor: 'initializing - private'!
initializeForDirectory: aDirectory

	self initialize.
	self directoryPath: aDirectory.
	self directoryPath isEmpty ifTrue: [^self].	"Not really valid; not a very good idea."
	self ensureDirectoryPathExists.
	self initializeReaderAndWriterClasses.
! !
!CypressAbstractFileoutRepository methodsFor: 'initializing - private'!
initializeReaderAndWriterClasses

	self suubclassResponsibility: #initializeReaderAndWriterClasses! !
!CypressAbstractFileoutRepository methodsFor: 'initializing - private'!
initializeUrl: aUrl andAlias: aString

	super initializeUrl: aUrl andAlias: aString.
	self directoryPath: self url pathForDirectory.
	self ensureDirectoryPathExists.
	self initializeReaderAndWriterClasses.
! !
!CypressAbstractFileoutRepository methodsFor: 'printing'!
printDetailsOn: aStream

	self alias notEmpty
		ifTrue: 
			[aStream
				nextPutAll: self alias;
				nextPutAll: ': '].
	aStream nextPutAll: self url printString
! !
!CypressAbstractFileoutRepository methodsFor: 'validating - private'!
validateUrl
	"At this level, there is nothing to check.
	 But different URLs denote different kinds of repositories, and
	 each kind of repository may have specific checks."

	self url fileName isEmpty
		ifFalse: [self error: self printString, ' should not be used with URLs for file names (', self url fileName, ' in ', self url pathForDirectory, ')']! !
" Class Implementation for CypressTopazRepository"!

" ------------------- Instance methods for CypressTopazRepository"!

!CypressTopazRepository methodsFor: 'initializing - private'!
initializeReaderAndWriterClasses

	readerClass := CypressDoNothingPackageReader.
	writerClass := CypressTopazFileoutWriter.! !
" Class Implementation for CypressSmalltalkRepository"!

" ------------------- Instance methods for CypressSmalltalkRepository"!

!CypressSmalltalkRepository methodsFor: 'initializing - private'!
initializeReaderAndWriterClasses

	readerClass := CypressDoNothingPackageReader.
	writerClass := CypressSmalltalkFileoutWriter.
! !
" Class Implementation for CypressPackageManager2"!

" ------------------- Class methods for CypressPackageManager2"!

!CypressPackageManager2 class methodsFor: 'Instance Creation'!
create

	^self new
		initializeFromImage;
		yourself.
! !
!CypressPackageManager2 class methodsFor: 'Initializing'!
initialize

	self savedPackageManagers: IdentityDictionary new! !
!CypressPackageManager2 class methodsFor: 'Accessing'!
named: aKey
	"Answer the Package Manager previously saved under aKey.
	 It is an error if there was not one saved under that key."

	^self
		named: aKey
		or: [self error: 'No previously saved Package Manager under the key ', aKey printString]! !
!CypressPackageManager2 class methodsFor: 'Accessing'!
named: aKey or: aBlock
	"Answer the Package Manager previously saved under aKey.
	 Answer the result of evaluating aBlock, if there was not one saved under that key."

	^self savedPackageManagers at: aKey ifAbsent: aBlock! !
!CypressPackageManager2 class methodsFor: 'Instance Creation'!
new

	^super new
		initialize;
		yourself.
! !
!CypressPackageManager2 class methodsFor: 'Accessing - private'!
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
	^names! !
!CypressPackageManager2 class methodsFor: 'Accessing - private'!
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
		sortAscending! !
!CypressPackageManager2 class methodsFor: 'Updating'!
removePackageManagerSavedAs: aKey
	"Remove the Package Manager previously saved under aKey, if there was one.
	 Answer it or nil if there was not one saved under that key."

	^self savedPackageManagers removeKey: aKey ifAbsent: []! !
!CypressPackageManager2 class methodsFor: 'Accessing'!
savedPackageManagers

	^SavedPackageManagers! !
!CypressPackageManager2 class methodsFor: 'Initializing - private'!
savedPackageManagers: anIdentityDictionary

	SavedPackageManagers := anIdentityDictionary! !
" ------------------- Instance methods for CypressPackageManager2"!

!CypressPackageManager2 methodsFor: 'Updating'!
addRepository: aRepository to: aKnownPackageInformation

	aKnownPackageInformation addRepository: aRepository.! !
!CypressPackageManager2 methodsFor: 'Updating'!
addUnknownPackageNamed: aString

	self packageInformationList
		at: aString
		put: (CypressUnknownPackageInformation named: aString).
! !
!CypressPackageManager2 methodsFor: 'Updating'!
assignRepository: aRepository to: aPackageInformation

	self assignRepository: aRepository toAll: (Array with: aPackageInformation)! !
!CypressPackageManager2 methodsFor: 'Updating'!
assignRepository: aRepository toAll: somePackageInformations
	"Assign to those having no repository information and add to those with.
	 Those without a repository need to be converted to Known Package Information instances."

	self knownRepositories at: aRepository url put: aRepository.
	(somePackageInformations reject: [:each | each isKnown])
		do: [:each | self addRepository: aRepository to: (self convertToKnown: each)].
	(somePackageInformations select: [:each | each isKnown])
		do: [:each | self addRepository: aRepository to: each].
! !
!CypressPackageManager2 methodsFor: 'Updating'!
convert: anUnknownPackageInformation toConflictingWith: aKnownPackageInformation

	| conflicting |
	conflicting := CypressConflictingPackageInformation
				fromUnknown: anUnknownPackageInformation
				conflictingWith: aKnownPackageInformation.
	self replace: anUnknownPackageInformation with: conflicting.
	^conflicting! !
!CypressPackageManager2 methodsFor: 'Updating'!
convert: anUnknownPackageInformation toEclipsedBy: aKnownPackageInformation

	| eclipsed |
	eclipsed := CypressEclipsedPackageInformation
				fromUnknown: anUnknownPackageInformation
				eclipsedBy: aKnownPackageInformation.
	self replace: anUnknownPackageInformation with: eclipsed.
	^eclipsed! !
!CypressPackageManager2 methodsFor: 'Updating'!
convertToKnown: aPackageInformation

	| known |
	known := CypressKnownPackageInformation fromUnknown: aPackageInformation.
	self replace: aPackageInformation with: known.
	(self findPackagesEclipsedBy: known) do: [:each | self convert: each toEclipsedBy: known].
	(self findPackagesConflictingWith: known) do: [:each | self convert: each toConflictingWith: known].
	^known.
! !
!CypressPackageManager2 methodsFor: 'Updating'!
createRepositoryNamed: aName under: aDirectory alias: aString schema: schemaName

	^CypressFileSystemRepository
		createOn: (schemaName
				, (CypressFileUtilities current
					directoryFromPath: aName
						relativeTo: aDirectory)
				, '/') asUrl
		alias: aString! !
!CypressPackageManager2 methodsFor: 'Accessing - private'!
findPackagesConflictingWith: aKnownPackageInformation

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name
				and: [aKnownPackageInformation name beginsWith: each name , '-']]
! !
!CypressPackageManager2 methodsFor: 'Accessing - private'!
findPackagesEclipsedBy: aKnownPackageInformation

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name
				and: [each name beginsWith: aKnownPackageInformation name , '-']]! !
!CypressPackageManager2 methodsFor: 'Initializing - private'!
initialize

	self
		knownRepositories: Dictionary new;
		packageInformationList: Dictionary new! !
!CypressPackageManager2 methodsFor: 'Initializing - private'!
initializeFromImage

	self initializePackageInformationList.
! !
!CypressPackageManager2 methodsFor: 'Initializing - private'!
initializePackageInformationList

	self
		packageInformationList: (self potentialPackageNames
				inject: Dictionary new
				into: 
					[:dict :each |
					dict
						at: each put: (CypressUnknownPackageInformation named: each);
						yourself])! !
!CypressPackageManager2 methodsFor: 'Accessing'!
knownRepositories

	^knownRepositories! !
!CypressPackageManager2 methodsFor: 'Updating'!
knownRepositories: someNamedRepositories

	knownRepositories := someNamedRepositories! !
!CypressPackageManager2 methodsFor: 'Loading'!
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
! !
!CypressPackageManager2 methodsFor: 'Updating'!
lookForPackagesInRepository: aRepository
	"Find the packages in the repository and update the list
	 of package information accordingly."

	| packageNames inImage exImage |
	packageNames := aRepository packageNames.
	inImage := self packageInformationList asArray select: [:each | packageNames includes: each name].
	exImage := packageNames reject: [:each | self packageInformationList anySatisfy: [:info | info name = each]].
	exImage := exImage collect: [:each | CypressUnknownPackageInformation named: each].

	self assignRepository: aRepository toAll: inImage, exImage! !
!CypressPackageManager2 methodsFor: 'Accessing'!
packageInformationList

	^packageInformationList! !
!CypressPackageManager2 methodsFor: 'Updating'!
packageInformationList: someNamedPackageInformations

	packageInformationList := someNamedPackageInformations! !
!CypressPackageManager2 methodsFor: 'Accessing'!
packageInformationNamed: aString

	^self packageInformationNamed: aString
		or: [self error: 'No package information for ' , aString printString]! !
!CypressPackageManager2 methodsFor: 'Accessing'!
packageInformationNamed: aString or: aBlock

	^self packageInformationList
		at: aString
		ifAbsent: aBlock.
! !
!CypressPackageManager2 methodsFor: 'Accessing - private'!
potentialPackageNames

	^self class potentialPackageNames! !
!CypressPackageManager2 methodsFor: 'Updating'!
replace: oldPackageInformation with: newPackageInformation

	self packageInformationList
		at: oldPackageInformation name
		put: newPackageInformation.
! !
!CypressPackageManager2 methodsFor: 'Initializing - private'!
repositoryOn: url alias: aString
	"Answer a repository instance for the specified URL.
	 The characteristics will come from the properties file at the URL location,
	 or will default according to the schema, if there is no properties file."

	^self knownRepositories
		at: url
		ifAbsentPut: [CypressAbstractRepository onUrl: url alias: aString].
! !
!CypressPackageManager2 methodsFor: 'Updating'!
saveAs: aKey
	"Save the receiver in the class' collection of named managers
	 under the specified key.
	 It will quietly replace anything already under that key."


	self savedPackageManagers at: aKey put: self.
! !
!CypressPackageManager2 methodsFor: 'Accessing - private'!
savedPackageManagers

	^self class savedPackageManagers! !
!CypressPackageManager2 methodsFor: 'Writing'!
writeChangesToAllRepositoriesFor: aPackageInformation

	aPackageInformation writeChangesToAllRepositories.! !
" Class Implementation for CypressAbstractFileUrl"!

" ------------------- Class methods for CypressAbstractFileUrl"!

!CypressAbstractFileUrl class methodsFor: 'instance creation'!
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
	^newUrl! !
!CypressAbstractFileUrl class methodsFor: 'parsing'!
urlClassForScheme: scheme

	scheme isNil ifTrue: [^FileUrl].
	^super urlClassForScheme: scheme
! !
" ------------------- Instance methods for CypressAbstractFileUrl"!

!CypressAbstractFileUrl methodsFor: 'accessing'!
codeFormat

	^self subclassResponsibility: #codeFormat.
! !
!CypressAbstractFileUrl methodsFor: 'testing'!
isStrict

	^self subclassResponsibility: #isStrict.
! !
!CypressAbstractFileUrl methodsFor: 'accessing'!
repositoryClass

	^CypressFileSystemRepository! !
" Class Implementation for CypressLaxFileUrl"!

" ------------------- Class methods for CypressLaxFileUrl"!

!CypressLaxFileUrl class methodsFor: 'constants'!
schemeName

	^'cypresslax'! !
" ------------------- Instance methods for CypressLaxFileUrl"!

!CypressLaxFileUrl methodsFor: 'accessing'!
codeFormat

	^'Cypress'! !
!CypressLaxFileUrl methodsFor: 'testing'!
isStrict

	^false! !
" Class Implementation for CypressFileTreeFormatFileUrl"!

" ------------------- Class methods for CypressFileTreeFormatFileUrl"!

!CypressFileTreeFormatFileUrl class methodsFor: 'constants'!
schemeName

	^'cypressft'! !
" ------------------- Instance methods for CypressFileTreeFormatFileUrl"!

!CypressFileTreeFormatFileUrl methodsFor: 'accessing'!
codeFormat

	^'FileTree'! !
!CypressFileTreeFormatFileUrl methodsFor: 'testing'!
isStrict

	^false! !
" Class Implementation for CypressFileUrl"!

" ------------------- Class methods for CypressFileUrl"!

!CypressFileUrl class methodsFor: 'constants'!
schemeName

	^'cypress'! !
" ------------------- Instance methods for CypressFileUrl"!

!CypressFileUrl methodsFor: 'accessing'!
codeFormat

	^'Cypress'! !
!CypressFileUrl methodsFor: 'testing'!
isStrict

	^true! !
" Class Implementation for CypressFileTreeReadOnlyFileUrl"!

" ------------------- Class methods for CypressFileTreeReadOnlyFileUrl"!

!CypressFileTreeReadOnlyFileUrl class methodsFor: 'constants'!
schemeName

	^'cypressfiletree'! !
" ------------------- Instance methods for CypressFileTreeReadOnlyFileUrl"!

!CypressFileTreeReadOnlyFileUrl methodsFor: 'accessing'!
codeFormat

	^'FileTree'! !
!CypressFileTreeReadOnlyFileUrl methodsFor: 'testing'!
isStrict

	^true! !
" Class Implementation for CypressTopazUrl"!

" ------------------- Class methods for CypressTopazUrl"!

!CypressTopazUrl class methodsFor: 'constants'!
schemeName

	^'topaz'! !
" ------------------- Instance methods for CypressTopazUrl"!

!CypressTopazUrl methodsFor: 'accessing'!
codeFormat

	^'Topaz'! !
!CypressTopazUrl methodsFor: 'testing'!
isStrict

	^true! !
!CypressTopazUrl methodsFor: 'accessing'!
repositoryClass

	^CypressTopazRepository! !
" Class Implementation for CypressSmalltalkUrl"!

" ------------------- Class methods for CypressSmalltalkUrl"!

!CypressSmalltalkUrl class methodsFor: 'constants'!
schemeName

	^'chunk'
! !
" ------------------- Instance methods for CypressSmalltalkUrl"!

!CypressSmalltalkUrl methodsFor: 'accessing'!
codeFormat

	^'Chunk'
! !
!CypressSmalltalkUrl methodsFor: 'testing'!
isStrict

	^true
! !
!CypressSmalltalkUrl methodsFor: 'accessing'!
repositoryClass

	^CypressSmalltalkRepository
! !
" Class Implementation for CypressPackageInformation"!

" ------------------- Class methods for CypressPackageInformation"!

!CypressPackageInformation class methodsFor: 'instance creation'!
named: aString repository: aCypressRepository
	"Answer an instance of the receiver representing the named package.
	 If the package was saved in a Repository, load up the saved details."

	^self new
		initializeFromName: aString andRepository: aCypressRepository;
		yourself
! !
!CypressPackageInformation class methodsFor: 'instance creation'!
new

	^super new
		initialize;
		yourself
! !
" ------------------- Instance methods for CypressPackageInformation"!

!CypressPackageInformation methodsFor: 'updating - type'!
beConflictedWith: somePackageNames
	"Be designated as representing the prefix of one or more Known Package names."

	type := 'Conflicted Name'.
	competingPackageNames := somePackageNames sortAscending.
	advice := 'Conflicts with the packages named ', self competingPackageNamesString! !
!CypressPackageInformation methodsFor: 'updating - type'!
beKnown
	"Be known to represent a real package."

	type := 'Known Package'.
	advice := ''.
	competingPackageNames := #()
! !
!CypressPackageInformation methodsFor: 'updating - type'!
beQualifiedNameOf: somePackageNames
	"Be designated as qualifying a Known Package name and therefore not eligible as a package name."

	type := 'Qualified Name'.
	competingPackageNames := somePackageNames sortAscending.
	advice := 'Qualifies the package named ', self competingPackageNamesString
! !
!CypressPackageInformation methodsFor: 'updating - type'!
beUnknown
	"Be designated as possibly representing a package, but not known to do so."

	type := 'Unknown'.
	advice := ''.
	competingPackageNames := #()! !
!CypressPackageInformation methodsFor: 'accessing'!
changesCount

	^changesCount! !
!CypressPackageInformation methodsFor: 'accessing'!
changesCount: anInteger

	changesCount := anInteger! !
!CypressPackageInformation methodsFor: 'accessing'!
changesStatus

	^self hasChanges
		ifTrue: [' (' , self changesCount printString , ')']
		ifFalse: ['']! !
!CypressPackageInformation methodsFor: 'accessing'!
classCount

	^self imageCounts first! !
!CypressPackageInformation methodsFor: 'accessing'!
competingPackageNames

	^competingPackageNames! !
!CypressPackageInformation methodsFor: 'accessing'!
competingPackageNamesString

	^String streamContents: 
			[:stream |
			self competingPackageNames
				do: [:each | stream nextPutAll: each printString]
				separatedBy: [stream nextPutAll: ', ']]! !
!CypressPackageInformation methodsFor: 'accessing'!
description

	self isKnown ifTrue: [^self savedLocation].
	self isUnknown ifTrue: [^' <unknown>'].
	^' <', advice, '>'! !
!CypressPackageInformation methodsFor: 'accessing'!
determinedChangesCount

	| notInImage notInSaved |
	notInImage := self savedDefinitions
				reject: [:each | self imageDefinitions includes: each].
	notInSaved := self imageDefinitions
		reject: [:each | self savedDefinitions includes: each].
	^notInImage size + notInSaved size
! !
!CypressPackageInformation methodsFor: 'testing'!
hasChanges

	^self changesCount > 0! !
!CypressPackageInformation methodsFor: 'accessing'!
imageCounts

	^imageCounts! !
!CypressPackageInformation methodsFor: 'accessing'!
imageCounts: someIntegers
	"A pair: the number of classes and number of methods"

	imageCounts := someIntegers! !
!CypressPackageInformation methodsFor: 'accessing'!
imageDefinitionCounts

	| classCount methodCount |
	classCount := methodCount := 0.
	self imageDefinitions do: 
			[:each |
			each classDefinition: [:classDefinition | classCount := classCount + 1]
				methodDefinition: [:methodDefinition | methodCount := methodCount + 1]].
	^Array with: classCount with: methodCount
! !
!CypressPackageInformation methodsFor: 'accessing'!
imageDefinitions

	^imageDefinitions
! !
!CypressPackageInformation methodsFor: 'accessing'!
imageDefinitions: someCypressDefinitions

	imageDefinitions := someCypressDefinitions
! !
!CypressPackageInformation methodsFor: 'accessing'!
imageDefinitionsStatus

	^self classCount printString , '/' , self methodCount printString
! !
!CypressPackageInformation methodsFor: 'initializing'!
initialize

	self
		beUnknown;
		name: '';
		imageDefinitions: #();
		savedDefinitions: #();
		savedLocation: '';
		repositoryDescription: '';
		imageCounts: #(0 0);
		changesCount: 0! !
!CypressPackageInformation methodsFor: 'initializing'!
initializeFromName: aString andRepository: aCypressRepositoryOrNil

	self name: aString.
	aCypressRepositoryOrNil isNil ifTrue: [^self].
	self updateKnownPackageRepository: aCypressRepositoryOrNil! !
!CypressPackageInformation methodsFor: 'testing - type'!
isConflicted

	^type = 'Conflicted Name'! !
!CypressPackageInformation methodsFor: 'testing - type'!
isKnown

	^type = 'Known Package'! !
!CypressPackageInformation methodsFor: 'testing - type'!
isQualifiedName

	^type = 'Qualified Name'! !
!CypressPackageInformation methodsFor: 'testing - type'!
isUnknown

	^type = 'Unknown'! !
!CypressPackageInformation methodsFor: 'accessing'!
methodCount

	^self imageCounts last! !
!CypressPackageInformation methodsFor: 'accessing'!
name

	^name
! !
!CypressPackageInformation methodsFor: 'accessing'!
name: aString

	name := aString
! !
!CypressPackageInformation methodsFor: 'printing'!
printDetailsOn: aStream

	aStream
		nextPutAll: self name;
		nextPutAll: ' - ';
		nextPutAll: self description! !
!CypressPackageInformation methodsFor: 'updating'!
readDefinitionsFromRepository

	^(self repository reader readPackageStructureForPackageNamed: self name)
		packageStructure snapshot
		definitions! !
!CypressPackageInformation methodsFor: 'updating'!
refresh

	self isKnown ifFalse: [^self].
	self
		updateImageDefinitions;
		updateSavedDefinitions;
		updateChangesCount.
! !
!CypressPackageInformation methodsFor: 'accessing'!
repository

	^repository! !
!CypressPackageInformation methodsFor: 'accessing'!
repository: aCypressFileSystemRepository

	repository := aCypressFileSystemRepository! !
!CypressPackageInformation methodsFor: 'unknown'!
repositoryDescription

	^repositoryDescription! !
!CypressPackageInformation methodsFor: 'unknown'!
repositoryDescription: aString

	repositoryDescription := aString! !
!CypressPackageInformation methodsFor: 'accessing'!
savedDefinitions

	^savedDefinitions
! !
!CypressPackageInformation methodsFor: 'accessing'!
savedDefinitions: someCypressDefinitions

	savedDefinitions := someCypressDefinitions
! !
!CypressPackageInformation methodsFor: 'accessing'!
savedLocation

	^savedLocation! !
!CypressPackageInformation methodsFor: 'accessing'!
savedLocation: aDirectory

	savedLocation := aDirectory! !
!CypressPackageInformation methodsFor: 'accessing'!
status

	| changes |
	(changes := self changesStatus) isEmpty ifTrue: [^self imageDefinitionsStatus].
	^self imageDefinitionsStatus, changes! !
!CypressPackageInformation methodsFor: 'updating'!
updateChangesCount
	"Must be applied after the image definitions and saved definitions are updated."

	self changesCount: self determinedChangesCount! !
!CypressPackageInformation methodsFor: 'updating'!
updateImageDefinitions

	self
		imageDefinitions: (CypressPackageDefinition named: self name) snapshot
					definitions;
		imageCounts: self imageDefinitionCounts! !
!CypressPackageInformation methodsFor: 'updating'!
updateKnownPackageRepository: aCypressRepository
	"Update the receiver to reflect it being a known package."

	self
		beKnown;
		updateRepository: aCypressRepository;
		refresh.
! !
!CypressPackageInformation methodsFor: 'updating'!
updateRepository: aCypressRepository

	self
		repository: aCypressRepository;
		repositoryDescription: self repository description;
		savedLocation: self repository directoryPath! !
!CypressPackageInformation methodsFor: 'updating'!
updateSavedDefinitions

	self savedDefinitions: self readDefinitionsFromRepository! !
" Class Implementation for CypressAbstractPackageInformation"!

" ------------------- Class methods for CypressAbstractPackageInformation"!

!CypressAbstractPackageInformation class methodsFor: 'Instance Creation'!
named: aString

	^self new
		initializeWithName: aString;
		yourself! !
" ------------------- Instance methods for CypressAbstractPackageInformation"!

!CypressAbstractPackageInformation methodsFor: 'Initializing - private'!
initialize! !
!CypressAbstractPackageInformation methodsFor: 'Initializing - private'!
initializeWithName: aString

	self initialize.
	self name: aString! !
!CypressAbstractPackageInformation methodsFor: 'Testing'!
isKnown

	^false
! !
!CypressAbstractPackageInformation methodsFor: 'Accessing'!
name

	^name! !
!CypressAbstractPackageInformation methodsFor: 'Updating'!
name: aString

	name := aString! !
!CypressAbstractPackageInformation methodsFor: 'Printing'!
printDetailsOn: aStream

	aStream nextPutAll: self name! !
!CypressAbstractPackageInformation methodsFor: 'Accessing'!
repositories

	^#()! !
" Class Implementation for CypressEclipsedPackageInformation"!

" ------------------- Class methods for CypressEclipsedPackageInformation"!

!CypressEclipsedPackageInformation class methodsFor: 'Instance Creation'!
fromUnknown: unknownPackageInformation eclipsedBy: knownPackageInformation

	^(self named: unknownPackageInformation name)
		eclipsedBy: knownPackageInformation;
		yourself.
! !
" ------------------- Instance methods for CypressEclipsedPackageInformation"!

!CypressEclipsedPackageInformation methodsFor: 'Accessing'!
eclipsedBy

	^eclipsedBy! !
!CypressEclipsedPackageInformation methodsFor: 'Updating'!
eclipsedBy: aPackageInformation

	eclipsedBy := aPackageInformation! !
" Class Implementation for CypressUnknownPackageInformation"!

" Class Implementation for CypressKnownPackageInformation"!

" ------------------- Class methods for CypressKnownPackageInformation"!

!CypressKnownPackageInformation class methodsFor: 'Instance Creation'!
fromUnknown: aPackageInformation

	^self named: aPackageInformation name! !
" ------------------- Instance methods for CypressKnownPackageInformation"!

!CypressKnownPackageInformation methodsFor: 'Updating'!
addRepository: aRepository

	self repositories at: aRepository url put: aRepository.
	self updateDigestsFromImageAndRepository: aRepository.! !
!CypressKnownPackageInformation methodsFor: 'Accessing - digests'!
determineDigestFromImage

	^self packageStructure digest! !
!CypressKnownPackageInformation methodsFor: 'Accessing - digests'!
determineDigestFromRepository: aRepository

	^ (aRepository readPackageStructureForPackageNamed: self name) digest.
! !
!CypressKnownPackageInformation methodsFor: 'Accessing - digests'!
digestFor: source

	^self digestFor: source or: [nil]! !
!CypressKnownPackageInformation methodsFor: 'Accessing - digests'!
digestFor: source or: aBlock

	^self digests at: source ifAbsent: aBlock! !
!CypressKnownPackageInformation methodsFor: 'Accessing - digests'!
digests

	^digests! !
!CypressKnownPackageInformation methodsFor: 'Initializing - private'!
digests: anIdentityDictionary
	"Key: Source of package definitions (#IMAGE or a Repository url)
	 Value: (e.g. MD5) Digest of the package's defintions or an empty string.
	 A nil digest means there are no definitions for the given source."

   digests := anIdentityDictionary
! !
!CypressKnownPackageInformation methodsFor: 'Accessing - digests'!
imageDigest

	^self digestFor: #IMAGE.
! !
!CypressKnownPackageInformation methodsFor: 'Initializing - private'!
initialize

	super initialize.
	self
		repositories: IdentityDictionary new;
		digests: IdentityDictionary new.
! !
!CypressKnownPackageInformation methodsFor: 'Testing'!
isKnown

	^true
! !
!CypressKnownPackageInformation methodsFor: 'Accessing - digests'!
packageStructure

	^CypressPackageStructure
		fromPackage: (CypressPackageDefinition named: self name)! !
!CypressKnownPackageInformation methodsFor: 'Updating'!
removeRepository: aRepository

	self repositories removeKey: aRepository url ifAbsent: [].
	self digests removeKey: aRepository url ifAbsent: [].
	self updateDigestsFromImage.
! !
!CypressKnownPackageInformation methodsFor: 'Accessing'!
repositories

	^repositories! !
!CypressKnownPackageInformation methodsFor: 'Initializing - private'!
repositories: anIdentityDictionary
	"Key: Repository url
	 Value: Repository"

	repositories := anIdentityDictionary! !
!CypressKnownPackageInformation methodsFor: 'Accessing - digests'!
repositoryDigests

	^self repositories collect: [:each | self digestFor: each url]! !
!CypressKnownPackageInformation methodsFor: 'Updating - digests'!
updateDigestsFromAllRepositories

	self repositories do: [:each | self updateDigestsFromRepository: each]! !
!CypressKnownPackageInformation methodsFor: 'Updating - digests'!
updateDigestsFromImage

	self digests at: #IMAGE put: self determineDigestFromImage! !
!CypressKnownPackageInformation methodsFor: 'Updating - digests'!
updateDigestsFromImageAndAllRepositories

	self
		updateDigestsFromImage;
		updateDigestsFromAllRepositories! !
!CypressKnownPackageInformation methodsFor: 'Updating - digests'!
updateDigestsFromImageAndRepository: aRepository

	self
		updateDigestsFromImage;
		updateDigestsFromRepository: aRepository! !
!CypressKnownPackageInformation methodsFor: 'Updating - digests'!
updateDigestsFromRepository: aRepository

	self digests at: aRepository url put: (self determineDigestFromRepository: aRepository).
! !
!CypressKnownPackageInformation methodsFor: 'Writing'!
writeChangesToAllRepositories

	| imageDigest changedDigests changedRepositories |
	self updateDigestsFromImageAndAllRepositories.
	imageDigest := self imageDigest.
	changedDigests := self repositoryDigests reject: [:each | each = imageDigest].
	changedRepositories := changedDigests keys collect: [:each | self repositories at: each].
	self writePackageToRepositories: changedRepositories.
! !
!CypressKnownPackageInformation methodsFor: 'Writing'!
writePackageToRepositories: someRepositories

	| packageStructure |
	packageStructure := self packageStructure.
	^someRepositories
		do: [:each | each writePackageStructure: packageStructure]! !
" Class Implementation for CypressConflictingPackageInformation"!

" ------------------- Class methods for CypressConflictingPackageInformation"!

!CypressConflictingPackageInformation class methodsFor: 'Instance Creation'!
fromUnknown: unknownPackageInformation conflictingWith: knownPackageInformation

	^(self named: unknownPackageInformation name)
		conflictsWith: knownPackageInformation;
		yourself.
! !
" ------------------- Instance methods for CypressConflictingPackageInformation"!

!CypressConflictingPackageInformation methodsFor: 'Accessing'!
conflictsWith

	^conflictsWith! !
!CypressConflictingPackageInformation methodsFor: 'Updating'!
conflictsWith: someCypressKnownPackageInformations

	conflictsWith := someCypressKnownPackageInformations! !
" Class Extensions "!

" Class initializers "!

CypressAbstractRepository initialize.!
CypressPackageManager2 initialize.!



" End of Package: Cypress-PackageManagement"!


