"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressPackageManager',
	#superclass : 'Object',
	#instVars : [
		'knownPackages',
		'knownRepositories',
		'packageInformationList'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'instance creation' }
CypressPackageManager class >> new [

	^super new
		initialize;
		yourself

]

{ #category : 'accessing' }
CypressPackageManager class >> packageNamePermutationsFor: aString [
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

]

{ #category : 'accessing' }
CypressPackageManager class >> potentialPackageNames [
	"Answer a list of 'package names' from classes and methods.
	 The class category is the package name, if the class is in a package at all.
	 The method category begins with an asterisk (*) before the package name,
	 but can be continued with other details (e.g., *PackageName-accessing).
	 This version does NOT recognize method category suffixes."

	| classCategories methodCategories |
	classCategories := Set new.
	methodCategories := Set new.
	System myUserProfile symbolList do: 
			[:dict |
			dict do: 
					[:aClass |
					aClass isBehavior and: 
							[classCategories addAll: (self packageNamePermutationsFor: aClass category).
							aClass categorysDo: 
									[:cat :method |
									cat first = $*
										ifTrue: 
											[methodCategories
												addAll: (self packageNamePermutationsFor: (cat copyFrom: 2 to: cat size))]].
							false]]].
	^(Set new)
		addAll: classCategories;
		addAll: methodCategories;
		removeIfPresent: 'User Classes';
		removeIfPresent: 'Kernel';
		sortAscending

]

{ #category : 'comparing' }
CypressPackageManager >> compareDefinitionsFromConflictedPackageInformation: aCypressPackageInformation [

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

]

{ #category : 'comparing' }
CypressPackageManager >> comparePackageFrom: aCypressPackageInformation [

	^self comparePackagesFrom: (Array with: aCypressPackageInformation)

]

{ #category : 'updating - private' }
CypressPackageManager >> determineKnownPackages [

	^(packageInformationList select: [:each | each repository notNil])
		inject: Dictionary new
		into: 
			[:dict :each |
			dict
				at: each name put: each savedLocation;
				yourself]

]

{ #category : 'initializing - private' }
CypressPackageManager >> initialize [

	self refreshPackageInformation.

]

{ #category : 'initializing - private' }
CypressPackageManager >> initializeConflictingPackageNames [

	| conflictingPackages |
	conflictingPackages := Dictionary new.
	packageInformationList do: 
			[:each |
			conflictingPackages at: each
				put: (knownPackages keys select: 
							[:knownName |
							knownName ~= each name
								and: [(knownName indexOfSubCollection: each name , '-' startingAt: 1 ifAbsent: [ 0 ]) = 1]])].
	conflictingPackages := conflictingPackages reject: [:each | each isEmpty].
	conflictingPackages
		keysAndValuesDo: [:package :conflicts | package beConflictedWith: conflicts]

]

{ #category : 'initializing - private' }
CypressPackageManager >> initializeKnownPackages [

	knownPackages := (System myUserProfile objectNamed: #KnownCypressPackages)
				ifNil: [Dictionary new]

]

{ #category : 'initializing - private' }
CypressPackageManager >> initializeKnownRepositories [

	knownRepositories := Dictionary new.
	knownPackages asSet
		do: [:each | self repositoryOn: each]

]

{ #category : 'initializing - private' }
CypressPackageManager >> initializePackageInformationList [

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

]

{ #category : 'initializing - private' }
CypressPackageManager >> initializeQualifiedPackageNames [

	| qualifiedPackages |
	qualifiedPackages := Dictionary new.
	packageInformationList do: 
			[:each |
			qualifiedPackages at: each
				put: (knownPackages keys select: 
							[:knownName |
							knownName ~= each name
								and: [(each name indexOfSubCollection: knownName , '-' startingAt: 1 ifAbsent: [ 0 ]) = 1]])].
	qualifiedPackages := qualifiedPackages reject: [:each | each isEmpty].
	qualifiedPackages
		keysAndValuesDo: [:package :baseNames | package beQualifiedNameOf: baseNames]

]

{ #category : 'updating' }
CypressPackageManager >> loadPackageFrom: aCypressPackageInformation [

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

]

{ #category : 'updating' }
CypressPackageManager >> lookForLoadedPackagesIn: aDirectory [
	"Update any of the packages in the image which have a Cypress file out in
	 the specified directory to reflect the path where the package has theoretically
	 been saved."

	self lookForLoadedPackagesInRepository: (self repositoryOn: aDirectory).
	^nil

]

{ #category : 'updating' }
CypressPackageManager >> lookForLoadedPackagesInRepository: aCypressRepository [
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

]

{ #category : 'updating' }
CypressPackageManager >> lookForUnloadedPackagesIn: aDirectory [
	"Load any package names from aDirectory as known packages.
	 This does not load the package contents."

	self lookForUnloadedPackagesInRepository: (self repositoryOn: aDirectory).
	^nil

]

{ #category : 'updating' }
CypressPackageManager >> lookForUnloadedPackagesInRepository: aCypressRepository [
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

]

{ #category : 'accessing' }
CypressPackageManager >> packageInformationList [

	^packageInformationList

]

{ #category : 'accessing' }
CypressPackageManager >> potentialPackageNames [

	^self class potentialPackageNames

]

{ #category : 'accessing' }
CypressPackageManager >> refreshedPackageInformationList [

	self refreshPackageInformation.
	^self packageInformationList.

]

{ #category : 'updating' }
CypressPackageManager >> refreshPackageInformation [

	self
		initializeKnownPackages;
		initializeKnownRepositories;
		initializePackageInformationList;
		initializeConflictingPackageNames;
		initializeQualifiedPackageNames

]

{ #category : 'initializing - private' }
CypressPackageManager >> repositoryOn: aDirectory [

	^knownRepositories
		at: aDirectory
		ifAbsentPut: [CypressFileSystemRepository on: aDirectory].

]

{ #category : 'updating - private' }
CypressPackageManager >> saveKnownPackages [

	self updateKnownPackages.
	((System myUserProfile resolveSymbol: #KnownCypressPackages)
		ifNil: 
			[(System myUserProfile objectNamed: #UserGlobals)
				addAssociation: #KnownCypressPackages -> Dictionary new])
			value: knownPackages

]

{ #category : 'updating - private' }
CypressPackageManager >> updateKnownPackages [

	knownPackages := self determineKnownPackages

]

{ #category : 'updating' }
CypressPackageManager >> updateSavedLocation: aDirectory for: aCypressPackageInformation [
	"Update the specified package to reflect the path and repository where the
	 package should be saved."

	aCypressPackageInformation
		updateKnownPackageRepository: (self repositoryOn: aDirectory).
	self saveKnownPackages.
	^nil

]

{ #category : 'writing - private' }
CypressPackageManager >> writeCypressPackageToDiskFrom: aCypressPackageInformation [

	| packageStructure |
	packageStructure := CypressPackageStructure
				fromPackage: (CypressPackageDefinition
						named: aCypressPackageInformation name).
	aCypressPackageInformation repository writer
		writePackageStructure: packageStructure

]

{ #category : 'writing' }
CypressPackageManager >> writePackagesToDiskFrom: someCypressPackageInformations [

	| packageStructure |
	^someCypressPackageInformations do: 
			[:each |
			packageStructure := CypressPackageStructure
						fromPackage: (CypressPackageDefinition named: each name).
			each repository writer writePackageStructure: packageStructure.
			each refresh.
			self saveKnownPackages]

]

{ #category : 'writing - private' }
CypressPackageManager >> writePackageStructure: packageStructure to: aCypressRepository [

	aCypressRepository writer writePackageStructure: packageStructure

]

{ #category : 'writing' }
CypressPackageManager >> writePackageToDiskFrom: aCypressPackageInformation [

	^self writePackagesToDiskFrom: (Array with: aCypressPackageInformation)

]
