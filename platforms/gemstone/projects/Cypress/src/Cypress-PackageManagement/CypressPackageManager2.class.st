"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressPackageManager2',
	#superclass : 'Object',
	#instVars : [
		'knownRepositories',
		'packageInformationList'
	],
	#classVars : [
		'SavedPackageManagers'
	],
	#category : 'Cypress-PackageManagement'
}

{ #category : 'Instance Creation' }
CypressPackageManager2 class >> create [

	^self new
		initializeFromImage;
		yourself.

]

{ #category : 'Initializing' }
CypressPackageManager2 class >> initialize [

	self savedPackageManagers: IdentityDictionary new

]

{ #category : 'Accessing' }
CypressPackageManager2 class >> named: aKey [
	"Answer the Package Manager previously saved under aKey.
	 It is an error if there was not one saved under that key."

	^self
		named: aKey
		or: [self error: 'No previously saved Package Manager under the key ', aKey printString]

]

{ #category : 'Accessing' }
CypressPackageManager2 class >> named: aKey or: aBlock [
	"Answer the Package Manager previously saved under aKey.
	 Answer the result of evaluating aBlock, if there was not one saved under that key."

	^self savedPackageManagers at: aKey ifAbsent: aBlock

]

{ #category : 'Instance Creation' }
CypressPackageManager2 class >> new [

	^super new
		initialize;
		yourself.

]

{ #category : 'Accessing - private' }
CypressPackageManager2 class >> packageNamePermutationsFor: aString [
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

{ #category : 'Accessing - private' }
CypressPackageManager2 class >> potentialPackageNames [
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

{ #category : 'Updating' }
CypressPackageManager2 class >> removePackageManagerSavedAs: aKey [
	"Remove the Package Manager previously saved under aKey, if there was one.
	 Answer it or nil if there was not one saved under that key."

	^self savedPackageManagers removeKey: aKey ifAbsent: []

]

{ #category : 'Accessing' }
CypressPackageManager2 class >> savedPackageManagers [

	^SavedPackageManagers

]

{ #category : 'Initializing - private' }
CypressPackageManager2 class >> savedPackageManagers: anIdentityDictionary [

	SavedPackageManagers := anIdentityDictionary

]

{ #category : 'Updating' }
CypressPackageManager2 >> addRepository: aRepository to: aKnownPackageInformation [

	aKnownPackageInformation addRepository: aRepository.

]

{ #category : 'Updating' }
CypressPackageManager2 >> addUnknownPackageNamed: aString [

	self packageInformationList
		at: aString
		put: (CypressUnknownPackageInformation named: aString).

]

{ #category : 'Querying' }
CypressPackageManager2 >> allResolvedPackageReferences [
  | resolved |
  resolved := OrderedCollection new.
  self knownRepositories
    keysAndValuesDo: [ :repoUrl :repo | 
      repo packageNames
        do: [ :packageName | resolved add: (CypressResolvedReference name: packageName repository: repo) ] ].
  ^ resolved asSortedCollection asArray

]

{ #category : 'Updating' }
CypressPackageManager2 >> assignRepository: aRepository to: aPackageInformation [

	self assignRepository: aRepository toAll: (Array with: aPackageInformation)

]

{ #category : 'Updating' }
CypressPackageManager2 >> assignRepository: aRepository toAll: somePackageInformations [
	"Assign to those having no repository information and add to those with.
	 Those without a repository need to be converted to Known Package Information instances."

	self knownRepositories at: aRepository url put: aRepository.
	(somePackageInformations reject: [:each | each isKnown])
		do: [:each | self addRepository: aRepository to: (self convertToKnown: each)].
	(somePackageInformations select: [:each | each isKnown])
		do: [:each | self addRepository: aRepository to: each].

]

{ #category : 'Updating' }
CypressPackageManager2 >> convert: anUnknownPackageInformation toConflictingWith: aKnownPackageInformation [

	| conflicting |
	conflicting := CypressConflictingPackageInformation
				fromUnknown: anUnknownPackageInformation
				conflictingWith: aKnownPackageInformation.
	self replace: anUnknownPackageInformation with: conflicting.
	^conflicting

]

{ #category : 'Updating' }
CypressPackageManager2 >> convert: anUnknownPackageInformation toEclipsedBy: aKnownPackageInformation [

	| eclipsed |
	eclipsed := CypressEclipsedPackageInformation
				fromUnknown: anUnknownPackageInformation
				eclipsedBy: aKnownPackageInformation.
	self replace: anUnknownPackageInformation with: eclipsed.
	^eclipsed

]

{ #category : 'Updating' }
CypressPackageManager2 >> convertToKnown: aPackageInformation [

	| known |
	known := CypressKnownPackageInformation fromUnknown: aPackageInformation.
	self replace: aPackageInformation with: known.
	(self findPackagesEclipsedBy: known) do: [:each | self convert: each toEclipsedBy: known].
	(self findPackagesConflictingWith: known) do: [:each | self convert: each toConflictingWith: known].
	^known.

]

{ #category : 'Updating' }
CypressPackageManager2 >> createRepositoryNamed: aName under: aDirectory alias: aString schema: schemaName [

	^CypressFileSystemRepository createOn: (CypressUrl
				absoluteFromText: schemaName
						, (CypressFileUtilities current directoryFromPath: aName
								relativeTo: aDirectory)
							, '/')
		alias: aString

]

{ #category : 'Accessing - private' }
CypressPackageManager2 >> findPackagesConflictingWith: aKnownPackageInformation [

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name and: 
					[(aKnownPackageInformation name indexOfSubCollection: each name , '-' startingAt: 1 ifAbsent: [ 0 ]) = 1]]

]

{ #category : 'Accessing - private' }
CypressPackageManager2 >> findPackagesEclipsedBy: aKnownPackageInformation [

	^self packageInformationList select: 
			[:each |
			aKnownPackageInformation name ~= each name and: 
					[(each name indexOfSubCollection: aKnownPackageInformation name , '-' startingAt: 1 ifAbsent: [ 0 ]) = 1]]

]

{ #category : 'Initializing - private' }
CypressPackageManager2 >> initialize [

	self
		knownRepositories: Dictionary new;
		packageInformationList: Dictionary new

]

{ #category : 'Initializing - private' }
CypressPackageManager2 >> initializeFromImage [

	self initializePackageInformationList.

]

{ #category : 'Initializing - private' }
CypressPackageManager2 >> initializePackageInformationList [

	self
		packageInformationList: (self potentialPackageNames
				inject: Dictionary new
				into: 
					[:dict :each |
					dict
						at: each put: (CypressUnknownPackageInformation named: each);
						yourself])

]

{ #category : 'Accessing' }
CypressPackageManager2 >> knownRepositories [

	^knownRepositories

]

{ #category : 'Updating' }
CypressPackageManager2 >> knownRepositories: someNamedRepositories [

	knownRepositories := someNamedRepositories

]

{ #category : 'Loading' }
CypressPackageManager2 >> loadPackageFrom: aKnownPackageInformation defaultSymbolDictionaryName: defaultSymbolDictionaryNameOrNil inRepository: aRepository [
  | snapshot summary loader |
  snapshot := (aRepository
    readPackageStructureForPackageNamed: aKnownPackageInformation name) snapshot.
  loader := snapshot
    updatePackage:
      (CypressPackageDefinition named: aKnownPackageInformation name)
    defaultSymbolDictionaryName: defaultSymbolDictionaryNameOrNil.
  summary := Dictionary new.
  loader unloadable notEmpty
    ifTrue: [ 
      summary
        at: 'Unloadable'
        put: (loader unloadable collect: [ :each | each printString ]) ].
  loader errors notEmpty
    ifTrue: [ summary at: 'Errors' put: (loader errors collect: [ :each | each printString ]) ].
  loader requirements notEmpty
    ifTrue: [ summary at: 'Missing Requirements' put: loader requirements asArray ].
  ^ summary

]

{ #category : 'Loading' }
CypressPackageManager2 >> loadPackageFrom: aKnownPackageInformation inRepository: aRepository [
  ^ self
    loadPackageFrom: aKnownPackageInformation
    defaultSymbolDictionaryName: nil
    inRepository: aRepository

]

{ #category : 'Updating' }
CypressPackageManager2 >> lookForPackagesInRepository: aRepository [
	"Find the packages in the repository and update the list
	 of package information accordingly."

	| packageNames inImage exImage |
	packageNames := aRepository packageNames.
	inImage := self packageInformationList asArray select: [:each | packageNames includes: each name].
	exImage := packageNames reject: [:each | self packageInformationList anySatisfy: [:info | info name = each]].
	exImage := exImage collect: [:each | CypressUnknownPackageInformation named: each].

	self assignRepository: aRepository toAll: inImage, exImage

]

{ #category : 'Accessing' }
CypressPackageManager2 >> packageInformationList [

	^packageInformationList

]

{ #category : 'Updating' }
CypressPackageManager2 >> packageInformationList: someNamedPackageInformations [

	packageInformationList := someNamedPackageInformations

]

{ #category : 'Accessing' }
CypressPackageManager2 >> packageInformationNamed: aString [

	^self packageInformationNamed: aString
		or: [self error: 'No package information for ' , aString printString]

]

{ #category : 'Accessing' }
CypressPackageManager2 >> packageInformationNamed: aString or: aBlock [

	^self packageInformationList
		at: aString
		ifAbsent: aBlock.

]

{ #category : 'Accessing - private' }
CypressPackageManager2 >> potentialPackageNames [

	^self class potentialPackageNames

]

{ #category : 'Updating' }
CypressPackageManager2 >> replace: oldPackageInformation with: newPackageInformation [

	self packageInformationList
		at: oldPackageInformation name
		put: newPackageInformation.

]

{ #category : 'Initializing - private' }
CypressPackageManager2 >> repositoryOn: url alias: aString [
	"Answer a repository instance for the specified URL.
	 The characteristics will come from the properties file at the URL location,
	 or will default according to the schema, if there is no properties file."

	^self knownRepositories
		at: url
		ifAbsentPut: [CypressAbstractRepository onUrl: url alias: aString].

]

{ #category : 'Updating' }
CypressPackageManager2 >> saveAs: aKey [
	"Save the receiver in the class' collection of named managers
	 under the specified key.
	 It will quietly replace anything already under that key."


	self savedPackageManagers at: aKey put: self.

]

{ #category : 'Accessing - private' }
CypressPackageManager2 >> savedPackageManagers [

	^self class savedPackageManagers

]

{ #category : 'Writing' }
CypressPackageManager2 >> writeChangesToAllRepositoriesFor: aPackageInformation [

	aPackageInformation writeChangesToAllRepositories.

]
