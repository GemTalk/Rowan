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
	subclass: 'CypressPackageComparator'
	instVarNames: #( directoryPackageMap diskTimestamps diskSnapshots imageSnapshots snapshotDifferences currentPackageName currentAdditions currentRemovals )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'Cypress-Comparison';
		comment: '';
		immediateInvariant.
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
			reader := ((Globals at: #CypressFileSystemRepository) on: aDirectory) reader
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
		inDictionary: Globals
		options: #()'
%

! Class initializers 

doit
%



! End of Package: Cypress-Comparison

! patches because of dependency problems
category: 'comparing'
method: CypressPackageManager
comparePackagesFrom: someCypressPackageInformations

        ^(someCypressPackageInformations
                inject: CypressPackageComparator new
                into:
                        [:comparer :each |
                        comparer comparingPackageNamed: each name fromDirectory: each savedLocation])
                                getDifferences
%

