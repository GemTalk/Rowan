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
true.
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
true.
%

! Class Implementation for CypressPackageComparator

! ------------------- Class methods for CypressPackageComparator

category: 'instance creation'
classmethod: CypressPackageComparator
comparingPackageNamed: packageName fromDirectory: aDirectory

	^(self new)
		comparingPackageNamed: packageName fromDirectory: aDirectory;
		yourself
%

category: 'instance creation'
classmethod: CypressPackageComparator
forCypress

	^(self new)
		comparingPackages: #('Cypress-Definitions' 'Cypress-Mocks' 'Cypress-Structure' 'Cypress-Tests' 'Cypress-GemStoneFileServer' 'Cypress-Comparison')
			fromDirectory: '/opt/git/CypressReferenceImplementation/';
		yourself
%

category: 'instance creation'
classmethod: CypressPackageComparator
new

	^super new
		initialize;
		yourself
%

! ------------------- Instance methods for CypressPackageComparator

category: 'comparing - private'
method: CypressPackageComparator
add: aDefinition to: aDictionary

	aDefinition
		classDefinition: [:classDefinition | self addClassDefinition: classDefinition to: aDictionary]
		methodDefinition: [:methodDefinition | self addMethodDefinition: methodDefinition to: aDictionary]
%

category: 'comparing - private'
method: CypressPackageComparator
addClassDefinition: classDefinition to: aDictionary

	(aDictionary at: classDefinition className ifAbsentPut: [Dictionary new])
		at: 'class category' put: classDefinition category;
		at: 'class comment' put: classDefinition comment;
		at: 'class definition' put: classDefinition classDefinitionString.
%

category: 'comparing - private'
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
method: CypressPackageComparator
applyAddition: aCypressAddition

	self add: aCypressAddition definition to: self currentAdditions
%

category: 'comparing - private'
method: CypressPackageComparator
applyModification: aCypressModification

	self
		add: aCypressModification modification to: self currentAdditions;
		add: aCypressModification obsoletion to: self currentRemovals.
%

category: 'comparing - private'
method: CypressPackageComparator
applyRemoval: aCypressRemoval

	self add: aCypressRemoval definition to: self currentRemovals.
%

category: 'comparing'
method: CypressPackageComparator
compare

	diskSnapshots keys do: [:packageName |
		self resetCurrentForPackage: packageName.
		self currentPatchOperations do: [:each | each applyTo: self].
	].
	self resetCurrentForPackage: nil.
%

category: 'initializing'
method: CypressPackageComparator
comparingPackageNamed: packageName fromDirectory: aDirectory

	self comparingPackages: (Array with: packageName) fromDirectory: aDirectory
%

category: 'initializing'
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
method: CypressPackageComparator
currentAdditions

	currentAdditions ifNil: [self updateCurrentAdditionsAndRemovals].
	^currentAdditions
%

category: 'comparing - private'
method: CypressPackageComparator
currentDiskSnapshot

	^diskSnapshots at: currentPackageName
%

category: 'comparing - private'
method: CypressPackageComparator
currentImageSnapshot

	^imageSnapshots at: currentPackageName
%

category: 'comparing - private'
method: CypressPackageComparator
currentPatchOperations

	^(CypressPatch fromBase: self currentDiskSnapshot toTarget: self currentImageSnapshot) operations.
%

category: 'comparing - private'
method: CypressPackageComparator
currentRemovals

	currentRemovals ifNil: [self updateCurrentAdditionsAndRemovals].
	^currentRemovals
%

category: 'comparing'
method: CypressPackageComparator
getDifferences

	self compare.
	^self snapshotDifferences
%

category: 'initializing - private'
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
method: CypressPackageComparator
resetCurrentForPackage: aStringOrNil

	currentPackageName := aStringOrNil.
	currentAdditions := nil.
	currentRemovals := nil.
%

category: 'accessing'
method: CypressPackageComparator
snapshotDifferences

	^snapshotDifferences
%

category: 'comparing - private'
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
method: CypressClassDefinition
category: aString

	category := aString
%

category: '*Cypress-Comparison-accessing'
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
true.
%



! End of Package: Cypress-Comparison


