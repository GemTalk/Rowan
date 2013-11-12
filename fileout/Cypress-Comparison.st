" Package: Cypress-Comparison"!


" Class Declarations "!

(Object
	subclass: 'CypressPackageComparator'
	instVarNames: #( directoryPackageMap diskTimestamps diskSnapshots imageSnapshots snapshotDifferences currentPackageName currentAdditions currentRemovals )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Comparison';
		comment: ''!

" Class Implementation for CypressPackageComparator"!

" ------------------- Class methods for CypressPackageComparator"!

!CypressPackageComparator class methodsFor: 'instance creation'!
comparingPackageNamed: packageName fromDirectory: aDirectory

	^(self new)
		comparingPackageNamed: packageName fromDirectory: aDirectory;
		yourself! !
!CypressPackageComparator class methodsFor: 'instance creation'!
forCypress

	^(self new)
		comparingPackages: #('Cypress-Definitions' 'Cypress-Mocks' 'Cypress-Structure' 'Cypress-Tests' 'Cypress-GemStoneFileServer' 'Cypress-Comparison')
			fromDirectory: '/opt/git/CypressReferenceImplementation/';
		yourself
! !
!CypressPackageComparator class methodsFor: 'instance creation'!
new

	^super new
		initialize;
		yourself
! !
" ------------------- Instance methods for CypressPackageComparator"!

!CypressPackageComparator methodsFor: 'comparing - private'!
add: aDefinition to: aDictionary

	aDefinition
		classDefinition: [:classDefinition | self addClassDefinition: classDefinition to: aDictionary]
		methodDefinition: [:methodDefinition | self addMethodDefinition: methodDefinition to: aDictionary]! !
!CypressPackageComparator methodsFor: 'comparing - private'!
addClassDefinition: classDefinition to: aDictionary

	(aDictionary at: classDefinition className ifAbsentPut: [Dictionary new])
		at: 'class category' put: classDefinition category;
		at: 'class comment' put: classDefinition comment;
		at: 'class definition' put: classDefinition classDefinitionString.
! !
!CypressPackageComparator methodsFor: 'comparing - private'!
addMethodDefinition: methodDefinition to: aDictionary

	((aDictionary at: methodDefinition className ifAbsentPut: [Dictionary new])
		at: (methodDefinition classIsMeta
				ifTrue: ['class methods']
				ifFalse: ['instance methods'])
		ifAbsentPut: [Dictionary new]) at: methodDefinition selector
			put: methodDefinition category -> methodDefinition source! !
!CypressPackageComparator methodsFor: 'comparing - private'!
applyAddition: aCypressAddition

	self add: aCypressAddition definition to: self currentAdditions! !
!CypressPackageComparator methodsFor: 'comparing - private'!
applyModification: aCypressModification

	self
		add: aCypressModification modification to: self currentAdditions;
		add: aCypressModification obsoletion to: self currentRemovals.
! !
!CypressPackageComparator methodsFor: 'comparing - private'!
applyRemoval: aCypressRemoval

	self add: aCypressRemoval definition to: self currentRemovals.
! !
!CypressPackageComparator methodsFor: 'comparing'!
compare

	diskSnapshots keys do: [:packageName |
		self resetCurrentForPackage: packageName.
		self currentPatchOperations do: [:each | each applyTo: self].
	].
	self resetCurrentForPackage: nil.
! !
!CypressPackageComparator methodsFor: 'initializing'!
comparingPackageNamed: packageName fromDirectory: aDirectory

	self comparingPackages: (Array with: packageName) fromDirectory: aDirectory! !
!CypressPackageComparator methodsFor: 'initializing'!
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
				put: (CypressPackageDefinition named: packageName) snapshot]! !
!CypressPackageComparator methodsFor: 'comparing - private'!
currentAdditions

	currentAdditions ifNil: [self updateCurrentAdditionsAndRemovals].
	^currentAdditions! !
!CypressPackageComparator methodsFor: 'comparing - private'!
currentDiskSnapshot

	^diskSnapshots at: currentPackageName
! !
!CypressPackageComparator methodsFor: 'comparing - private'!
currentImageSnapshot

	^imageSnapshots at: currentPackageName
! !
!CypressPackageComparator methodsFor: 'comparing - private'!
currentPatchOperations

	^(CypressPatch fromBase: self currentDiskSnapshot toTarget: self currentImageSnapshot) operations.
! !
!CypressPackageComparator methodsFor: 'comparing - private'!
currentRemovals

	currentRemovals ifNil: [self updateCurrentAdditionsAndRemovals].
	^currentRemovals! !
!CypressPackageComparator methodsFor: 'comparing'!
getDifferences

	self compare.
	^self snapshotDifferences! !
!CypressPackageComparator methodsFor: 'initializing - private'!
initialize

	directoryPackageMap := Dictionary new.
	diskTimestamps := Dictionary new.
	diskSnapshots := Dictionary new.
	imageSnapshots := Dictionary new.
	snapshotDifferences := Dictionary new
		at: 'newer' put: (Dictionary with: 'Finished at' -> DateAndTime now);
		at: 'older' put: (Dictionary with: 'Finished at' -> DateAndTime now);
		yourself.
! !
!CypressPackageComparator methodsFor: 'comparing - private'!
resetCurrentForPackage: aStringOrNil

	currentPackageName := aStringOrNil.
	currentAdditions := nil.
	currentRemovals := nil.
! !
!CypressPackageComparator methodsFor: 'accessing'!
snapshotDifferences

	^snapshotDifferences! !
!CypressPackageComparator methodsFor: 'comparing - private'!
updateCurrentAdditionsAndRemovals

	| oldTimestamp |
	currentAdditions := (snapshotDifferences at: 'newer')
				at: currentPackageName
				ifAbsentPut: [Dictionary new].
	oldTimestamp := (diskTimestamps at: currentPackageName) ifNil: [^self].
	currentRemovals := (snapshotDifferences at: 'older')
				at: currentPackageName
				ifAbsentPut: [Dictionary with: 'Timestamp' -> oldTimestamp printString].
! !
" Class Extensions "!

" Class Extension for CypressClassDefinition"!

" ------------------- Instance methods for CypressClassDefinition"!

!CypressClassDefinition methodsFor: '*Cypress-Comparison-accessing'!
category: aString

	category := aString! !
!CypressClassDefinition methodsFor: '*Cypress-Comparison-accessing'!
classDefinitionString

	^superclassName, ' subclass: ', self name printString, '
		instVarNames: #(', self instanceVariablesString, ')
		classVars: #(', self classVariablesString, ')
		classInstVars: #(', self classInstanceVariablesString, ')
		poolDictionaries: #(', self poolDictionariesString, ')
		inDictionary: UserGlobals
		options: #()'! !
" Class initializers "!




" End of Package: Cypress-Comparison"!


