"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressPackageComparator',
	#superclass : 'Object',
	#instVars : [
		'directoryPackageMap',
		'diskTimestamps',
		'diskSnapshots',
		'imageSnapshots',
		'snapshotDifferences',
		'currentPackageName',
		'currentAdditions',
		'currentRemovals'
	],
	#category : 'Cypress-Comparison'
}

{ #category : 'instance creation' }
CypressPackageComparator class >> comparingPackageNamed: packageName fromDirectory: aDirectory [

	^(self new)
		comparingPackageNamed: packageName fromDirectory: aDirectory;
		yourself

]

{ #category : 'instance creation' }
CypressPackageComparator class >> new [

	^super new
		initialize;
		yourself

]

{ #category : 'comparing - private' }
CypressPackageComparator >> add: aDefinition to: aDictionary [

	aDefinition
		classDefinition: [:classDefinition | self addClassDefinition: classDefinition to: aDictionary]
		methodDefinition: [:methodDefinition | self addMethodDefinition: methodDefinition to: aDictionary]

]

{ #category : 'comparing - private' }
CypressPackageComparator >> addClassDefinition: classDefinition to: aDictionary [

	(aDictionary at: classDefinition className ifAbsentPut: [Dictionary new])
		at: 'class category' put: classDefinition category;
		at: 'class comment' put: classDefinition comment;
		at: 'class definition' put: classDefinition classDefinitionString.

]

{ #category : 'comparing - private' }
CypressPackageComparator >> addMethodDefinition: methodDefinition to: aDictionary [

	((aDictionary at: methodDefinition className ifAbsentPut: [Dictionary new])
		at: (methodDefinition classIsMeta
				ifTrue: ['class methods']
				ifFalse: ['instance methods'])
		ifAbsentPut: [Dictionary new]) at: methodDefinition selector
			put: methodDefinition category -> methodDefinition source

]

{ #category : 'comparing - private' }
CypressPackageComparator >> applyAddition: aCypressAddition [

	self add: aCypressAddition definition to: self currentAdditions

]

{ #category : 'comparing - private' }
CypressPackageComparator >> applyModification: aCypressModification [

	self
		add: aCypressModification modification to: self currentAdditions;
		add: aCypressModification obsoletion to: self currentRemovals.

]

{ #category : 'comparing - private' }
CypressPackageComparator >> applyRemoval: aCypressRemoval [

	self add: aCypressRemoval definition to: self currentRemovals.

]

{ #category : 'comparing' }
CypressPackageComparator >> compare [

	diskSnapshots keys do: [:packageName |
		self resetCurrentForPackage: packageName.
		self currentPatchOperations do: [:each | each applyTo: self].
	].
	self resetCurrentForPackage: nil.

]

{ #category : 'initializing' }
CypressPackageComparator >> comparingPackages: someNames fromDirectory: aDirectory [

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
			modTimestamp := (modTime indexOfSubCollection: 'stat:' startingAt: 1 ifAbsent: [ 0 ]) = 1
						ifTrue: [nil]
						ifFalse: [self dateAndTimeFromUnixFormatString: modTime].
			diskTimestamps at: packageName put: modTimestamp.
			imageSnapshots at: packageName
				put: (CypressPackageDefinition named: packageName) snapshot]

]

{ #category : 'comparing - private' }
CypressPackageComparator >> currentAdditions [

	currentAdditions ifNil: [self updateCurrentAdditionsAndRemovals].
	^currentAdditions

]

{ #category : 'comparing - private' }
CypressPackageComparator >> currentDiskSnapshot [

	^diskSnapshots at: currentPackageName

]

{ #category : 'comparing - private' }
CypressPackageComparator >> currentImageSnapshot [

	^imageSnapshots at: currentPackageName

]

{ #category : 'comparing - private' }
CypressPackageComparator >> currentPatchOperations [

	^(CypressPatch fromBase: self currentDiskSnapshot toTarget: self currentImageSnapshot) operations.

]

{ #category : 'comparing - private' }
CypressPackageComparator >> currentRemovals [

	currentRemovals ifNil: [self updateCurrentAdditionsAndRemovals].
	^currentRemovals

]

{ #category : 'initializing - private' }
CypressPackageComparator >> dateAndTimeFromUnixFormatString: aString [
	"YYYY-MM-DDTHH:MM:SS +HHMM
	 Examples:
		| string |
		string := '2013-06-20 14:47:55.40271592140198 -0700'.
		(DateAndTimeANSI fromUnixFormatString: string) printString = '2013-06-20T14:47:55.40271592140198-07:00'.
	"

	| stream sign positionBias |
	stream := ReadStreamPortable on: aString.
	sign := aString at: aString size - 4.
	positionBias := stream class isLegacyStreamImplementation
				ifTrue: [1]
				ifFalse: [0].
	^DateAndTime
		year: (stream next: 4) asNumber
		month: (stream
				next;
				next: 2) asNumber
		day: (stream
				next;
				next: 2) asNumber
		hour: (stream
				next;
				next: 2) asNumber
		minute: (stream
				next;
				next: 2) asNumber
		second: (stream
				next;
				next: aString size - 6 - stream position + positionBias) asNumber
		offset: (Duration
				days: 0
				hours: (stream
						next;
						next;
						next: 2) asNumber
						* (sign == $- ifTrue: [-1] ifFalse: [1])
				minutes: (stream next: 2) asNumber
				seconds: 0)

]

{ #category : 'comparing' }
CypressPackageComparator >> getDifferences [

	self compare.
	^self snapshotDifferences

]

{ #category : 'initializing - private' }
CypressPackageComparator >> initialize [

	directoryPackageMap := Dictionary new.
	diskTimestamps := Dictionary new.
	diskSnapshots := Dictionary new.
	imageSnapshots := Dictionary new.
	snapshotDifferences := Dictionary new
		at: 'newer' put: (Dictionary with: 'Finished at' -> DateAndTime now);
		at: 'older' put: (Dictionary with: 'Finished at' -> DateAndTime now);
		yourself.

]

{ #category : 'comparing - private' }
CypressPackageComparator >> resetCurrentForPackage: aStringOrNil [

	currentPackageName := aStringOrNil.
	currentAdditions := nil.
	currentRemovals := nil.

]

{ #category : 'accessing' }
CypressPackageComparator >> snapshotDifferences [

	^snapshotDifferences

]

{ #category : 'comparing - private' }
CypressPackageComparator >> updateCurrentAdditionsAndRemovals [

	| oldTimestamp |
	currentAdditions := (snapshotDifferences at: 'newer')
				at: currentPackageName
				ifAbsentPut: [Dictionary new].
	oldTimestamp := (diskTimestamps at: currentPackageName) ifNil: [^self].
	currentRemovals := (snapshotDifferences at: 'older')
				at: currentPackageName
				ifAbsentPut: [Dictionary with: 'Timestamp' -> oldTimestamp printString].

]
