"
Keeps track of what LoadedPackages are in the GemStone repository, and is the gateway for operations to query or change the image.
"
Class {
	#name : 'RwGsImage',
	#superclass : 'Object',
	#category : 'Rowan-GemStone'
}

{ #category : 'modifying image' }
RwGsImage class >> applyModification: aPackageSetModification for: aRwProjectLoadSpecification [

	| visitor patchSet |
	visitor := RwGsImagePatchVisitor newFor: aRwProjectLoadSpecification.
	visitor visit: aPackageSetModification.
	patchSet := visitor patchSet.
	patchSet classesWithNewVersions isEmpty
		ifTrue: [ 
			patchSet apply.
			^ self ].
	patchSet classesWithNewVersions
		do: [ :each | each newClassVersionPatches: aPackageSetModification patchSet: nil ].
	visitor := RwGsImagePatchVisitor newFor: aRwProjectLoadSpecification.
	visitor visit: aPackageSetModification.
	patchSet := visitor patchSet.
	patchSet apply
]

{ #category : 'querying' }
RwGsImage class >> currentUserId [

	^ GsCurrentSession currentSession userProfile userId
]

{ #category : 'querying' }
RwGsImage class >> loadedPackageNamed: aName ifAbsent: absentBlock [

	"scan the symbol list a RwLoadedPackage instance of the given name"

	self symbolList
		do: [ :symbolDict | 
			(symbolDict isKindOf: RwGsPackageSymbolDictionary)
				ifTrue: [ 
					(symbolDict loadedPackageNamed: aName ifAbsent: [  ])
						ifNotNil: [ :loadedPackage | ^ loadedPackage ] ] ].
	^ absentBlock value
]

{ #category : 'querying' }
RwGsImage class >> newOrExistingSymbolDictionaryNamed: dictName [

	"If the current session's transient symbol list includes a dictionary with the given name, answer it.
	Otherwise, create one, add it to the beginning of both transient and persistent symbol lists, and 
	answer it."

	| symbolName session symbolList association |
	symbolName := dictName asSymbol.
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	^ symbolList
		detect: [ :each | (each at: symbolName ifAbsent: [ nil ]) == each ]
		ifNone: [ 
			| newDict |
			newDict := RwGsPackageSymbolDictionary new
				name: symbolName;
				objectSecurityPolicy: symbolList objectSecurityPolicy;
				yourself.
			symbolList insertObject: newDict at: 1.
			session userProfile symbolList addLast: newDict.
			newDict ]
]

{ #category : 'querying' }
RwGsImage class >> objectNamed: aSymbol [

	"Returns the first object in the current session's symbol list that has the given
 name.  If no object with the given name is found, returns nil."

	^ GsCurrentSession currentSession symbolList objectNamed: aSymbol
]

{ #category : 'querying' }
RwGsImage class >> resolveName: aName [

	"Searches the current session's symbol list for an Association whose key is equal to
 aString, and returns that Association.  If no such Association is found in the
 symbol list, returns nil. Uses the current session's transient copy of the symbol list."

	^ GsCurrentSession currentSession resolveSymbol: aName asSymbol
]

{ #category : 'querying' }
RwGsImage class >> symbolList [

	"Answer the current session (transient) symbol list"

	^ GsCurrentSession currentSession symbolList
]
