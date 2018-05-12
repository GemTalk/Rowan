Extension { #name : 'Behavior' }

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rowanPackageName [

	"answer the name of the package that the receiver is a member of. Answer 'Rowan unpackagedName` if 
		the receiver is not a member of any package"

	| loadedClass |
	loadedClass := Rowan image
		loadedClassForClass: self thisClass
		ifAbsent: [ ^ Rowan unpackagedName ].
	^ loadedClass loadedPackage name

]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rowanProjectName [

	"answer the name of the project that the receiver is a member of. Answer `Rowan unpackagedName` if 
		the receiver is not a member of any project"

	| loadedClass |
	loadedClass := Rowan image
		loadedClassForClass: self thisClass
		ifAbsent: [ ^ Rowan unpackagedName ].
	^ loadedClass loadedProject name

]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwCompileMethod: sourceString category: aCategoryString [

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategoryString asString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwCompileMethod: sourceString category: aCategoryString packageName: packageName [

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategoryString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		inPackageNamed: packageName
]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwMoveMethod: methodSelector toCategory: categoryName [

	^ Rowan projectTools browser
		moveMethod: methodSelector
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		toProtocol: categoryName

]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwRemoveCategory: categoryName [

	^ Rowan projectTools browser
		removeProtocol: categoryName 
		fromClassNamed:  self thisClass name asString
		isMeta: self isMeta

]

{ #category : '*rowan-gemstone-kernel' }
Behavior >> rwRemoveSelector: methodSelector [

	^ Rowan projectTools browser
		removeMethod: methodSelector
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
]
