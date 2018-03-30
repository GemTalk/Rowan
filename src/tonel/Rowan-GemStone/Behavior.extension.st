Extension { #name : 'Behavior' }

{ #category : '*rowan-gemstone' }
Behavior >> rwCompileMethod: sourceString category: aCategoryString [

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategoryString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
]

{ #category : '*rowan-gemstone' }
Behavior >> rwCompileMethod: sourceString category: aCategoryString packageName: packageName [

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategoryString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		inPackageNamed: packageName
]

{ #category : '*rowan-gemstone' }
Behavior >> rwRemoveSelector: methodSelector [

	^ Rowan projectTools browser
		removeMethod: methodSelector
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
]
