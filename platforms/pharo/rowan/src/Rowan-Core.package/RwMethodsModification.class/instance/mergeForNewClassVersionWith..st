merging
mergeForNewClassVersionWith: aMethodsModification

	"merge aMethodsModification modifictions into the receiver"

	"add all method modifications to the receiver, there should be no selector overlap, if there is 
		the receiver wins --- but for now we'll error out. "

	aMethodsModification elementsModified
		keysAndValuesDo: [ :selector :aMethodModification | 
			(elementsModified at: selector ifAbsent: [  ])
				ifNil: [elementsModified at: selector put: aMethodModification]
				ifNotNil: [ :existingMethodModification | 
					existingMethodModification mergeForNewClassVersionWith: aMethodModification ] ]
