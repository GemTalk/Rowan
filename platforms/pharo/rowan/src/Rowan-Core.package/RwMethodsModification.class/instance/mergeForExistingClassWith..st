merging
mergeForExistingClassWith: anExistingMethodsModification

	"merge anExistingMethodsModification modifictions into the receiver"

	anExistingMethodsModification elementsModified
		keysAndValuesDo: [ :selector :anExistingMethodModification | 
			"anExistingMethodsModification contains any new  or modified methods that 
				were applied to this class and they all need to be brought forward 
				along with the forced new versions of methods (those not changed or removed)"
			(elementsModified at: selector ifAbsent: [  ])
				ifNil: [
					"added method"
					elementsModified at: selector put: anExistingMethodModification ]
				ifNotNil: [
					 "modified method"
					elementsModified 
						at: selector 
						put: anExistingMethodModification asMethodModificationForNewClassVersion] ]
