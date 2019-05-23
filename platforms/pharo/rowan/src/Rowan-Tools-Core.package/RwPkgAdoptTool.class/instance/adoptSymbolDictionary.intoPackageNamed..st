smalltalk api
adoptSymbolDictionary: symbolDictionary intoPackageNamed: packageName

	"create loaded classes in the loaded package <packageName> for the unpackaged classes in <symbolDictionary>"

	symbolDictionary keysAndValuesDo: [:className :object |
		object isBehavior
			ifTrue: [ 
				object rowanPackageName = Rowan unpackagedName
					ifTrue: [ self adoptClassNamed: className asString intoPackageNamed: packageName ] ] ]
.
