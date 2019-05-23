method browsing
isExtensionMethod: methodSelector forClassNamed: className isMeta: isMeta

	"Answer true if the specified method is a Rowan extension method"

	| loadedMethod loadedClassOrExtension loadedPackage |

	loadedMethod := self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [ 
			"No loaded method found ... it is not an extension method"
			^ false ].
	loadedPackage := loadedMethod loadedPackage.

	loadedClassOrExtension := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'Internal error -- no class or extension for ' , className printString
						, ' in package ' , loadedPackage name printString , '.' ].

	^loadedClassOrExtension isLoadedClassExtension
