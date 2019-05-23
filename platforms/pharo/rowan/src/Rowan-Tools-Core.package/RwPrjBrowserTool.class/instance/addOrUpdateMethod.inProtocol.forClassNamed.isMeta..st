method browsing
addOrUpdateMethod: methodSource inProtocol: hybridPackageName forClassNamed: className isMeta: isMeta

	"If the method is already installed in a different package, remove the method from that package.
	 If package name matches the name of the package of the class definition, then add the method 
		to the class definition.
	 If there is no matching class extension or the package name does not match, add the method 
		to a class extension in the named package.
	 Return the resulting compiled method"

	"a hybrid package name has a leading '*' followed by the name of a package ... 
 		where the hybrid package name is not expected to preserve case"

	| loadedPackage |
	loadedPackage := (hybridPackageName at: 1) = $*
		ifTrue: [ 
			Rowan image
				loadedHybridPackageNamed: hybridPackageName
				ifAbsent: [ 
					self
						error:
							'A package for hybrid package name ' , hybridPackageName printString
								, ' was not found.' ] ]
		ifFalse: [ 
			| loadedClass |
			loadedClass := Rowan image 
				loadedClassNamed: className 
				ifAbsent: [
					RwPerformingUnpackagedEditNotification signal: 'Attempt to add or modify an unpackage method in the class ', className printString, '. The modification will not be tracked by Rowan'.
					"Notification resumed, so continue with add/modify"
					^ (Rowan image objectNamed: className)
						compileMethod: methodSource
						dictionaries: Rowan image symbolList
						category: hybridPackageName].
			loadedClass loadedPackage ].

	^ self
		addOrUpdateMethod: methodSource
		inProtocol: hybridPackageName
		forClassNamed: className
		isMeta: isMeta
		inPackageNamed: loadedPackage name