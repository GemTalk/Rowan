method browsing
removeMethod: methodSelector forClassNamed: className isMeta: isMeta

	"Remove the given method selector from the class named className"

	| loadedMethodToBeRemoved loadedPackage loadedClassOrExtension projectDef packageDef crDef |
	loadedMethodToBeRemoved := self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [
			RwPerformingUnpackagedEditNotification signal: 'Attempt to remove an unpackaged method from the class ', className printString, '. The removal will not be tracked by Rowan'.
			"Notification resumed, so continue with removal"
			^ (Rowan globalNamed: className) removeSelector: methodSelector ].
	loadedPackage := loadedMethodToBeRemoved loadedPackage.
	loadedClassOrExtension := loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'Internal error -- no class or extension for ' , className printString
						, ' in package ' , loadedPackage name printString , '.' ].
	projectDef := loadedPackage loadedProject asDefinition.
	packageDef := projectDef packageNamed: loadedPackage name.
	crDef := loadedClassOrExtension isLoadedClass
		ifTrue: [ packageDef classDefinitions at: loadedClassOrExtension name ]
		ifFalse: [ packageDef classExtensions at: loadedClassOrExtension name ].
	isMeta
		ifTrue: [ crDef removeClassMethod: methodSelector ]
		ifFalse: [ crDef removeInstanceMethod: methodSelector ].
	Rowan projectTools load loadProjectDefinition: projectDef