class browsing
removeProtocol: hybridPackageName fromClassNamed: className isMeta: isMeta

	"hybridPackageName is expected to be an existing protocol in className, that may or may not be empty"

	| projectSetDefinition loadedClass theClass theBehavior loadedPackage |
	projectSetDefinition := RwProjectSetDefinition new.

	loadedClass := self _loadedClassNamed: className ifAbsent: [ ].

	loadedPackage := (hybridPackageName at: 1) = $*
		ifTrue: [ 
			Rowan image
				loadedHybridPackageNamed: hybridPackageName
				ifAbsent: [ 
					self
						error:
							'A package for hybrid package name ' , hybridPackageName printString
								, ' was not found.' ] ]
		ifFalse: [ loadedClass loadedPackage ].

	theClass := Rowan image objectNamed: className.
	theBehavior := isMeta
		ifTrue: [ theClass class ]
		ifFalse: [ theClass ].

	(((theBehavior selectorsIn: hybridPackageName)
		collect: [:methodSelector |
			self
				_loadedMethod: methodSelector
				inClassNamed: className
				isMeta: isMeta
				ifAbsent: [
					RwPerformingUnpackagedEditNotification signal: 'Attempt to remove an unpackaged method from the class ', className printString, '. The removal will not be tracked by Rowan'.
					"Notification resumed, so continue with removal"
					(Rowan image objectNamed: className) removeSelector: methodSelector.
					nil "no loaded method involved" ] ]) select: [:each | each notNil ])
			do: [:loadedMethodToBeRemoved |
				| loadedClassOrExtension projectDef crDef packageDef methodSelector | 

				methodSelector := loadedMethodToBeRemoved selector.
				loadedPackage == loadedMethodToBeRemoved loadedPackage
					ifFalse: [ self error: 'Internal error -- the loaded package of a method to be removed does not match the expected loaded package' ].
				loadedClassOrExtension := loadedPackage
					classOrExtensionForClassNamed: className
					ifAbsent: [ 
						self
							error:
								'Internal error -- no class or extension for ' , className printString
									, ' in package ' , loadedPackage name printString , '.' ].
				projectDef := projectSetDefinition
					projectNamed: loadedPackage loadedProject name
					ifAbsent: [ 
						projectDef := loadedPackage loadedProject asDefinition.
						projectSetDefinition addProject: projectDef.
						projectDef ].
				packageDef := projectDef packageNamed: loadedPackage name.
				crDef := loadedClassOrExtension isLoadedClass
					ifTrue: [ packageDef classDefinitions at: loadedClassOrExtension name ]
					ifFalse: [ packageDef classExtensions at: loadedClassOrExtension name ].
				isMeta
					ifTrue: [ crDef removeClassMethod: methodSelector ]
					ifFalse: [ crDef removeInstanceMethod: methodSelector ] ].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	"Rowan does not automatically remove empty method categories ... just removes methods,
		we need to clean up after Rowan is done."
	theBehavior removeCategory: hybridPackageName asSymbol