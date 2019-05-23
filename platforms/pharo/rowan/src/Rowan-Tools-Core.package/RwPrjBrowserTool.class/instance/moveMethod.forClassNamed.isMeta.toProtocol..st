method browsing
moveMethod: methodSelector forClassNamed: className isMeta: isMeta toProtocol: hybridPackageName

	"hybridPackageName is expected to be an existing protocol in className"

	| loadedMethodToBeMoved srcLoadedMethodPackage srcLoadedClassPackage srcLoadedClassOrExtension projectDef packageDef 
		clsDef projectSetDefinition destinationLoadedPackage methodDef |

	loadedMethodToBeMoved := self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [ ].

	destinationLoadedPackage := (hybridPackageName at: 1) = $*
		ifTrue: [ 
			Rowan image
				loadedHybridPackageNamed: hybridPackageName
				ifAbsent: [ 
					self
						error:
							'A package for hybrid package name ' , hybridPackageName printString
								, ' was not found.' ] ]
		ifFalse: [ 
			| lc |
			lc := self
				_loadedClassNamed: className
				ifAbsent: [].
			lc 
				ifNil: [
					loadedMethodToBeMoved
						ifNil: [ 
							"unpackaged method moved to unpackaged category"
							^ (Rowan image objectNamed: className) moveMethod: methodSelector toCategory: hybridPackageName ].
				RwPerformingUnpackagedEditNotification signal: 'Attempt to move a packaged method to an unpackaged class ', className printString, '. The unpackaged method will not be tracked by Rowan'.
				"Notification resumed, so continue with move"
				"Move packaged method to unpackaged category"
				"Disown the method, then move it to proper category" 
				Rowan packageTools disown disownMethod: methodSelector inClassNamed: className isMeta: isMeta.
				^ (Rowan image objectNamed: className) moveMethod: methodSelector toCategory: hybridPackageName ].
			"use the loaded package for the class that contains the method"
			srcLoadedClassPackage := lc loadedPackage ].

	loadedMethodToBeMoved
		ifNil: [
			| instanceSelectors classSelectors res |
			"Moving an unpackaged method to a package in an unpackage class"
			"Move the method to the proper category and then adopt the method"
			isMeta
				ifTrue: [
					instanceSelectors := {}.
					classSelectors := { methodSelector } ]
				ifFalse: [
					instanceSelectors := { methodSelector }.
					classSelectors := {} ].
			res := (Rowan image objectNamed: className) moveMethod: methodSelector toCategory: hybridPackageName.
			Rowan packageTools adopt
				adoptClassNamed: className 
				classExtension: true
				instanceSelectors: instanceSelectors 
				classSelectors: classSelectors 
				intoPackageNamed: destinationLoadedPackage name.
			^ res ].

	"Moving a packaged method to another package"
	srcLoadedMethodPackage := loadedMethodToBeMoved loadedPackage.
	srcLoadedClassOrExtension := srcLoadedMethodPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'Internal error -- no class or extension for ' , className printString
						, ' in package ' , srcLoadedMethodPackage name printString , '.' ].

	projectSetDefinition := RwProjectSetDefinition new.
	projectDef := srcLoadedMethodPackage loadedProject asDefinition.
	packageDef := projectDef packageNamed: srcLoadedMethodPackage name.
	clsDef := srcLoadedClassOrExtension isLoadedClass
		ifTrue: [ packageDef classDefinitions at: className ]
		ifFalse: [ packageDef classExtensions at: className ].

	projectSetDefinition addProject: projectDef.
	destinationLoadedPackage name = srcLoadedMethodPackage name
		ifTrue: [
			"method is not moving to a different package, only need to change the protocol for the method"

			methodDef := isMeta
				ifTrue: [ clsDef classMethodDefinitions at: methodSelector ]
				ifFalse: [ clsDef instanceMethodDefinitions at: methodSelector ].
			methodDef protocol: hybridPackageName ]
		ifFalse: [
			"method is moving to a different package"

			| destProjectDef destPackageDef destClsDef srcPackageDef  srcClsDef |
			destinationLoadedPackage loadedProject name = projectDef name 
				ifTrue: [ destProjectDef := projectDef ]
				ifFalse: [ 
					"method is moving to a different project"
					destProjectDef := destinationLoadedPackage loadedProject asDefinition.
					projectSetDefinition addProject: destProjectDef ].
			srcPackageDef := projectDef packageNamed: srcLoadedMethodPackage name.
			srcClsDef := srcLoadedClassOrExtension isLoadedClass
				ifTrue: [ srcPackageDef classDefinitions at: className ]
				ifFalse: [ srcPackageDef classExtensions at: className ].

			destPackageDef := destProjectDef packageNamed: destinationLoadedPackage name. 

			destClsDef := srcLoadedClassPackage == destinationLoadedPackage
				ifTrue: [
					"method is not an extension method ... add the method to the class definition"
					destPackageDef 
						classDefinitions 
							at: className 
							ifAbsent: [ 
								self error: 'internal error - class ', 
									className printString , 
									' not found in expected package ', 
									destinationLoadedPackage name printString ] ]
				ifFalse: [ 
					"method is to be added as an extension method" 
					destPackageDef classExtensions at: className ifAbsentPut: [ RwClassExtensionDefinition newForClassNamed: className ] ].
			isMeta
				ifTrue: [
					methodDef := srcClsDef classMethodDefinitions at: methodSelector.
					methodDef protocol: hybridPackageName.
					destClsDef addClassMethodDefinition: methodDef.
					srcClsDef removeClassMethod: methodSelector ]
				ifFalse: [ 
					methodDef := srcClsDef instanceMethodDefinitions at: methodSelector.
					methodDef protocol: hybridPackageName.
					destClsDef addInstanceMethodDefinition: methodDef.
					srcClsDef removeInstanceMethod: methodSelector ] ].

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.