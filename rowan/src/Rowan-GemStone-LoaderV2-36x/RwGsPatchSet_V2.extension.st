Extension { #name : 'RwGsPatchSet_V2' }

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsPatchSet_V2 >> doMoveMethodsBetweenPackages [
	| image |
	image := Rowan image.
	movedMethods
		do: [ :aMethodMove | 
			| packageDef classOrExtensionDef loadedPackage loadedClassOrExtension loadedMethods loadedMethod compiledMethod registry |
			packageDef := aMethodMove packageBefore.
			classOrExtensionDef := aMethodMove classOrExtensionBefore.
			loadedPackage := image loadedPackageNamed: packageDef name.
			loadedClassOrExtension := loadedPackage
				classOrExtensionForClassNamed: classOrExtensionDef name
				ifAbsent: [ 
					self
						error:
							'internal error - no loaded class or extension found for class '
								, classOrExtensionDef name printString , '  in package '
								, packageDef name printString ].
			loadedMethods := aMethodMove isMeta
				ifTrue: [ loadedClassOrExtension loadedClassMethods ]
				ifFalse: [ loadedClassOrExtension loadedInstanceMethods ].
			loadedMethod := loadedMethods at: aMethodMove methodBefore selector.

			registry := image loadedRegistryForPackageNamed: loadedPackage name.

			loadedClassOrExtension removeLoadedMethod: loadedMethod.
			loadedClassOrExtension isLoadedClassExtension
				ifTrue: [ 
					loadedClassOrExtension isEmpty
						ifTrue: [ 
							(registry
								loadedClassExtensionsForClass: loadedClassOrExtension handle
								ifAbsent: [  ])
								ifNotNil: [ :loadedClassExtensions | 
									loadedClassExtensions isEmpty
										ifFalse: [ 
											loadedClassExtensions remove: loadedClassOrExtension.
											loadedClassExtensions isEmpty
												ifTrue: [ 
													registry
														unregisterLoadedClassExtension: loadedClassOrExtension
														forClass: loadedClassOrExtension handle ] ] ].
							loadedPackage removeLoadedClassExtension: loadedClassOrExtension ] ].

			compiledMethod := loadedMethod handle.
			compiledMethod _owner: nil.

			packageDef := aMethodMove packageAfter.
			classOrExtensionDef := aMethodMove classOrExtensionAfter.
			loadedPackage := image loadedPackageNamed: packageDef name.
			registry := (Rowan image
				symbolDictNamed:
					(aMethodMove projectAfter
						gemstoneSymbolDictNameForPackageNamed: aMethodMove packageAfter name))
				rowanSymbolDictionaryRegistry.
			loadedClassOrExtension := loadedPackage
				classOrExtensionForClassNamed: classOrExtensionDef name
				ifAbsent: [ 
					classOrExtensionDef isClassExtension
						ifTrue: [ 
							"add a new class extension on demand"
							(aMethodMove projectAfter
								gemstoneUseSessionMethodsForExtensionsForPackageNamed:
									aMethodMove packageAfter name)
								ifTrue: [ 
									| resolved |
									((resolved := self loadSymbolList
										objectNamed: classOrExtensionDef name asSymbol) notNil
										and: [ resolved isBehavior and: [ resolved isMeta not ] ])
										ifFalse: [ 
											self
												error:
													'The extension class named ' , self className printString , ' cannot be found.' ]	"logic is not quite correct here ... need to create a lodedClassExtension for this puppy ... deferring session method work to a later date"	"https://github.com/dalehenrich/Rowan/issues/254" ]
								ifFalse: [ 
									registry
										ensureExtensionClassNamed: classOrExtensionDef name
										existsForPackageNamed: packageDef name
										implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2 ].
							loadedPackage loadedClassExtensions at: classOrExtensionDef name ]
						ifFalse: [ 
							self
								error:
									'internal error - no loaded class found for class '
										, classOrExtensionDef name printString , '  in package '
										, packageDef name printString ] ].

			loadedClassOrExtension addLoadedMethod: loadedMethod.

			compiledMethod _owner: loadedMethod ]
]
