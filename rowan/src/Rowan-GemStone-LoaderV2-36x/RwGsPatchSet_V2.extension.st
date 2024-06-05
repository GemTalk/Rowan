Extension { #name : 'RwGsPatchSet_V2' }

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsPatchSet_V2 >> doMoveMethodsBetweenPackages [
	| image |
	image := Rowan image.
	movedMethods
		do: [ :aMethodMove | 
			| packageDef classOrExtensionOrTraitDef loadedPackage loadedClassOrExtensionOrTrait loadedMethods loadedMethod compiledMethod registry |
			packageDef := aMethodMove packageBefore.
			classOrExtensionOrTraitDef := aMethodMove classOrExtensionBefore.
			loadedPackage := image loadedPackageNamed: packageDef name.
			loadedClassOrExtensionOrTrait := loadedPackage
				classOrExtensionForClassNamed: classOrExtensionOrTraitDef name
				ifAbsent: [ 
					loadedPackage
						loadedTraitNamed: classOrExtensionOrTraitDef name
						ifAbsent: [ 
							self
								error:
									'internal error - no loaded class or extension or trait found '
										, classOrExtensionOrTraitDef name printString , '  in package '
										, packageDef name printString ] ].
			loadedMethods := aMethodMove isMeta
				ifTrue: [ loadedClassOrExtensionOrTrait loadedClassMethods ]
				ifFalse: [ loadedClassOrExtensionOrTrait loadedInstanceMethods ].
			loadedMethod := loadedMethods at: aMethodMove methodBefore selector.

			registry := image loadedRegistryForPackageNamed: loadedPackage name.

			loadedClassOrExtensionOrTrait removeLoadedMethod: loadedMethod.
			loadedClassOrExtensionOrTrait isLoadedClassExtension
				ifTrue: [ 
					loadedClassOrExtensionOrTrait isEmpty
						ifTrue: [ 
							(registry
								loadedClassExtensionsForClass: loadedClassOrExtensionOrTrait handle
								ifAbsent: [  ])
								ifNotNil: [ :loadedClassExtensions | 
									loadedClassExtensions isEmpty
										ifFalse: [ 
											loadedClassExtensions remove: loadedClassOrExtensionOrTrait.
											loadedClassExtensions isEmpty
												ifTrue: [ 
													registry
														unregisterLoadedClassExtension: loadedClassOrExtensionOrTrait
														forClass: loadedClassOrExtensionOrTrait handle ] ] ].
							loadedPackage removeLoadedClassExtension: loadedClassOrExtensionOrTrait ] ].

			compiledMethod := loadedMethod handle.
			compiledMethod _origin: nil.

			packageDef := aMethodMove packageAfter.
			classOrExtensionOrTraitDef := aMethodMove classOrExtensionAfter.
			loadedPackage := image loadedPackageNamed: packageDef name.
			registry := (Rowan image
				symbolDictNamed:
					(aMethodMove projectAfter
						gemstoneSymbolDictNameForPackageNamed: aMethodMove packageAfter name))
				rowanSymbolDictionaryRegistry.
			loadedClassOrExtensionOrTrait := loadedPackage
				classOrExtensionForClassNamed: classOrExtensionOrTraitDef name
				ifAbsent: [ 
					classOrExtensionOrTraitDef isClassExtension
						ifTrue: [ 
							"add a new class extension on demand"
							(aMethodMove projectAfter
								gemstoneUseSessionMethodsForExtensionsForPackageNamed:
									aMethodMove packageAfter name)
								ifTrue: [ 
									| resolved |
									((resolved := self loadSymbolList
										objectNamed: classOrExtensionOrTraitDef name asSymbol) notNil
										and: [ resolved isBehavior and: [ resolved isMeta not ] ])
										ifFalse: [ 
											self
												error:
													'The extension class named ' , self className printString , ' cannot be found.' ]	"logic is not quite correct here ... need to create a lodedClassExtension for this puppy ... deferring session method work to a later date"	"https://github.com/dalehenrich/Rowan/issues/254" ]
								ifFalse: [ 
									registry
										ensureExtensionClassNamed: classOrExtensionOrTraitDef name
										existsForPackageNamed: packageDef name
										implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2 ].
							loadedPackage loadedClassExtensions at: classOrExtensionOrTraitDef name ]
						ifFalse: [ 
							self
								error:
									'internal error - no loaded class found for class '
										, classOrExtensionOrTraitDef name printString , '  in package '
										, packageDef name printString ] ].

			loadedClassOrExtensionOrTrait addLoadedMethod: loadedMethod.

			compiledMethod _origin: loadedMethod ]
]
