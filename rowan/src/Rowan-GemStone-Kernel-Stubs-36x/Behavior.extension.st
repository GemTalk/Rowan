Extension { #name : 'Behavior' }

{ #category : '*rowan-gemstone-kernel-stubs-36x' }
Behavior >> _rwCompileMethodForConditionalPackaging: sourceString symbolList: symList category: categ environmentId: environmentId ifUnpackagedDo: unpackagedBlock [
	"for now, we will only check for Rowan packaging if we are working in environment 0"
	environmentId == 0
		ifTrue: [ 
			| meth |
			meth := Rowan platform
				_parseMethod: sourceString
				category: categ
				using: symList
				environmentId: environmentId.
			meth class == GsNMethod
				ifTrue: [
| protocol |
protocol := categ asString. 
					"successful  parse of method, so let's see if the method is already packaged"
					(self
						compiledMethodAt: meth selector
						environmentId: environmentId
						otherwise: nil)
						ifNotNil: [ :theMethod | 
							| packageName |
							"existing compiled method, so let's check if it is packaged"
							packageName := theMethod rowanPackageName.
							packageName = Rowan unpackagedName
								ifTrue: [ 
									"method not already, packaged, check to see if the the 
										currentTopazPackageName has been set, if so package the method
										using the topaz package name"
									Rowan gemstoneTools topaz currentTopazPackageName
										ifNotNil: [ :topazPackageName | packageName := topazPackageName ] ].
							packageName ~= Rowan unpackagedName
								ifTrue: [ 
									"The original method was packaged (or topaz package name set), so 
										preserve the packaging"
UserGlobals at: #case3 ifAbsent: [ self halt ].
									^ self
										rwCompileMethod: sourceString
										dictionaries: symList
										category: categ
										packageName: packageName ].
self halt. ]
						ifNil: [ 
							"new method"
							"https://github.com/GemTalk/Rowan/issues/830"
							(protocol beginsWith: '*')
								ifTrue: [ 
									^ (Rowan image loadedHybridPackageNamed: protocol ifAbsent: [  ])
										ifNil: [ 
											| packageName packageConvention |
											"If the protocol begins with a '*' but does not map to an existing 
												package and the package of the class follows the Rowan convention, 
												then the method is added to the package of the class. If the class is 
												not packaged, then the method is added to the given protocol, otherwise 
												it is an error."
											packageName := self rowanPackageName.
											(packageConvention := (Rowan image loadedPackageNamed: packageName)
												loadedProject packageConvention) = 'Rowan'
												ifTrue: [ 
UserGlobals at: #case2 ifAbsent: [ self halt ].
													^ self
														rwCompileMethod: sourceString
														dictionaries: symList
														category: categ
														packageName: packageName ].
											packageName = Rowan unpackagedName
												ifTrue: [ 
													"the class is not packaged, and protocol does not map to an existing 
														package ... so we'll treat this as an unpackaged method."
self halt.
													^ unpackagedBlock value ].
self halt.
											self
												error:
													'The method category ' , protocol printString
														,
															' does not map to a known package and you may not add an unpackaged method to a class that is packaged ('
														, packageName , '),' ]
										ifNotNil: [ :hybridLoadedPackage | 
											"If the protocol begins with a '*' and maps to an existing package 
												(Rowan, Monticello or RowanHybrid convention), then the method is put 
												into that package."
UserGlobals at: #case1 ifAbsent: [ self halt ].
											^ self
												rwCompileMethod: sourceString
												dictionaries: symList
												category: categ
												packageName: hybridLoadedPackage name ] ]
								ifFalse: [ 
									"protocol DOES NOT begin with '*'"
									Rowan gemstoneTools topaz currentTopazPackageName
										ifNil: [ 
											| packageName |
											"Protocol does not begin with a '* and the currentTopazPackageName is NOT 
												set"
											packageName := self rowanPackageName.
											packageName = Rowan unpackagedName
												ifTrue: [ 
													"the class is not packaged, so just add an unpackaged method to the 
														unpackaged class"
self halt.
													^ unpackagedBlock value ]
												ifFalse: [ 
													"the class is packaged, so add the method to the package of the class"
self halt.
													^ self
														rwCompileMethod: sourceString
														dictionaries: symList
														category: categ
														packageName: packageName ] ]
										ifNotNil: [ :packageName | 
											"if protocol does not begin with a '* and the currentTopazPackageName is set"
											(Rowan loadedPackageNamed: packageName) loadedProject
												packageConvention = 'Rowan'
												ifTrue: [ 
													"If the currentTopazPackageName maps to a package that follows the 
														Rowan convention, then the method is put into that package, as 
														long as item 1 does not apply."
self halt.
													^ self
														rwCompileMethod: sourceString
														dictionaries: symList
														category: categ
														packageName: packageName ] ] ] ] ] ].
self halt.
	^ unpackagedBlock value
]

{ #category : '*rowan-gemstone-kernel-stubs-36x' }
Behavior >> _rwMoveMethod: aSelector toCategory: categoryName [
	"Move the method into <packageName>, whether or not it has been packaged.

	Preserve the Rowan packaging of the class if the class is already packaged."

	| packageName |
	packageName := self rowanPackageName.
	packageName = Rowan unpackagedName
		ifTrue: [
			"receiver not packaged, set category in class" 
			^ self _moveMethod: aSelector toCategory: categoryName ].
	^ self rwMoveMethod: aSelector toCategory: categoryName

]

{ #category : '*rowan-gemstone-kernel-stubs-36x' }
Behavior >> _rwRemoveAllMethods: baseMeths enironmentId: envId [
	| unpackagedName packagedSels |
	envId == 0
		ifFalse: [ 
			"only check for packaged methods in envId 0"
			^ self ].
	unpackagedName := Rowan unpackagedName.
	packagedSels := {}.
	baseMeths
		keysAndValuesDo: [ :sel :meth | 
			self setStamp: nil forMethod: sel.
			meth rowanPackageName ~= unpackagedName
				ifTrue: [ packagedSels add: sel ] ].
	packagedSels do: [ :sel | self rwRemoveSelector: sel ]

]
