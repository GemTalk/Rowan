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
					"successful  parse of method, so let's see if the method is already pacakged"
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
									^ self
										rwCompileMethod: sourceString
										dictionaries: symList
										category: categ
										packageName: packageName ] ]
						ifNil: [ 
							"no existing method, but if current topaz package name is set, we'll compile the 
								new method in that package"
							Rowan gemstoneTools topaz currentTopazPackageName
								ifNotNil: [ :packageName | 
									^ self
										rwCompileMethod: sourceString
										dictionaries: symList
										category: categ
										packageName: packageName ] ] ] ].
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
Behavior >> _rwRemoveAllMethods: baseMeths environmentId: envId [
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
