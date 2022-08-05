! Class extensions for 'Behavior'

!		Instance methods for 'Behavior'

category: '*rowan-gemstone-kernel-stubs-36x'
method: Behavior
_rwCompileMethodForConditionalPackaging: sourceString symbolList: symList category: categ environmentId: environmentId ifUnpackagedDo: unpackagedBlock
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
%

category: '*rowan-gemstone-kernel-stubs-36x'
method: Behavior
_rwMoveMethod: aSelector toCategory: categoryName
	"Move the method into <packageName>, whether or not it has been packaged.

	Preserve the Rowan packaging of the class if the class is already packaged."

	| packageName |
	packageName := self rowanPackageName.
	packageName = Rowan unpackagedName
		ifTrue: [
			"receiver not packaged, set category in class" 
			^ self _moveMethod: aSelector toCategory: categoryName ].
	^ self rwMoveMethod: aSelector toCategory: categoryName
%

category: '*rowan-gemstone-kernel-stubs-36x'
method: Behavior
_rwRemoveAllMethods: baseMeths enironmentId: envId
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
%

! Class extensions for 'Class'

!		Instance methods for 'Class'

category: '*rowan-gemstone-kernel-stubs-36x'
method: Class
_rwCategory: newCategory
"Sets the classCategory variable of the receiver.
 The argument should be a kind of CharacterCollection or nil.

 Preserve the Rowan packaging of the class if the class is already packaged."

	| packageName |
	packageName := self rowanPackageName.
	packageName = Rowan unpackagedName
		ifTrue: [
			"receiver not packaged, set category in class" 
			^ self _category: newCategory ].
	^ self rwCategory: newCategory
%

category: '*rowan-gemstone-kernel-stubs-36x'
method: Class
_rwCreateSubclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary newVersionOf: oldClass description: aDescription options: optionsArray ifUnpackagedDo: unpackagedBlock
	| descr newClass |
	descr := aDescription.
	oldClass
		ifNotNil: [
			oldClass rowanPackageName ~= Rowan unpackagedName
				ifTrue: [ 
					"The oldClass is packaged, so preserve the packaging for the new class version"
					newClass := self
						rwSubclass: aString
						instVarNames: anArrayOfInstvarNames
						classVars: anArrayOfClassVars
						classInstVars: anArrayOfClassInstVars
						poolDictionaries: anArrayOfPoolDicts
						inDictionary: aDictionary
						newVersionOf: oldClass
						category: oldClass _classCategory
						packageName: oldClass rowanPackageName
						options: optionsArray.
					descr
						ifNil: [ 
							descr := [ oldClass commentForFileout ]
								on: Error
								do: [  ] ].
					newClass rwComment: descr.
					^ newClass ] ].
	Rowan gemstoneTools topaz currentTopazPackageName
		ifNotNil: [ :packageName | 
			newClass := self
				rwSubclass: aString
				instVarNames: anArrayOfInstvarNames
				classVars: anArrayOfClassVars
				classInstVars: anArrayOfClassInstVars
				poolDictionaries: anArrayOfPoolDicts
				inDictionary: aDictionary
				newVersionOf: oldClass
				category: nil
				packageName: packageName
				options: optionsArray.
			(descr isNil and: [ oldClass notNil ])
				ifTrue: [ 
					descr := [ oldClass commentForFileout ]
						on: Error
						do: [  ] ].
			newClass rwComment: descr.
			^ newClass ].
	^ unpackagedBlock value
%

