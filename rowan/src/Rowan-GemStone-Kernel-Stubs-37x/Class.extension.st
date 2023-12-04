Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-kernel-stubs-37x' }
Class >> _rwCategory: newCategory [
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

]

{ #category : '*rowan-gemstone-kernel-stubs-37x' }
Class >> _rwCreateSubclass: aString instVarNames: anArrayOfInstvarNames classVars: anArrayOfClassVars classInstVars: anArrayOfClassInstVars poolDictionaries: anArrayOfPoolDicts inDictionary: aDictionary newVersionOf: oldClass description: aDescription options: optionsArray ifUnpackagedDo: unpackagedBlock [
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

]
