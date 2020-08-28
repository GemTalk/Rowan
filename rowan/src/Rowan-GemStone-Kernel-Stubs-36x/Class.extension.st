Extension { #name : 'Class' }

{ #category : '*rowan-gemstone-kernel-stubs-36x' }
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
