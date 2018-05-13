Extension { #name : 'SymbolDictionary' }

{ #category : '*rowan-gemstone-kernel' }
SymbolDictionary >> _rowanCloneSymbolDictionaryNamed: aSymbol symbolList: symbolList [

	"return a new symbol dictionary containing copies of all of the classes in the receiver ... the state of each class should be the same with 
		respect to class intance variables and class variables ... similar to what happens when a new version of a class is created, except the
		copied classes are not in the class history of the original class"

	| clone |
	clone := self class new.
	clone name: aSymbol.
	self keysAndValuesDo: [:oldClassName :oldClass |
		oldClass isBehavior
			ifTrue: [ 
				| newClass hist x |
				hist := oldClass classHistory copy.	"make copy ... leverage the new class version behavior, which preserves a lot of state, without 
																	adding the new class to old class' class hitory"
				newClass := oldClass superclass 
					_subclass: oldClassName asString 
					instVarNames: oldClass instVarNames
					format: oldClass format  
					constraints: #() 
					classVars: oldClass classVarNames
					classInstVars: oldClass class instVarNames
					poolDictionaries: #()
					inDictionary: nil
					inClassHistory: hist
					description: ''
					options: oldClass _nonInheritedOptions.
				(x := newClass _copyMethodsAndVariablesFrom: oldClass except: nil dictionaries: symbolList)
					isEmpty 
						ifFalse: [ self error: 'failed to compile methods in the cloned class ', oldClassName asString printString ].
				clone at: oldClassName put: newClass ]].
	^ clone

]

{ #category : '*rowan-gemstone-kernel' }
SymbolDictionary >> rowanSymbolDictionaryRegistry [

	"answer the RwGsSymbolDictionaryRegistry instance installed in the receiver, otherwise answer nil"

	^ self at: #'RwSymbolDictionaryRegistry' ifAbsent: [  ]

]
