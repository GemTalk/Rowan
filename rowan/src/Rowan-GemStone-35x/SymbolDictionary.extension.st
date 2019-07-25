Extension { #name : 'SymbolDictionary' }

{ #category : '*rowan-gemstone-35x' }
SymbolDictionary >> _rowanCloneSymbolDictionaryNamed: aSymbol symbolList: symbolList [

	"return a new symbol dictionary containing copies of all of the classes in the receiver ... the state of each class should be the same with 
		respect to class intance variables and class variables ... similar to what happens when a new version of a class is created, except the
		copied classes are not in the class history of the original class"

	"it is expected that the receiver is in the <symbolList>"

	| toBeOrdered order clonedSymDict processed aClass clonedClasses |
    "order the list of classes in the receiver in superclass order, so that superclasses are cloned before the subclasses are cloned"
	toBeOrdered := (self values select: [:each | each isBehavior ]) asIdentitySet.
	order := OrderedCollection new.
	processed := IdentitySet new.
	[ (aClass := RwGsPatchSet_254 _anyElementOf: toBeOrdered ifEmpty: [ nil ]) isNil ]
		whileFalse: [ 
			RwGsPatchSet_254
				_orderBySuperclass: aClass
				from: toBeOrdered
				into: order
				ignoring: processed ].
	"create clone and clone all of the classes before compiling methods"
	clonedSymDict := self class new.
	clonedSymDict name: aSymbol.
	clonedClasses := OrderedCollection new.
	[
		symbolList add: clonedSymDict before: self. "install clone after receiver, so that methods will be compiled and reference the cloned classes"
		"clone the body of classes before compiling methods"
		order do: [:oldClass |
			| oldClassName clonedClass hist superclass |
			oldClassName := oldClass name asSymbol.
			hist := oldClass classHistory copy.	"make copy ... leverage the new class version behavior, which preserves a lot of state, without 
																adding the new class to old class' class hitory"
			superclass := symbolList objectNamed: oldClass superclass name. "look up superclass, in case superclass has been cloned"
			clonedClass :=  superclass
				_subclass: oldClassName asString 
				instVarNames: oldClass instVarNames
				format: oldClass format  
				classVars: oldClass classVarNames
				classInstVars: oldClass class instVarNames
				poolDictionaries: #()
				inDictionary: nil
				inClassHistory: hist
				description: ''
				options: oldClass _optionsArrayForDefinition .
			clonedSymDict at: oldClassName put: clonedClass.
			clonedClasses add: {clonedClass. oldClass} ].
			"compile methods in cloned class"
			clonedClasses do: [:ar | | x clonedClass oldClass |
				clonedClass := ar at: 1.
				oldClass := ar at: 2.
				(x := clonedClass _rowanCopyMethodsAndVariablesFrom: oldClass dictionaries: symbolList)
					isEmpty 
						ifFalse: [ self error: 'failed to compile methods in the cloned class ', oldClass name asString printString ] ] ] 
		ensure: [ 
			"do not leave clone in the symbol list"
			symbolList remove: clonedSymDict ifAbsent: [] ].
	^ clonedSymDict
]
