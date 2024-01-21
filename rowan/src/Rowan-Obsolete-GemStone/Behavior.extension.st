Extension { #name : 'Behavior' }

{ #category : '*rowan-obsolete-gemstone' }
Behavior >> rwCompileMethod: sourceString category: aCategoryString [

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategoryString asString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
]

{ #category : '*rowan-obsolete-gemstone' }
Behavior >> rwCompileMethod: sourceString category: aCategoryString packageName: packageName [

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		inProtocol: aCategoryString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		inPackageNamed: packageName
]

{ #category : '*rowan-obsolete-gemstone' }
Behavior >> rwCompileMethod: sourceString dictionaries: aSymbolList category: aCategoryString packageName: packageName [
	"Rowan version of Behavior >> #compileMethod:dictionaries:category:environmentId:"

	"This compiles some source code for the receiver.  The first argument,
 sourceString, is the string of source code to be compiled.  The second
 argument is a SymbolList to be used in parsing, along with the list of all
 class variables and pool dictionaries for the receiver and all of its
 superclasses.  The third argument (a String) indicates the method's category.

 sourceString must be a kind of String or DoubleByteString.  Instances of
 JapaneseString are not supported as source strings.  String literals
 ('abc') are generated as instances of the class of sourceString,
 unless sourceString is a Symbol, in which case 'abc' produces a String.
 If sourceString is a DoubleByteSymbol, 'abc' produces a DoubleByteString.

 anEnvironmentId must be a SmallInteger >= 0 and <= 16rFFFF.
 0 denotes the base Smalltalk image.  1 was reserved for use by Ruby .

 If there are no errors, this adds the resulting compiled method to the
 receiver's method dictionary and returns that method,
 otherwise signals a CompileError .
 A CompileWarning may be signaled, after adding the new method
 to a receiver's method dictionary."

	^ Rowan projectTools browser
		addOrUpdateMethod: sourceString
		dictionaries: aSymbolList
		inProtocol: aCategoryString
		forClassNamed: self thisClass name asString
		isMeta: self isMeta
		inPackageNamed: packageName
]
