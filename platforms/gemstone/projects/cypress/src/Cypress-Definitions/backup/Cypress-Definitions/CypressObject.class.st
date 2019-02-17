"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressObject',
	#superclass : 'Object',
	#category : 'Cypress-Definitions'
}

{ #category : 'miscellany' }
CypressObject class >> collection: aCollection gather: aOneArgBlock [
	"Evaluate the block once for each element of aCollection. The block should answer a collection. 
	Answer an Array containing all elements of all the answered collections."

	| result |
	result := Array new.
	aCollection do: [:each | result addAll: (aOneArgBlock value: each)].
	^result

]

{ #category : 'miscellany' }
CypressObject class >> elementsIn: sourceCollection butNotIn: exclusionCollection [

	| exclusionSet |
	exclusionSet := exclusionCollection asSet.
	^sourceCollection reject: [:each | exclusionSet includes: each]

]

{ #category : 'miscellany' }
CypressObject class >> elementsInBoth: collection1 and: collection2 [
	"Set intersection generalized to any collections."

	| temporarySet |
	temporarySet := collection2 asSet.
	^collection1 select: [:each | temporarySet includes: each]

]

{ #category : 'converting' }
CypressObject class >> normalizeLineEndingsOf: aString [
	"Answer a copy of aString with the line endings normalized to
	 correspond to the current platform, regardless of how they were
	 saved. For example, Squeak uses CR and would normalize with
	 #withSqueakLineEndings, for example.

	 GemStone Smalltalk uses the Unix line ending of LF."

	| cr lf inPos outPos outString newOutPos indexLF indexCR |
	cr := Character cr.
	indexCR := aString indexOf: cr startingAt: 1.
	indexCR = 0 ifTrue: [^aString].
	lf := Character lf.
	indexLF := aString indexOf: lf startingAt: 1.
	indexLF = 0 ifTrue: [^aString copyReplacing: cr with: lf].
	inPos := outPos := 1.
	outString := String new: aString size.
	
	["check if next CR is before next LF or if there are no more LF"
	(indexLF = 0 or: [indexCR < indexLF])
		ifTrue: 
			[newOutPos := outPos + 1 + indexCR - inPos.
			outString
				replaceFrom: outPos
				to: newOutPos - 2
				with: aString
				startingAt: inPos.
			outString at: newOutPos - 1 put: lf.
			outPos := newOutPos.
			1 + indexCR = indexLF
				ifTrue: 
					["Caught a CR-LF pair"
					inPos := 1 + indexLF.
					indexLF := aString indexOf: lf startingAt: inPos]
				ifFalse: [inPos := 1 + indexCR].
			indexCR := aString indexOf: cr startingAt: inPos]
		ifFalse: 
			[newOutPos := outPos + 1 + indexLF - inPos.
			outString
				replaceFrom: outPos
				to: newOutPos - 1
				with: aString
				startingAt: inPos.
			outPos := newOutPos.
			inPos := 1 + indexLF.
			indexLF := aString indexOf: lf startingAt: inPos].
	indexCR = 0]
			whileFalse.

	"no more CR line endings. copy the rest"
	newOutPos := outPos + (aString size - inPos + 1).
	outString
		replaceFrom: outPos
		to: newOutPos - 1
		with: aString
		startingAt: inPos.
	^outString copyFrom: 1 to: newOutPos - 1

]

{ #category : 'accessing' }
CypressObject >> allClasses [

	| classes |
	classes := Array new.
	self symbolList
		do: [:dict | classes addAll: (dict select: [:each | each isBehavior])].
	^classes

]

{ #category : 'private' }
CypressObject >> anyElementOf: aCollection ifEmpty: aBlock [

	aCollection do: [:each | ^each].
	^aBlock value

]

{ #category : 'accessing' }
CypressObject >> classesInPackageNamed: aString [

	| packageName classes |
	packageName := aString asLowercase.
	classes := Array new.
	self symbolList do: 
			[:dict |
			classes
				addAll: (dict select: 
							[:each |
							each isBehavior and: 
									[| candidateName |
									candidateName := each category asLowercase.
									candidateName = packageName
										or: [(candidateName indexOfSubCollection: packageName , '-' startingAt: 1 ifAbsent: [ 0 ]) = 1]]])].
	^classes sortAscending: #('name')

]

{ #category : 'miscellany' }
CypressObject >> collection: aCollection gather: aOneArgBlock [
	"Evaluate the block once for each element of aCollection. The block should answer a collection. 
	Answer an Array containing all elements of all the answered collections."

	^self class collection: aCollection gather: aOneArgBlock

]

{ #category : 'initializing' }
CypressObject >> defaultSymbolDictionaryName [
  "Name of the SymbolDictionary where new classes should be installed"

  ^ #'UserGlobals'

]

{ #category : 'sorting' }
CypressObject >> determineClassHierarchicalOrder: someClasses [
	"Returns an ordered collection of the specified classes such that
	 hierarchical dependencies come first."
	"Not sure whether we ever get non-behaviors. 
	The previous, more complex, version of this method contained this filter."

	| order toBeOrdered processed aClass |
	toBeOrdered := (someClasses select: [:each | each isBehavior])
				asIdentitySet.
	order := OrderedCollection new.
	processed := IdentitySet new.
	[(aClass := self anyElementOf: toBeOrdered ifEmpty: [nil]) isNil]
		whileFalse: 
			[self
				orderBySuperclass: aClass
				from: toBeOrdered
				into: order
				ignoring: processed].
	^order

]

{ #category : 'miscellany' }
CypressObject >> elementsIn: sourceCollection butNotIn: exclusionCollection [

	^self class elementsIn: sourceCollection butNotIn: exclusionCollection

]

{ #category : 'miscellany' }
CypressObject >> elementsInBoth: collection1 and: collection2 [
	"Set intersection generalized to any collections."

	^self class elementsInBoth: collection1 and: collection2

]

{ #category : 'initializing' }
CypressObject >> initialize [
	"Placeholder: #initialize is not defined by Object in GemStone Smalltalk."

]

{ #category : 'converting' }
CypressObject >> normalizeLineEndingsOf: aString [
	"Answer a copy of aString with the line endings normalized to
	 correspond to the current platform, regardless of how they were
	 saved. For example, Squeak uses CR and would normalize with
	 #withSqueakLineEndings, for example."

	^self class normalizeLineEndingsOf: aString.

]

{ #category : 'private' }
CypressObject >> orderBySuperclass: aClass from: toBeOrdered into: order ignoring: processed [
	"Private. Add to 'order', superclasses first, aClass and any of its superclasses 
	that appear in 'toBeOrdered' but do not appear in 'processed'.
	Remove from 'toBeOrdered' any class added to 'ordered'.
	Any class seen, add to 'processed' whether or not added to 'order'."

	| superclass |
	superclass := aClass superclass.
	superclass isNil | (processed includes: superclass)
		ifFalse: 
			[self
				orderBySuperclass: superclass
				from: toBeOrdered
				into: order
				ignoring: processed].
	processed add: aClass.
	(toBeOrdered includes: aClass)
		ifTrue: 
			[toBeOrdered remove: aClass.
			order add: aClass]

]

{ #category : 'printing' }
CypressObject >> printDetailsOn: aStream [

]

{ #category : 'printing' }
CypressObject >> printOn: aStream [

	| className |
	className := self class name.
	aStream
		nextPutAll: (className first isVowel ifTrue:[ 'an ' ] ifFalse:[ 'a ' ]);
		nextPutAll: className;
		nextPutAll: '('.
	self printDetailsOn: aStream.
	aStream nextPutAll: ')'.

]

{ #category : 'accessing' }
CypressObject >> resolveGlobalNamed: aString [

	^self resolveGlobalNamed: aString
		or: [CypressError signal: 'Could not resolve global named ' , aString printString]

]

{ #category : 'accessing' }
CypressObject >> resolveGlobalNamed: aString or: aBlock [

	^((System myUserProfile resolveSymbol: aString) ifNil: [^aBlock value])
		value

]

{ #category : 'converting' }
CypressObject >> stringForVariables: variableList [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	variableList do: [:each | stream nextPutAll: each]
		separatedBy: [stream space].
	^stream contents

]

{ #category : 'accessing' }
CypressObject >> symbolDictionaryForClassNamed: aString [
  "Answer the SymbolDictionary containing the named class.
	 If there are multiple answers, answer the first.
	 If there are no answers (i.e., the class does not exist), put it in UserGlobals."

  ^ self
    symbolDictionaryForClassNamed: aString
    or: [ System myUserProfile objectNamed: self defaultSymbolDictionaryName ]

]

{ #category : 'accessing' }
CypressObject >> symbolDictionaryForClassNamed: aString or: aBlock [
	"Answer the SymbolDictionary containing the named class.
	 If there are multiple answers, answer the first.
	 If there are no answers (i.e., the class does not exist), answer
	 the result of evaluating aBlock."

	^self symbolList asArray
		detect: [:each | each anySatisfy: [:every | every isBehavior and: [every name asString = aString asString]]]
		ifNone: aBlock

]

{ #category : 'accessing' }
CypressObject >> symbolList [
  ^ System myUserProfile symbolList

]
