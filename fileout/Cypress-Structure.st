" Package: Cypress-Structure"!


" Class Declarations "!

(Error
	subclass: 'CypressJsonError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Structure';
		comment: ''!

(Object
	subclass: 'CypressJsonParser'
	instVarNames: #( stream )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Structure';
		comment: ''!

(Object
	subclass: 'GsGeneralDependencySorter'
	instVarNames: #( candidates dependsOnConverter dependentConverter individualDependencyMap dependencyGraphs candidateAliasMap )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Structure';
		comment: ''!

(CypressObject
	subclass: 'CypressStructure'
	instVarNames: #( name properties packageStructure )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Structure';
		comment: ''!

(CypressStructure
	subclass: 'CypressClassStructure'
	instVarNames: #( instanceMethods classMethods comment isClassExtension )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Structure';
		comment: ''!

(CypressStructure
	subclass: 'CypressPackageStructure'
	instVarNames: #( classes extensions )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Structure';
		comment: ''!

(CypressStructure
	subclass: 'CypressMethodStructure'
	instVarNames: #( source isMetaclass classStructure )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Structure';
		comment: ''!

" Class Implementation for CypressJsonError"!

" Class Implementation for CypressJsonParser"!

" ------------------- Class methods for CypressJsonParser"!

!CypressJsonParser class methodsFor: 'instance creation'!
new

	CypressJsonError signal: 'Instantiate the parser with a stream.'! !
!CypressJsonParser class methodsFor: 'instance creation'!
on: aStream
	^ self basicNew initializeOn: aStream
! !
!CypressJsonParser class methodsFor: 'accessing'!
parse: aString
	^ self parseStream: aString readStream
! !
!CypressJsonParser class methodsFor: 'accessing'!
parseStream: aStream
	^ (self on: aStream) parse
! !
" ------------------- Instance methods for CypressJsonParser"!

!CypressJsonParser methodsFor: 'adding'!
addProperty: anAssociation to: anObject
	"Add the property anAssociation described with key and value to anObject. Subclasses might want to refine this implementation."
	
	^ anObject 
		add: anAssociation;
		yourself
! !
!CypressJsonParser methodsFor: 'adding'!
addValue: anObject to: aCollection
	"Add anObject to aCollection. Subclasses might want to refine this implementation."

	^ aCollection copyWith: anObject
! !
!CypressJsonParser methodsFor: 'creating'!
createArray
	"Create an empty collection. Subclasses might want to refine this implementation."

	^ Array new
! !
!CypressJsonParser methodsFor: 'creating'!
createFalse
	"Create the false literal. Subclasses might want to refine this implementation."
	
	^ false
! !
!CypressJsonParser methodsFor: 'creating'!
createNull
	"Create the null literal. Subclasses might want to refine this implementation."

	^ nil
! !
!CypressJsonParser methodsFor: 'creating'!
createObject
	"Create an empty object. Subclasses might want to refine this implementation."
	
	^ Dictionary new
! !
!CypressJsonParser methodsFor: 'creating'!
createProperty: aKey with: aValue
	"Create an empty attribute value pair. Subclasses might want to refine this implementation."
	
	^ aKey -> aValue
! !
!CypressJsonParser methodsFor: 'creating'!
createString: aString
	"Create a string literal. Subclasses might want to refine this implementation."

	^ aString
! !
!CypressJsonParser methodsFor: 'creating'!
createTrue
	"Create the true literal. Subclasses might want to refine this implementation."

	^ true
! !
!CypressJsonParser methodsFor: 'private'!
expect: aString
	"Expects aString and consume input, throw an error otherwise."

	^(self match: aString)
		ifFalse: [CypressJsonError signal: aString , ' expected']! !
!CypressJsonParser methodsFor: 'initialization'!
initializeOn: aStream
	stream := aStream
! !
!CypressJsonParser methodsFor: 'private'!
match: aString
	"Tries to match aString, consume input and answer true if successful."
	
	| position |
	position := stream position.
	aString do: [ :each |
		(stream atEnd or: [ stream next ~= each ]) ifTrue: [ 
			stream position: position.
			^ false ] ].
	self whitespace.
	^ true
! !
!CypressJsonParser methodsFor: 'parsing'!
parse

	| result |
	result := self
				whitespace;
				parseValue.
	stream atEnd ifFalse: [CypressJsonError signal: 'end of input expected'].
	^result! !
!CypressJsonParser methodsFor: 'parsing'!
parseArray

	| result |
	self expect: '['.
	result := self createArray.
	(self match: ']') ifTrue: [^result].
	[stream atEnd] whileFalse: 
			[result := self addValue: self parseValue to: result.
			(self match: ']') ifTrue: [^result].
			self expect: ','].
	CypressJsonError signal: 'end of array expected'! !
!CypressJsonParser methodsFor: 'parsing-internal'!
parseCharacter
	| char |
	(char := stream next) = $\ 
		ifFalse: [ ^ char ].
	(char := stream next) = $" 
		ifTrue: [ ^ char ].
	char = $\
		ifTrue: [ ^ char ].
	char = $/
		ifTrue: [ ^ char ].
	char = $b
		ifTrue: [ ^ Character backspace ].
	char = $f
		ifTrue: [ ^ Character newPage ].
	char = $n
		ifTrue: [ ^ Character lf ].
	char = $r
		ifTrue: [ ^ Character cr ].
	char = $t
		ifTrue: [ ^ Character tab ].
	char = $u
		ifTrue: [ ^ self parseCharacterHex ].
	CypressJsonError signal: 'invalid escape character \' , (String with: char)
! !
!CypressJsonParser methodsFor: 'parsing-internal'!
parseCharacterHex
	| value |
	value := self parseCharacterHexDigit.
	3 timesRepeat: [ value := (value << 4) + self parseCharacterHexDigit ].
	^ Character codePoint: value
! !
!CypressJsonParser methodsFor: 'parsing-internal'!
parseCharacterHexDigit
    | digit |
    stream atEnd
        ifFalse: [ 
            digit := stream next codePoint.
            (digit between: 48 and: 57)
                ifTrue: [ ^ digit - 48 ].	"$0"	"$9"
            (digit between: 65 and: 70)
                ifTrue: [ ^ digit - 55 ].	"$A"	"$F"
            (digit between: 97 and: 102)
                ifTrue: [ ^ digit - 87 ]	"$a"	"$f" ].
    CypressJsonError signal: 'hex-digit expected'
! !
!CypressJsonParser methodsFor: 'parsing-internal'!
parseNumber
	| negated number |
	negated := stream peek = $-.
	negated ifTrue: [ stream next ].
	number := self parseNumberInteger.
	(stream peek = $.) ifTrue: [
		stream next. 
		number := number + self parseNumberFraction ].
	(stream peek = $e or: [ stream peek = $E ]) ifTrue: [
		stream next.
		number := number * self parseNumberExponent ].
	negated ifTrue: [ number := number negated ].
	self whitespace.
	^ number
! !
!CypressJsonParser methodsFor: 'parsing-internal'!
parseNumberExponent
    | number negated |
    number := 0.
    negated := stream peek = $-.
    (negated or: [ stream peek = $+ ])
        ifTrue: [ stream next ].
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].
    negated
        ifTrue: [ number := number negated ].
    ^ 10 raisedTo: number
! !
!CypressJsonParser methodsFor: 'parsing-internal'!
parseNumberFraction
    | number power |
    number := 0.
    power := 1.0.
    [ stream atEnd not and: [ stream peek isDigit ] ]
        whileTrue: [ 
            number := 10 * number + (stream next codePoint - 48).
            power := power * 10.0 ].
    ^ number / power
! !
!CypressJsonParser methodsFor: 'parsing-internal'!
parseNumberInteger
    | number |
    number := 0.
    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].
    ^ number
! !
!CypressJsonParser methodsFor: 'parsing'!
parseObject

	| result |
	self expect: '{'.
	result := self createObject.
	(self match: '}') ifTrue: [^result].
	[stream atEnd] whileFalse: 
			[result := self addProperty: self parseProperty to: result.
			(self match: '}') ifTrue: [^result].
			self expect: ','].
	CypressJsonError signal: 'end of object expected'! !
!CypressJsonParser methodsFor: 'parsing-internal'!
parseProperty
	| name value |
	name := self parseString.
	self expect: ':'.
	value := self parseValue.
	^ self createProperty: name with: value.
! !
!CypressJsonParser methodsFor: 'parsing-internal'!
parseString
	| result |
	self expect: '"'.
	result := WriteStream on: String new.
	[ stream atEnd or: [ stream peek = $" ] ] 
		whileFalse: [ result nextPut: self parseCharacter ].
	^ self expect: '"'; createString: result contents
! !
!CypressJsonParser methodsFor: 'parsing'!
parseValue
	| char |
	stream atEnd ifFalse: [ 
		char := stream peek.
		char = ${
			ifTrue: [ ^ self parseObject ].
		char = $[
			ifTrue: [ ^ self parseArray ].
		char = $"
			ifTrue: [ ^ self parseString ].
		(char = $- or: [ char between: $0 and: $9 ])
			ifTrue: [ ^ self parseNumber ].
		(self match: 'true')
			ifTrue: [ ^ self createTrue ].
		(self match: 'false')
			ifTrue: [ ^ self createFalse ].
		(self match: 'null')
			ifTrue: [ ^ self createNull ] ].
	CypressJsonError signal: 'invalid input'
! !
!CypressJsonParser methodsFor: 'private'!
whitespace
	"Strip whitespaces from the input stream."

	[ stream atEnd not and: [ stream peek isSeparator ] ]
		whileTrue: [ stream next ]
! !
" Class Implementation for GsGeneralDependencySorter"!

" ------------------- Class methods for GsGeneralDependencySorter"!

!GsGeneralDependencySorter class methodsFor: 'instance creation'!
on: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock
	"Create an instance of the receiver capable for sorting the dependencies of someCandidates.
	 aOneArgBlock is used to evaluate the key of the object depended on for a candidate.
	 anotherOneArgBlock is used to evaluate the key of the candidate itself."

	^self new
		initializeOn: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock;
		yourself.
! !
" ------------------- Instance methods for GsGeneralDependencySorter"!

!GsGeneralDependencySorter methodsFor: 'sorting - private'!
determineGraphRoots

	^dependencyGraphs
		selectAssociations: [:each | (candidateAliasMap includesKey: each key) not]! !
!GsGeneralDependencySorter methodsFor: 'initializing - private'!
initializeOn: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock

	candidates := someCandidates.
	dependsOnConverter := aOneArgBlock.
	dependentConverter := anotherOneArgBlock.
	individualDependencyMap := Dictionary new.
	dependencyGraphs := Dictionary new.
	candidateAliasMap := Dictionary new! !
!GsGeneralDependencySorter methodsFor: 'sorting'!
inOrder

	| sorted |
	sorted := OrderedCollection new.
	self mapCandidatesIntoGraphs.
	self determineGraphRoots
		do: [:each | self transcribeGraph: each into: sorted].
	^sorted.
! !
!GsGeneralDependencySorter methodsFor: 'sorting - private'!
mapCandidatesIntoGraphs

	| dependsOnKey dependentKey |
	candidates do: 
			[:each |
			| individualDependency |
			dependsOnKey := dependsOnConverter value: each.
			dependentKey := dependentConverter value: each.
			candidateAliasMap at: dependentKey put: each.
			individualDependencyMap at: dependsOnKey ifAbsentPut: [Dictionary new].
			individualDependencyMap at: dependentKey ifAbsentPut: [Dictionary new].
			individualDependency := individualDependencyMap
						associationAt: dependsOnKey.
			(dependencyGraphs includesKey: dependsOnKey)
				ifFalse: [dependencyGraphs add: individualDependency].
			individualDependency value
				add: (individualDependencyMap associationAt: dependentKey)]! !
!GsGeneralDependencySorter methodsFor: 'sorting - private'!
transcribeGraph: subtree into: sorted

	subtree keysAndValuesDo: [:name :subsubtree |
		sorted add: (candidateAliasMap at: name).
		self transcribeGraph: subsubtree into: sorted.
	].
! !
" Class Implementation for CypressStructure"!

" ------------------- Class methods for CypressStructure"!

!CypressStructure class methodsFor: 'instance creation'!
named: aString

	^(self new)
		name: aString;
		yourself! !
" ------------------- Instance methods for CypressStructure"!

!CypressStructure methodsFor: 'accessing'!
name

	^name
! !
!CypressStructure methodsFor: 'accessing'!
name: aString 

	name := aString
! !
!CypressStructure methodsFor: 'accessing'!
packageStructure
	^packageStructure
! !
!CypressStructure methodsFor: 'accessing'!
packageStructure: aCypressPackageStructure
	packageStructure := aCypressPackageStructure
! !
!CypressStructure methodsFor: 'printing'!
printDetailsOn: aStream

	aStream nextPutAll: self name.
! !
!CypressStructure methodsFor: 'accessing'!
properties

	properties ifNil: [ properties := Dictionary new ].
	^properties
! !
!CypressStructure methodsFor: 'accessing'!
properties: aDictionary

	properties := aDictionary
! !
" Class Implementation for CypressClassStructure"!

" ------------------- Class methods for CypressClassStructure"!

!CypressClassStructure class methodsFor: 'instance creation'!
fromClassDefinition: classDefinition

	^self new
		fromClassDefinition: classDefinition;
		yourself
! !
" ------------------- Instance methods for CypressClassStructure"!

!CypressClassStructure methodsFor: 'converting'!
asCypressClassDefinition

	self isClassExtension ifTrue: [^CypressError signal: 'Extensions cannot have class definitions'].
	^CypressClassDefinition
		name: self className
		superclassName: self superclassName
		category: self category
		instVarNames: self instanceVariableNames
		classInstVarNames: self classInstanceVariableNames
		classVarNames: self classVariableNames
		poolDictionaryNames: self poolDictionaryNames
		comment: self comment
		subclassType: self subclassType! !
!CypressClassStructure methodsFor: 'accessing'!
category

	^self properties
		at: 'category'
		ifAbsent: [self packageStructure packageName]
! !
!CypressClassStructure methodsFor: 'accessing'!
category: aString

	^self properties at: 'category' put: aString! !
!CypressClassStructure methodsFor: 'accessing'!
classInstanceVariableNames

	^self properties at: 'classinstvars' ifAbsent: [#()]! !
!CypressClassStructure methodsFor: 'accessing'!
classInstanceVariableNames: someStrings

	^self properties at: 'classinstvars' put: someStrings! !
!CypressClassStructure methodsFor: 'converting'!
classInstanceVariablesString

	^self stringForVariables: self classInstanceVariableNames! !
!CypressClassStructure methodsFor: 'querying'!
classMethodNamed: methodName

	^self classMethods
		at: methodName
		ifAbsentPut: [CypressMethodStructure named: methodName]
! !
!CypressClassStructure methodsFor: 'accessing'!
classMethods

	classMethods ifNil: [ classMethods := Dictionary new ].
	^classMethods
! !
!CypressClassStructure methodsFor: 'accessing'!
className

	^self name
! !
!CypressClassStructure methodsFor: 'accessing'!
classVariableNames

	^self properties at: 'classvars' ifAbsent: [#()]! !
!CypressClassStructure methodsFor: 'accessing'!
classVariableNames: someStrings

	^self properties at: 'classvars' put: someStrings! !
!CypressClassStructure methodsFor: 'converting'!
classVariablesString

	^self stringForVariables: self classVariableNames! !
!CypressClassStructure methodsFor: 'accessing'!
comment

	comment ifNil: [ comment := '' ].
	^comment
! !
!CypressClassStructure methodsFor: 'accessing'!
comment: aString

	comment := aString
! !
!CypressClassStructure methodsFor: 'initialization'!
fromClassDefinition: classDefinition

	self
		isClassExtension: false;
		name: classDefinition name;
		category: classDefinition category;
		comment: classDefinition comment;
		superclassName: classDefinition superclassName;
		instanceVariableNames: classDefinition instVarNames;
		classInstanceVariableNames: classDefinition classInstVarNames;
		classVariableNames: classDefinition classVarNames;
		poolDictionaryNames: classDefinition poolDictionaryNames;
		subclassType: classDefinition subclassType.
! !
!CypressClassStructure methodsFor: 'querying'!
instanceMethodNamed: methodName

	^self instanceMethods
		at: methodName 
		ifAbsentPut: [CypressMethodStructure named: methodName]
! !
!CypressClassStructure methodsFor: 'accessing'!
instanceMethods

	instanceMethods ifNil: [ instanceMethods := Dictionary new ].
	^instanceMethods
! !
!CypressClassStructure methodsFor: 'accessing'!
instanceVariableNames

	^self properties at: 'instvars' ifAbsent: [#()]
! !
!CypressClassStructure methodsFor: 'accessing'!
instanceVariableNames: someStrings

	^self properties at: 'instvars' put: someStrings
! !
!CypressClassStructure methodsFor: 'converting'!
instanceVariablesString

	^self stringForVariables: self instanceVariableNames! !
!CypressClassStructure methodsFor: 'accessing'!
isClassExtension

        isClassExtension ifNil: [ isClassExtension := true ].
        ^isClassExtension
! !
!CypressClassStructure methodsFor: 'accessing'!
isClassExtension: aBoolean

	isClassExtension := aBoolean
! !
!CypressClassStructure methodsFor: 'accessing'!
name

	^self properties at: 'name'
! !
!CypressClassStructure methodsFor: 'accessing'!
name: aString

	self properties at: 'name' put: aString
! !
!CypressClassStructure methodsFor: 'converting'!
poolDictionariesString

	^self stringForVariables: self poolDictionaryNames! !
!CypressClassStructure methodsFor: 'accessing'!
poolDictionaryNames

	^self properties at: 'pools' ifAbsent: [#()]! !
!CypressClassStructure methodsFor: 'accessing'!
poolDictionaryNames: someStrings

	^self properties at: 'pools' put: someStrings! !
!CypressClassStructure methodsFor: 'accessing'!
subclassType

	^self properties at: '_gs_subclassType' ifAbsent: ['']
! !
!CypressClassStructure methodsFor: 'accessing'!
subclassType: aString

	aString isEmpty
		ifTrue: [self properties removeKey: '_gs_subclassType' ifAbsent: []]
		ifFalse: [self properties at: '_gs_subclassType' put: aString]! !
!CypressClassStructure methodsFor: 'accessing'!
superclassName

	^self properties at: 'super'
! !
!CypressClassStructure methodsFor: 'accessing'!
superclassName: aString

	^self properties at: 'super' put: aString
! !
" Class Implementation for CypressPackageStructure"!

" ------------------- Class methods for CypressPackageStructure"!

!CypressPackageStructure class methodsFor: 'instance creation'!
fromPackage: aCypressPackageDefinition

	^(self new) 
		fromPackage: aCypressPackageDefinition;
		yourself
! !
" ------------------- Instance methods for CypressPackageStructure"!

!CypressPackageStructure methodsFor: 'accessing'!
classes

	classes ifNil: [ classes := OrderedCollection new ].
	^classes
! !
!CypressPackageStructure methodsFor: 'accessing'!
extensions

	extensions ifNil: [ extensions := OrderedCollection new ].
	^extensions
! !
!CypressPackageStructure methodsFor: 'initialization'!
fromPackage: aCypressPackageDefinition

	| snapshot classMap classDefinitions classStructure |
	snapshot := aCypressPackageDefinition snapshot.
	name := aCypressPackageDefinition name, self packageExtension.
	properties := Dictionary new.
	classDefinitions := OrderedCollection new.
	classMap := Dictionary new.
	snapshot definitions do: [:definition |  
			definition 
				classDefinition: [:classDefinition |  classDefinitions add: classDefinition ] 
				methodDefinition: [:methodDefinition | 
					(classMap 
						at: methodDefinition className 
						ifAbsentPut: [Set new]) 
							add: methodDefinition. ]].
	classDefinitions do: [:classDefinition |
		classStructure := (CypressClassStructure fromClassDefinition: classDefinition)
			packageStructure: self.
		(classMap removeKey: classDefinition className ifAbsent: [#()]) do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self classes add: classStructure ].
	classMap keysAndValuesDo: [:className :methods |
		classStructure := (CypressClassStructure named: className)
			isClassExtension: true;
			packageStructure: self.
		methods do: [:methodDefinition | | methodStructure |
			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)
				packageStructure: self;
				classStructure: classStructure.
			(methodDefinition
				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 
				classMethod: [:classMethod | classStructure classMethods ])
					at: methodDefinition selector
					put: methodStructure ].
		self extensions add: classStructure ].
! !
!CypressPackageStructure methodsFor: 'accessing'!
packageExtension

	^self packageExtensionOr: ['.package' ]
! !
!CypressPackageStructure methodsFor: 'accessing'!
packageExtensionOr: aBlock

	^self properties at: 'extension' ifAbsent: aBlock
! !
!CypressPackageStructure methodsFor: 'accessing'!
packageName

	^self name copyWithoutSuffix: self packageExtension
! !
!CypressPackageStructure methodsFor: 'accessing'!
packageStructure
	^self
! !
!CypressPackageStructure methodsFor: 'snapshotting'!
snapshot
	| definitions |
	definitions := OrderedCollection new.
	self classes do: [:classStructure |
        	definitions add: classStructure asCypressClassDefinition.
                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	self extensions do: [:classStructure |
                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ].
                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |
			definitions add: methodStructure asCypressMethodDefinition ]].
	^ CypressSnapshot definitions: definitions
! !
" Class Implementation for CypressMethodStructure"!

" ------------------- Class methods for CypressMethodStructure"!

!CypressMethodStructure class methodsFor: 'instance creation'!
fromMethodDefinition: methodDefinition

	^self new
		fromMethodDefinition: methodDefinition;
		yourself
! !
" ------------------- Instance methods for CypressMethodStructure"!

!CypressMethodStructure methodsFor: 'converting'!
asCypressMethodDefinition
	"Try to coerce Unicode source to simple Strings when possible."

	^CypressMethodDefinition 
        	className: self classStructure className
		classIsMeta: self isMetaclass
		selector: self selector
		category: self category
		source: self source asString
! !
!CypressMethodStructure methodsFor: 'accessing'!
category

	^self properties at: 'category'
! !
!CypressMethodStructure methodsFor: 'accessing'!
category: aString

	self properties at: 'category' put: aString
! !
!CypressMethodStructure methodsFor: 'accessing'!
classStructure
	^classStructure
! !
!CypressMethodStructure methodsFor: 'accessing'!
classStructure: aCypressClassStructure
	classStructure := aCypressClassStructure
! !
!CypressMethodStructure methodsFor: 'initialization'!
fromMethodDefinition: methodDefinition

	self isMetaclass: methodDefinition classIsMeta.
	self selector: methodDefinition selector.
	self category: methodDefinition category.
	self source: methodDefinition source.
! !
!CypressMethodStructure methodsFor: 'accessing'!
isMetaclass

	isMetaclass ifNil: [ isMetaclass := false ].
	^isMetaclass
! !
!CypressMethodStructure methodsFor: 'accessing'!
isMetaclass: aBoolean
	isMetaclass := aBoolean
! !
!CypressMethodStructure methodsFor: 'accessing'!
selector
    ^ String
        streamContents: [ :stream | 
            self name
                do: [ :chara | 
                    stream
                        nextPut:
                            (chara = $.
                                ifTrue: [ $: ]
                                ifFalse: [ chara ]) ] ]
! !
!CypressMethodStructure methodsFor: 'accessing'!
selector: aString
    name := String
        streamContents: [ :stream | 
            aString
                do: [ :chara | 
                    stream
                        nextPut:
                            (chara = $:
                                ifTrue: [ $. ]
                                ifFalse: [ chara ]) ] ]
! !
!CypressMethodStructure methodsFor: 'accessing'!
source

	^source
! !
!CypressMethodStructure methodsFor: 'accessing'!
source: aString

	source := aString
! !
" Class Extensions "!

" Class Extension for Array"!

" ------------------- Instance methods for Array"!

!Array methodsFor: '*Cypress-Structure'!
asCypressPropertyObject

	^self collect: [:each | each asCypressPropertyObject]! !
!Array methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream indent: startIndent

	| indent |
	aStream
		nextPutAll: '[';
		lf.
	indent := startIndent + 1.
	1 to: self size
		do: 
			[:index |
			| item |
			item := self at: index.
			aStream tab: indent.
			item writeCypressJsonOn: aStream indent: indent.
			index < self size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	self size = 0 ifTrue: [aStream tab: indent].
	aStream nextPutAll: ' ]'! !
" Class Extension for Boolean"!

" ------------------- Instance methods for Boolean"!

!Boolean methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream indent: startIndent

	aStream nextPutAll: self printString! !
" Class Extension for Object"!

" ------------------- Instance methods for Object"!

!Object methodsFor: '*Cypress-Structure'!
asCypressPropertyObject

	^self! !
!Object methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: fileStream

	self writeCypressJsonOn: fileStream indent: 0! !
" Class Extension for Dictionary"!

" ------------------- Instance methods for Dictionary"!

!Dictionary methodsFor: '*Cypress-Structure'!
asCypressPropertyObject

	| result |
	result := self class new: self size.
	self associationsDo: [:assoc | result at: assoc key put: assoc value asCypressPropertyObject].
	^result.
! !
!Dictionary methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream indent: startIndent

	| indent cnt |
	indent := startIndent.
	aStream
		nextPutAll: '{';
		lf.
	cnt := 0.
	indent := indent + 1.
	self keys asSortedCollection do: 
			[:key |
			| value |
			value := self at: key.
			cnt := cnt + 1.
			aStream tab: indent.
			key writeCypressJsonOn: aStream indent: indent.
			aStream nextPutAll: ' : '.
			value writeCypressJsonOn: aStream indent: indent.
			cnt < self size
				ifTrue: 
					[aStream
						nextPutAll: ',';
						lf]].
	self size = 0 ifTrue: [aStream tab: indent].
	aStream nextPutAll: ' }'! !
" Class Extension for String"!

" ------------------- Instance methods for String"!

!String methodsFor: '*Cypress-Structure'!
asCypressPropertyObject

	^self unescapePercents withUnixLineEndings! !
!String methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream indent: startIndent

	aStream
		nextPutAll: '"';
		nextPutAll: self withUnixLineEndings encodeAsUTF8 escapePercents;
		nextPutAll: '"'! !
" Class Extension for Number"!

" ------------------- Instance methods for Number"!

!Number methodsFor: '*Cypress-Structure'!
writeCypressJsonOn: aStream indent: startIndent

	aStream nextPutAll: self printString! !
" Class initializers "!




" End of Package: Cypress-Structure"!


