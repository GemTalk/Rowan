SystemOrganization addCategory: 'Cypress-Structure'!

Error subclass: #CypressJsonError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!

!Dictionary methodsFor: '*Cypress-Structure' stamp: ''!
asCypressPropertyObject	| result |	result := self class new: self size.	self associationsDo: [:assoc | result at: assoc key put: assoc value asCypressPropertyObject].	^result.! !

!Dictionary methodsFor: '*Cypress-Structure' stamp: ''!
writeCypressJsonOn: aStream indent: startIndent	| indent cnt |	indent := startIndent.	aStream		nextPutAll: '{';		lf.	cnt := 0.	indent := indent + 1.	self keys asSortedCollection do: 			[:key |			| value |			value := self at: key.			cnt := cnt + 1.			aStream tab: indent.			key writeCypressJsonOn: aStream indent: indent.			aStream nextPutAll: ' : '.			value writeCypressJsonOn: aStream indent: indent.			cnt < self size				ifTrue: 					[aStream						nextPutAll: ',';						lf]].	self size = 0 ifTrue: [aStream tab: indent].	aStream nextPutAll: ' }'! !

!Number methodsFor: '*Cypress-Structure' stamp: ''!
writeCypressJsonOn: aStream indent: startIndent	aStream nextPutAll: self printString! !

!String methodsFor: '*Cypress-Structure' stamp: ''!
asCypressPropertyObject	^self unescapePercents withUnixLineEndings! !

!String methodsFor: '*Cypress-Structure' stamp: ''!
writeCypressJsonOn: aStream indent: startIndent	aStream		nextPutAll: '"';		nextPutAll: self withUnixLineEndings encodeAsUTF8 escapePercents;		nextPutAll: '"'! !

!Array methodsFor: '*Cypress-Structure' stamp: ''!
asCypressPropertyObject	^self collect: [:each | each asCypressPropertyObject]! !

!Array methodsFor: '*Cypress-Structure' stamp: ''!
writeCypressJsonOn: aStream indent: startIndent	| indent |	aStream		nextPutAll: '[';		lf.	indent := startIndent + 1.	1 to: self size		do: 			[:index |			| item |			item := self at: index.			aStream tab: indent.			item writeCypressJsonOn: aStream indent: indent.			index < self size				ifTrue: 					[aStream						nextPutAll: ',';						lf]].	self size = 0 ifTrue: [aStream tab: indent].	aStream nextPutAll: ' ]'! !

Object subclass: #CypressJsonParser
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!

!CypressJsonParser class methodsFor: 'instance creation' stamp: ''!
new	CypressJsonError signal: 'Instantiate the parser with a stream.'! !

!CypressJsonParser class methodsFor: 'instance creation' stamp: ''!
on: aStream	^ self basicNew initializeOn: aStream! !

!CypressJsonParser class methodsFor: 'accessing' stamp: ''!
parse: aString	^ self parseStream: aString readStream! !

!CypressJsonParser class methodsFor: 'accessing' stamp: ''!
parseStream: aStream	^ (self on: aStream) parse! !

!CypressJsonParser methodsFor: 'adding' stamp: ''!
addProperty: anAssociation to: anObject	"Add the property anAssociation described with key and value to anObject. Subclasses might want to refine this implementation."		^ anObject 		add: anAssociation;		yourself! !

!CypressJsonParser methodsFor: 'adding' stamp: ''!
addValue: anObject to: aCollection	"Add anObject to aCollection. Subclasses might want to refine this implementation."	^ aCollection copyWith: anObject! !

!CypressJsonParser methodsFor: 'creating' stamp: ''!
createArray	"Create an empty collection. Subclasses might want to refine this implementation."	^ Array new! !

!CypressJsonParser methodsFor: 'creating' stamp: ''!
createFalse	"Create the false literal. Subclasses might want to refine this implementation."		^ false! !

!CypressJsonParser methodsFor: 'creating' stamp: ''!
createNull	"Create the null literal. Subclasses might want to refine this implementation."	^ nil! !

!CypressJsonParser methodsFor: 'creating' stamp: ''!
createObject	"Create an empty object. Subclasses might want to refine this implementation."		^ Dictionary new! !

!CypressJsonParser methodsFor: 'creating' stamp: ''!
createProperty: aKey with: aValue	"Create an empty attribute value pair. Subclasses might want to refine this implementation."		^ aKey -> aValue! !

!CypressJsonParser methodsFor: 'creating' stamp: ''!
createString: aString	"Create a string literal. Subclasses might want to refine this implementation."	^ aString! !

!CypressJsonParser methodsFor: 'creating' stamp: ''!
createTrue	"Create the true literal. Subclasses might want to refine this implementation."	^ true! !

!CypressJsonParser methodsFor: 'private' stamp: ''!
expect: aString	"Expects aString and consume input, throw an error otherwise."	^(self match: aString)		ifFalse: [CypressJsonError signal: aString , ' expected']! !

!CypressJsonParser methodsFor: 'initialization' stamp: ''!
initializeOn: aStream	stream := aStream! !

!CypressJsonParser methodsFor: 'private' stamp: ''!
match: aString	"Tries to match aString, consume input and answer true if successful."		| position |	position := stream position.	aString do: [ :each |		(stream atEnd or: [ stream next ~= each ]) ifTrue: [ 			stream position: position.			^ false ] ].	self whitespace.	^ true! !

!CypressJsonParser methodsFor: 'parsing' stamp: ''!
parse	| result |	result := self				whitespace;				parseValue.	stream atEnd ifFalse: [CypressJsonError signal: 'end of input expected'].	^result! !

!CypressJsonParser methodsFor: 'parsing' stamp: ''!
parseArray	| result |	self expect: '['.	result := self createArray.	(self match: ']') ifTrue: [^result].	[stream atEnd] whileFalse: 			[result := self addValue: self parseValue to: result.			(self match: ']') ifTrue: [^result].			self expect: ','].	CypressJsonError signal: 'end of array expected'! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: ''!
parseCharacter	| char |	(char := stream next) = $\ 		ifFalse: [ ^ char ].	(char := stream next) = $" 		ifTrue: [ ^ char ].	char = $\		ifTrue: [ ^ char ].	char = $/		ifTrue: [ ^ char ].	char = $b		ifTrue: [ ^ Character backspace ].	char = $f		ifTrue: [ ^ Character newPage ].	char = $n		ifTrue: [ ^ Character lf ].	char = $r		ifTrue: [ ^ Character cr ].	char = $t		ifTrue: [ ^ Character tab ].	char = $u		ifTrue: [ ^ self parseCharacterHex ].	CypressJsonError signal: 'invalid escape character \' , (String with: char)! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: ''!
parseCharacterHex	| value |	value := self parseCharacterHexDigit.	3 timesRepeat: [ value := (value << 4) + self parseCharacterHexDigit ].	^ Character codePoint: value! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: ''!
parseCharacterHexDigit    | digit |    stream atEnd        ifFalse: [             digit := stream next codePoint.            (digit between: 48 and: 57)                ifTrue: [ ^ digit - 48 ].	"$0"	"$9"            (digit between: 65 and: 70)                ifTrue: [ ^ digit - 55 ].	"$A"	"$F"            (digit between: 97 and: 102)                ifTrue: [ ^ digit - 87 ]	"$a"	"$f" ].    CypressJsonError signal: 'hex-digit expected'! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: ''!
parseNumber	| negated number |	negated := stream peek = $-.	negated ifTrue: [ stream next ].	number := self parseNumberInteger.	(stream peek = $.) ifTrue: [		stream next. 		number := number + self parseNumberFraction ].	(stream peek = $e or: [ stream peek = $E ]) ifTrue: [		stream next.		number := number * self parseNumberExponent ].	negated ifTrue: [ number := number negated ].	self whitespace.	^ number! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: ''!
parseNumberExponent    | number negated |    number := 0.    negated := stream peek = $-.    (negated or: [ stream peek = $+ ])        ifTrue: [ stream next ].    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].    negated        ifTrue: [ number := number negated ].    ^ 10 raisedTo: number! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: ''!
parseNumberFraction    | number power |    number := 0.    power := 1.0.    [ stream atEnd not and: [ stream peek isDigit ] ]        whileTrue: [             number := 10 * number + (stream next codePoint - 48).            power := power * 10.0 ].    ^ number / power! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: ''!
parseNumberInteger    | number |    number := 0.    [ stream atEnd not and: [ stream peek isDigit ] ] whileTrue: [ number := 10 * number + (stream next codePoint - 48) ].    ^ number! !

!CypressJsonParser methodsFor: 'parsing' stamp: ''!
parseObject	| result |	self expect: '{'.	result := self createObject.	(self match: '}') ifTrue: [^result].	[stream atEnd] whileFalse: 			[result := self addProperty: self parseProperty to: result.			(self match: '}') ifTrue: [^result].			self expect: ','].	CypressJsonError signal: 'end of object expected'! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: ''!
parseProperty	| name value |	name := self parseString.	self expect: ':'.	value := self parseValue.	^ self createProperty: name with: value.! !

!CypressJsonParser methodsFor: 'parsing-internal' stamp: ''!
parseString	| result |	self expect: '"'.	result := WriteStream on: String new.	[ stream atEnd or: [ stream peek = $" ] ] 		whileFalse: [ result nextPut: self parseCharacter ].	^ self expect: '"'; createString: result contents! !

!CypressJsonParser methodsFor: 'parsing' stamp: ''!
parseValue	| char |	stream atEnd ifFalse: [ 		char := stream peek.		char = ${			ifTrue: [ ^ self parseObject ].		char = $[			ifTrue: [ ^ self parseArray ].		char = $"			ifTrue: [ ^ self parseString ].		(char = $- or: [ char between: $0 and: $9 ])			ifTrue: [ ^ self parseNumber ].		(self match: 'true')			ifTrue: [ ^ self createTrue ].		(self match: 'false')			ifTrue: [ ^ self createFalse ].		(self match: 'null')			ifTrue: [ ^ self createNull ] ].	CypressJsonError signal: 'invalid input'! !

!CypressJsonParser methodsFor: 'private' stamp: ''!
whitespace	"Strip whitespaces from the input stream."	[ stream atEnd not and: [ stream peek isSeparator ] ]		whileTrue: [ stream next ]! !

Object subclass: #GsGeneralDependencySorter
	instanceVariableNames: 'candidates dependsOnConverter dependentConverter individualDependencyMap dependencyGraphs candidateAliasMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!

!GsGeneralDependencySorter class methodsFor: 'instance creation' stamp: ''!
on: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock	"Create an instance of the receiver capable for sorting the dependencies of someCandidates.	 aOneArgBlock is used to evaluate the key of the object depended on for a candidate.	 anotherOneArgBlock is used to evaluate the key of the candidate itself."	^self new		initializeOn: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock;		yourself.! !

!GsGeneralDependencySorter methodsFor: 'sorting - private' stamp: ''!
determineGraphRoots	^dependencyGraphs		selectAssociations: [:each | (candidateAliasMap includesKey: each key) not]! !

!GsGeneralDependencySorter methodsFor: 'initializing - private' stamp: ''!
initializeOn: someCandidates dependsOn: aOneArgBlock dependent: anotherOneArgBlock	candidates := someCandidates.	dependsOnConverter := aOneArgBlock.	dependentConverter := anotherOneArgBlock.	individualDependencyMap := Dictionary new.	dependencyGraphs := Dictionary new.	candidateAliasMap := Dictionary new! !

!GsGeneralDependencySorter methodsFor: 'sorting' stamp: ''!
inOrder	| sorted |	sorted := OrderedCollection new.	self mapCandidatesIntoGraphs.	self determineGraphRoots		do: [:each | self transcribeGraph: each into: sorted].	^sorted.! !

!GsGeneralDependencySorter methodsFor: 'sorting - private' stamp: ''!
mapCandidatesIntoGraphs	| dependsOnKey dependentKey |	candidates do: 			[:each |			| individualDependency |			dependsOnKey := dependsOnConverter value: each.			dependentKey := dependentConverter value: each.			candidateAliasMap at: dependentKey put: each.			individualDependencyMap at: dependsOnKey ifAbsentPut: [Dictionary new].			individualDependencyMap at: dependentKey ifAbsentPut: [Dictionary new].			individualDependency := individualDependencyMap						associationAt: dependsOnKey.			(dependencyGraphs includesKey: dependsOnKey)				ifFalse: [dependencyGraphs add: individualDependency].			individualDependency value				add: (individualDependencyMap associationAt: dependentKey)]! !

!GsGeneralDependencySorter methodsFor: 'sorting - private' stamp: ''!
transcribeGraph: subtree into: sorted	subtree keysAndValuesDo: [:name :subsubtree |		sorted add: (candidateAliasMap at: name).		self transcribeGraph: subsubtree into: sorted.	].! !

!Object methodsFor: '*Cypress-Structure' stamp: ''!
asCypressPropertyObject	^self! !

!Object methodsFor: '*Cypress-Structure' stamp: ''!
writeCypressJsonOn: fileStream	self writeCypressJsonOn: fileStream indent: 0! !

!Boolean methodsFor: '*Cypress-Structure' stamp: ''!
writeCypressJsonOn: aStream indent: startIndent	aStream nextPutAll: self printString! !

CypressObject subclass: #CypressStructure
	instanceVariableNames: 'name properties packageStructure'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!

CypressStructure subclass: #CypressClassStructure
	instanceVariableNames: 'instanceMethods classMethods comment isClassExtension'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!

!CypressClassStructure class methodsFor: 'instance creation' stamp: ''!
fromClassDefinition: classDefinition	^self new		fromClassDefinition: classDefinition;		yourself! !

!CypressClassStructure methodsFor: 'converting' stamp: ''!
asCypressClassDefinition	self isClassExtension ifTrue: [^CypressError signal: 'Extensions cannot have class definitions'].	^CypressClassDefinition		name: self className		superclassName: self superclassName		category: self category		instVarNames: self instanceVariableNames		classInstVarNames: self classInstanceVariableNames		classVarNames: self classVariableNames		poolDictionaryNames: self poolDictionaryNames		comment: self comment		subclassType: self subclassType! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
category	^self properties		at: 'category'		ifAbsent: [self packageStructure packageName]! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
category: aString	^self properties at: 'category' put: aString! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
classInstanceVariableNames	^self properties at: 'classinstvars' ifAbsent: [#()]! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
classInstanceVariableNames: someStrings	^self properties at: 'classinstvars' put: someStrings! !

!CypressClassStructure methodsFor: 'converting' stamp: ''!
classInstanceVariablesString	^self stringForVariables: self classInstanceVariableNames! !

!CypressClassStructure methodsFor: 'querying' stamp: ''!
classMethodNamed: methodName	^self classMethods		at: methodName		ifAbsentPut: [CypressMethodStructure named: methodName]! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
classMethods	classMethods ifNil: [ classMethods := Dictionary new ].	^classMethods! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
className	^self name! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
classVariableNames	^self properties at: 'classvars' ifAbsent: [#()]! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
classVariableNames: someStrings	^self properties at: 'classvars' put: someStrings! !

!CypressClassStructure methodsFor: 'converting' stamp: ''!
classVariablesString	^self stringForVariables: self classVariableNames! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
comment	comment ifNil: [ comment := '' ].	^comment! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
comment: aString	comment := aString! !

!CypressClassStructure methodsFor: 'initialization' stamp: ''!
fromClassDefinition: classDefinition	self		isClassExtension: false;		name: classDefinition name;		category: classDefinition category;		comment: classDefinition comment;		superclassName: classDefinition superclassName;		instanceVariableNames: classDefinition instVarNames;		classInstanceVariableNames: classDefinition classInstVarNames;		classVariableNames: classDefinition classVarNames;		poolDictionaryNames: classDefinition poolDictionaryNames;		subclassType: classDefinition subclassType.! !

!CypressClassStructure methodsFor: 'querying' stamp: ''!
instanceMethodNamed: methodName	^self instanceMethods		at: methodName 		ifAbsentPut: [CypressMethodStructure named: methodName]! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
instanceMethods	instanceMethods ifNil: [ instanceMethods := Dictionary new ].	^instanceMethods! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
instanceVariableNames	^self properties at: 'instvars' ifAbsent: [#()]! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
instanceVariableNames: someStrings	^self properties at: 'instvars' put: someStrings! !

!CypressClassStructure methodsFor: 'converting' stamp: ''!
instanceVariablesString	^self stringForVariables: self instanceVariableNames! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
isClassExtension        isClassExtension ifNil: [ isClassExtension := true ].        ^isClassExtension! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
isClassExtension: aBoolean	isClassExtension := aBoolean! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
name	^self properties at: 'name'! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
name: aString	self properties at: 'name' put: aString! !

!CypressClassStructure methodsFor: 'converting' stamp: ''!
poolDictionariesString	^self stringForVariables: self poolDictionaryNames! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
poolDictionaryNames	^self properties at: 'pools' ifAbsent: [#()]! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
poolDictionaryNames: someStrings	^self properties at: 'pools' put: someStrings! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
subclassType	^self properties at: '_gs_subclassType' ifAbsent: ['']! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
subclassType: aString	aString isEmpty		ifTrue: [self properties removeKey: '_gs_subclassType' ifAbsent: []]		ifFalse: [self properties at: '_gs_subclassType' put: aString]! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
superclassName	^self properties at: 'super'! !

!CypressClassStructure methodsFor: 'accessing' stamp: ''!
superclassName: aString	^self properties at: 'super' put: aString! !

CypressStructure subclass: #CypressMethodStructure
	instanceVariableNames: 'source isMetaclass classStructure'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!

!CypressMethodStructure class methodsFor: 'instance creation' stamp: ''!
fromMethodDefinition: methodDefinition	^self new		fromMethodDefinition: methodDefinition;		yourself! !

!CypressMethodStructure methodsFor: 'converting' stamp: ''!
asCypressMethodDefinition	"Try to coerce Unicode source to simple Strings when possible."	^CypressMethodDefinition         	className: self classStructure className		classIsMeta: self isMetaclass		selector: self selector		category: self category		source: self source asString! !

!CypressMethodStructure methodsFor: 'accessing' stamp: ''!
category	^self properties at: 'category'! !

!CypressMethodStructure methodsFor: 'accessing' stamp: ''!
category: aString	self properties at: 'category' put: aString! !

!CypressMethodStructure methodsFor: 'accessing' stamp: ''!
classStructure	^classStructure! !

!CypressMethodStructure methodsFor: 'accessing' stamp: ''!
classStructure: aCypressClassStructure	classStructure := aCypressClassStructure! !

!CypressMethodStructure methodsFor: 'initialization' stamp: ''!
fromMethodDefinition: methodDefinition	self isMetaclass: methodDefinition classIsMeta.	self selector: methodDefinition selector.	self category: methodDefinition category.	self source: methodDefinition source.! !

!CypressMethodStructure methodsFor: 'accessing' stamp: ''!
isMetaclass	isMetaclass ifNil: [ isMetaclass := false ].	^isMetaclass! !

!CypressMethodStructure methodsFor: 'accessing' stamp: ''!
isMetaclass: aBoolean	isMetaclass := aBoolean! !

!CypressMethodStructure methodsFor: 'accessing' stamp: ''!
selector    ^ String        streamContents: [ :stream |             self name                do: [ :chara |                     stream                        nextPut:                            (chara = $.                                ifTrue: [ $: ]                                ifFalse: [ chara ]) ] ]! !

!CypressMethodStructure methodsFor: 'accessing' stamp: ''!
selector: aString    name := String        streamContents: [ :stream |             aString                do: [ :chara |                     stream                        nextPut:                            (chara = $:                                ifTrue: [ $. ]                                ifFalse: [ chara ]) ] ]! !

!CypressMethodStructure methodsFor: 'accessing' stamp: ''!
source	^source! !

!CypressMethodStructure methodsFor: 'accessing' stamp: ''!
source: aString	source := aString! !

CypressStructure subclass: #CypressPackageStructure
	instanceVariableNames: 'classes extensions'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Structure'!

!CypressPackageStructure class methodsFor: 'instance creation' stamp: ''!
fileOutsForPackagesNamed: someNames	^someNames inject: Dictionary new		into: 			[:result :each |			result				at: each					put: (String streamContents: 								[:stream |								(self fromPackage: (CypressPackageDefinition named: each))									fileOutOn: stream]);				yourself]! !

!CypressPackageStructure class methodsFor: 'instance creation' stamp: ''!
fromPackage: aCypressPackageDefinition	^(self new) 		fromPackage: aCypressPackageDefinition;		yourself! !

!CypressPackageStructure methodsFor: 'accessing' stamp: ''!
classes	classes ifNil: [ classes := OrderedCollection new ].	^classes! !

!CypressPackageStructure methodsFor: 'accessing' stamp: ''!
classesInFileInOrder	^(GsGeneralDependencySorter		on: self classes		dependsOn: [:candidate | candidate superclassName]		dependent: [:candidate | candidate className])			inOrder! !

!CypressPackageStructure methodsFor: 'accessing' stamp: ''!
classesWithInitializers	^self classesInFileInOrder		select: [:each | each classMethods anySatisfy: [:method | method selector = 'initialize']]! !

!CypressPackageStructure methodsFor: 'accessing' stamp: ''!
extensions	extensions ifNil: [ extensions := OrderedCollection new ].	^extensions! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOut: aString implementationsFrom: someClassStructures on: aStream	someClassStructures		do: [:each | self fileOutType: aString implementationOf: each on: aStream]! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOut: aString methods: someMethodStructures on: aStream	someMethodStructures isEmpty ifTrue: [^self].	self		fileOut: aString methodsPreambleFor: someMethodStructures any classStructure on: aStream;		fileOutMethods: someMethodStructures on: aStream! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOut: aString methodsPreambleFor: classStructure on: aStream	aStream		nextPutAll: '!! ------------------- ', aString, ' methods for ', classStructure name; lf;		lf! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutClassDeclaration: classStructure on: aStream	aStream		nextPutAll: 'doit'; lf;		nextPutAll: '(', classStructure superclassName; lf;		nextPutAll: '	subclass: ', classStructure className asString printString; lf;		nextPutAll: '	instVarNames: #( ', classStructure instanceVariablesString, ' )'; lf;		nextPutAll: '	classVars: #( ', classStructure classVariablesString, ' )'; lf;		nextPutAll: '	classInstVars: #( ', classStructure classInstanceVariablesString, ' )'; lf;		nextPutAll: '	poolDictionaries: #()'; lf;		nextPutAll: '	inDictionary: UserGlobals'; lf;		nextPutAll: '	options: #())'; lf;		nextPutAll: '		category: ', classStructure category printString, ';'; lf;		nextPutAll: '		comment: ', classStructure comment printString, ';'; lf;		nextPutAll: '		immediateInvariant.'; lf;		nextPutAll: '%'; lf;		lf.! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutClassDeclarationsOn: aStream	self classesInFileInOrder		do: [:classStructure | self fileOutClassDeclaration: classStructure on: aStream]! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutClassesOn: aStream	self		fileOutClassesPreambleOn: aStream;		fileOutClassDeclarationsOn: aStream;		fileOutClassImplementationsOn: aStream! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutClassesPreambleOn: aStream	aStream		nextPutAll: '!! Class Declarations'; lf;		lf! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutClassImplementationsOn: aStream	self		fileOut: 'Class Implementation'		implementationsFrom: self classesInFileInOrder		on: aStream! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutClassInitializerFor: classStructure on: aStream	aStream		nextPutAll: classStructure className, ' initialize.'; lf! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutClassInitializersOn: aStream	self fileOutClassInitializersPreambleOn: aStream.	self classesWithInitializers do: [:each | self fileOutClassInitializerFor: each on: aStream].	self fileOutClassInitializersPostambleOn: aStream.! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutClassInitializersPostambleOn: aStream	aStream		nextPutAll: '%'; lf;		lf! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutClassInitializersPreambleOn: aStream	aStream		nextPutAll: '!! Class initializers '; lf;		lf;		nextPutAll: 'doit'; lf! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutExtensionImplementationsOn: aStream	self		fileOut: 'Class Extension'		implementationsFrom: self extensions		on: aStream! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutExtensionsOn: aStream	self		fileOutExtensionsPreambleOn: aStream;		fileOutExtensionImplementationsOn: aStream! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutExtensionsPreambleOn: aStream	aStream		nextPutAll: '!! Class Extensions'; lf;		lf! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutMethod: methodStructure on: aStream	aStream		nextPutAll: 'category: ', methodStructure category printString; lf;		nextPutAll: 'set compile_env: 0'; lf;		nextPutAll: (methodStructure isMetaclass ifTrue: ['classmethod: '] ifFalse: ['method: ']), methodStructure classStructure className; lf;		nextPutAll: methodStructure source.	methodStructure source last = Character lf		ifFalse: [aStream lf].	aStream nextPutAll: '%'; lf;		lf! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutMethods: someMethodStructures on: aStream	(someMethodStructures		asSortedCollection: [:a :b | a selector <= b selector])			do: [:methodStructure | self fileOutMethod: methodStructure on: aStream]! !

!CypressPackageStructure methodsFor: 'filing out' stamp: ''!
fileOutOn: aStream	self		fileOutPackagePreambleOn: aStream;		fileOutClassesOn: aStream;		fileOutExtensionsOn: aStream;		fileOutClassInitializersOn: aStream;		fileOutPackagePostambleOn: aStream! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutPackagePostambleOn: aStream	aStream		lf;		lf;		nextPutAll: '!! End of Package: ', self packageName; lf;		lf;		lf! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutPackagePreambleOn: aStream	aStream		nextPutAll: '!! Package: ', self packageName; lf;		lf;		lf;		nextPutAll: '!! Remove existing behavior from package ', self packageName; lf;		nextPutAll: '!!!!!!!! This can be cleaned up when some package functionality is moved to the base system.'; lf;		lf;		nextPutAll: 'doit'; lf;		nextPutAll: '| packageName |'; lf;		nextPutAll: 'packageName := ', self packageName printString, '.'; lf;		nextPutAll: 'System myUserProfile symbolList do: [:symDict |'; lf;		nextPutAll: '	symDict do: [:possibleClass |'; lf;		nextPutAll: '			| toRemove |'; lf;		nextPutAll: '		possibleClass isBehavior ifTrue: ['; lf;		nextPutAll: '			{possibleClass. possibleClass class} do: [:aClass |'; lf;		nextPutAll: '				aClass category = packageName'; lf;		nextPutAll: '					ifTrue: ['; lf;		nextPutAll: '							"*anythingbutpackagename[-anything]"'; lf;		nextPutAll: '						toRemove := aClass categoryNames select: '; lf;		nextPutAll: '										[:each |'; lf;		nextPutAll: '										(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])'; lf;		nextPutAll: '										or: [each first ~= $*]]'; lf;		nextPutAll: '					]'; lf;		nextPutAll: '					ifFalse: ['; lf;		nextPutAll: '							"*packagename[-anything]"'; lf;		nextPutAll: '						toRemove := aClass categoryNames select: '; lf;		nextPutAll: '										[:each |'; lf;		nextPutAll: '										each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]'; lf;		nextPutAll: '					].'; lf;		nextPutAll: '				toRemove do: [:each | aClass removeCategory: each].'; lf;		nextPutAll: '			]'; lf;		nextPutAll: '		]'; lf;		nextPutAll: '	]'; lf;		nextPutAll: '].'; lf;		nextPutAll: '%'; lf;		lf;		lf! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutPreambleType: aString for: classStructure on: aStream	aStream		nextPutAll: '!! ', aString, ' for ', classStructure name; lf;		lf! !

!CypressPackageStructure methodsFor: 'filing out - private' stamp: ''!
fileOutType: aString implementationOf: classStructure on: aStream	self		fileOutPreambleType: aString			for: classStructure			on: aStream;		fileOut: 'Class'			methods: classStructure classMethods			on: aStream;		fileOut: 'Instance'			methods: classStructure instanceMethods			on: aStream! !

!CypressPackageStructure methodsFor: 'initialization' stamp: ''!
fromPackage: aCypressPackageDefinition	| snapshot classMap classDefinitions classStructure |	snapshot := aCypressPackageDefinition snapshot.	name := aCypressPackageDefinition name, self packageExtension.	properties := Dictionary new.	classDefinitions := OrderedCollection new.	classMap := Dictionary new.	snapshot definitions do: [:definition |  			definition 				classDefinition: [:classDefinition |  classDefinitions add: classDefinition ] 				methodDefinition: [:methodDefinition | 					(classMap 						at: methodDefinition className 						ifAbsentPut: [Set new]) 							add: methodDefinition. ]].	classDefinitions do: [:classDefinition |		classStructure := (CypressClassStructure fromClassDefinition: classDefinition)			packageStructure: self.		(classMap removeKey: classDefinition className ifAbsent: [#()]) do: [:methodDefinition | | methodStructure |			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)				packageStructure: self;				classStructure: classStructure.			(methodDefinition				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 				classMethod: [:classMethod | classStructure classMethods ])					at: methodDefinition selector					put: methodStructure ].		self classes add: classStructure ].	classMap keysAndValuesDo: [:className :methods |		classStructure := (CypressClassStructure named: className)			isClassExtension: true;			packageStructure: self.		methods do: [:methodDefinition | | methodStructure |			methodStructure := (CypressMethodStructure fromMethodDefinition: methodDefinition)				packageStructure: self;				classStructure: classStructure.			(methodDefinition				instanceMethod: [:instanceMethod | classStructure instanceMethods ] 				classMethod: [:classMethod | classStructure classMethods ])					at: methodDefinition selector					put: methodStructure ].		self extensions add: classStructure ].! !

!CypressPackageStructure methodsFor: 'accessing' stamp: ''!
packageExtension	^self packageExtensionOr: ['.package' ]! !

!CypressPackageStructure methodsFor: 'accessing' stamp: ''!
packageExtensionOr: aBlock	^self properties at: 'extension' ifAbsent: aBlock! !

!CypressPackageStructure methodsFor: 'accessing' stamp: ''!
packageName	^self name copyWithoutSuffix: self packageExtension! !

!CypressPackageStructure methodsFor: 'accessing' stamp: ''!
packageStructure	^self! !

!CypressPackageStructure methodsFor: 'snapshotting' stamp: ''!
snapshot	| definitions |	definitions := OrderedCollection new.	self classes do: [:classStructure |        	definitions add: classStructure asCypressClassDefinition.                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |			definitions add: methodStructure asCypressMethodDefinition ].                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |			definitions add: methodStructure asCypressMethodDefinition ]].	self extensions do: [:classStructure |                (classStructure instanceMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |			definitions add: methodStructure asCypressMethodDefinition ].                (classStructure classMethods asSortedCollection: [:a :b | a selector <= b selector]) do: [:methodStructure |			definitions add: methodStructure asCypressMethodDefinition ]].	^ CypressSnapshot definitions: definitions! !

!CypressStructure class methodsFor: 'instance creation' stamp: ''!
named: aString	^(self new)		name: aString;		yourself! !

!CypressStructure methodsFor: 'accessing' stamp: ''!
name	^name! !

!CypressStructure methodsFor: 'accessing' stamp: ''!
name: aString 	name := aString! !

!CypressStructure methodsFor: 'accessing' stamp: ''!
packageStructure	^packageStructure! !

!CypressStructure methodsFor: 'accessing' stamp: ''!
packageStructure: aCypressPackageStructure	packageStructure := aCypressPackageStructure! !

!CypressStructure methodsFor: 'printing' stamp: ''!
printDetailsOn: aStream	aStream nextPutAll: self name.! !

!CypressStructure methodsFor: 'accessing' stamp: ''!
properties	properties ifNil: [ properties := Dictionary new ].	^properties! !

!CypressStructure methodsFor: 'accessing' stamp: ''!
properties: aDictionary	properties := aDictionary! !
