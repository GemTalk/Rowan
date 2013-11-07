SystemOrganization addCategory: 'Cypress-Definitions'!

Error subclass: #CypressError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

Error subclass: #CypressLoaderError
	instanceVariableNames: 'patchOperation exception'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressLoaderError commentStamp: '' prior: 0!
CypressLoaderError is used to report a failure applying a specific CypressPatchOperation.The CypressLoader made a first attempt to apply the Patch Operation and reported a CypressLoaderErrorNotification, set aside the Patch Operation, and has retried it after applyingall other Patch Operations.Instance Variables:patchOperation:		the CypressPatchOperation that could not be applied.exception:			the Error which occurred while trying to apply the Patch Operation.!

!CypressLoaderError class methodsFor: 'instance creation' stamp: ''!
patchOperation: aPatchOperation exception: anException	^self new		initializePatchOperation: aPatchOperation exception: anException;		yourself! !

!CypressLoaderError methodsFor: 'accessing' stamp: ''!
exception	"Answer the original exception raised when applying the Patch Operation."	^exception! !

!CypressLoaderError methodsFor: 'updating' stamp: ''!
exception: anException	"Assign the original exception raised when applying the Patch Operation."	exception := anException! !

!CypressLoaderError methodsFor: 'initializing - private' stamp: ''!
initialize	super initialize.	gsResumable := true! !

!CypressLoaderError methodsFor: 'initializing - private' stamp: ''!
initializeMessageText	messageText := String streamContents: 					[:stream |					stream						nextPutAll: self patchOperation printString;						nextPutAll: ' failed because ';						nextPutAll: self exception printString]! !

!CypressLoaderError methodsFor: 'initializing - private' stamp: ''!
initializePatchOperation: aPatchOperation exception: anException	self		patchOperation: aPatchOperation;		exception: anException;		initializeMessageText! !

!CypressLoaderError methodsFor: 'handling' stamp: ''!
logNotification: aString	GsFile gciLogServer: aString.	Transcript cr; nextPutAll: aString.! !

!CypressLoaderError methodsFor: 'accessing' stamp: ''!
patchOperation	"Answer the Patch Operation that could not be applied."	^patchOperation! !

!CypressLoaderError methodsFor: 'updating' stamp: ''!
patchOperation: aCypressPatchOperation	"Assign the Patch Operation that could not be applied."	patchOperation := aCypressPatchOperation! !

Error subclass: #CypressLoaderMissingClasses
	instanceVariableNames: 'requirementsMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressLoaderMissingClasses class methodsFor: 'instance creation' stamp: ''!
missingRequirementsMap: aDictionary	"Answer an instance of the receiver initialized on the specified	 missing requirements. aDictionary maps prerequisite names to	 a collection of dependent definitions."	^self new		initializeRequirementsMap: aDictionary;		yourself! !

!CypressLoaderMissingClasses methodsFor: 'initializing - private' stamp: ''!
initialize	super initialize.	gsResumable := true! !

!CypressLoaderMissingClasses methodsFor: 'initializing - private' stamp: ''!
initializeMessageText	messageText := String streamContents: 					[:stream |					stream nextPutAll: 'Missing classes:'.					self requirementsMap keysAndValuesDo: 							[:className :definitions |							stream								space;								nextPutAll: className printString , '(' , definitions size printString											, ')']]! !

!CypressLoaderMissingClasses methodsFor: 'initializing - private' stamp: ''!
initializeRequirementsMap: aDictionary	self		requirementsMap: aDictionary;		initializeMessageText.! !

!CypressLoaderMissingClasses methodsFor: 'accessing' stamp: ''!
requirementsMap	"The requirements map is a Dictionary mapping missing class	 names to a collection of dependent definitions."   ^requirementsMap! !

!CypressLoaderMissingClasses methodsFor: 'updating' stamp: ''!
requirementsMap: aDictionary	"The requirements map is a Dictionary mapping missing class	 names to a collection of dependent definitions."	requirementsMap := aDictionary! !

!Class methodsFor: '*Cypress-Definitions' stamp: ''!
asCypressClassDefinition	^CypressClassDefinition		name: self name		superclassName: self superclass name		category: self category		instVarNames: self instVarNames		classInstVarNames: self class instVarNames		classVarNames: self classVarNames		poolDictionaryNames: self sharedPools		comment: self comment		subclassType: self subclassType.! !

!Class methodsFor: '*Cypress-Definitions' stamp: ''!
subclassType	"Answer a description of the receiver to identify whether it is a regular class,	 a byte subclass, or an indexable subclass."	^(self isBytes and: [self superClass isBytes not])		ifTrue: ['byteSubclass']		ifFalse: 			[(self isIndexable and: [self superClass isIndexable not])				ifTrue: ['indexableSubclass']				ifFalse: ['']]! !

Notification subclass: #CypressLoaderErrorNotification
	instanceVariableNames: 'patchOperation exception'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressLoaderErrorNotification commentStamp: '' prior: 0!
CypressLoaderErrorNotification is used to notify a consumer of the CypressLoader that a particular CypressPatchOperation failed.As a Notification, it resumes by default, logging the error to the Transcript.Instance Variables:patchOperation:		the CypressPatchOperation that could not be applied.exception:			the Error which occurred while trying to apply the Patch Operation.!

!CypressLoaderErrorNotification class methodsFor: 'instance creation' stamp: ''!
patchOperation: aPatchOperation exception: anException	^self new		initializePatchOperation: aPatchOperation exception: anException;		yourself! !

!CypressLoaderErrorNotification methodsFor: 'handling' stamp: ''!
defaultAction	"Log the notification to the GCI log and the Transcript, then resume."	self logNotification: 'Notice: ' , self asString.	^super defaultAction! !

!CypressLoaderErrorNotification methodsFor: 'accessing' stamp: ''!
exception	"Answer the original exception raised when applying the Patch Operation."	^exception! !

!CypressLoaderErrorNotification methodsFor: 'updating' stamp: ''!
exception: anException	"Assign the original exception raised when applying the Patch Operation."	exception := anException! !

!CypressLoaderErrorNotification methodsFor: 'initializing - private' stamp: ''!
initializeMessageText	messageText := String streamContents: 					[:stream |					stream						nextPutAll: self patchOperation printString;						nextPutAll: ' failed because ';						nextPutAll: self exception printString]! !

!CypressLoaderErrorNotification methodsFor: 'initializing - private' stamp: ''!
initializePatchOperation: aPatchOperation exception: anException	self		patchOperation: aPatchOperation;		exception: anException;		initializeMessageText! !

!CypressLoaderErrorNotification methodsFor: 'handling' stamp: ''!
logNotification: aString	GsFile gciLogServer: aString.	Transcript cr; nextPutAll: aString.! !

!CypressLoaderErrorNotification methodsFor: 'accessing' stamp: ''!
patchOperation	"Answer the Patch Operation that could not be applied."	^patchOperation! !

!CypressLoaderErrorNotification methodsFor: 'updating' stamp: ''!
patchOperation: aCypressPatchOperation	"Assign the Patch Operation that could not be applied."	patchOperation := aCypressPatchOperation! !

!GsNMethod methodsFor: '*Cypress-Definitions' stamp: ''!
asCypressMethodDefinition	"Try to coerce Unicode source to simple Strings when possible."	^CypressMethodDefinition		className: self methodClass theNonMetaClass name		classIsMeta: self methodClass isMeta		selector: self selector		category: self category		source: self sourceString asString! !

Object subclass: #CypressObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

CypressObject subclass: #CypressDefinition
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

CypressDefinition subclass: #CypressClassDefinition
	instanceVariableNames: 'name superclassName category comment instVarNames classInstVarNames classVarNames poolDictionaryNames subclassType'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressClassDefinition class methodsFor: 'instance creation' stamp: ''!
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment subclassType: subclassType	^self new		name: aClassName asString		superclassName: aSuperclassName asString		category: aCategory asString		instVarNames: (someInstanceVariableNames asArray				collect: [:each | each asString])		classInstVarNames: (someClassInstanceVariableNames asArray				collect: [:each | each asString])		classVarNames: (someClassVariableNames asArray				collect: [:each | each asString])		poolDictionaryNames: (somePoolDictionaryNames asArray				collect: [:each | each asString])		comment: (self normalizeLineEndings: aComment)		subclassType: subclassType! !

!CypressClassDefinition methodsFor: 'comparing' stamp: ''!
= aDefinition	^(super = aDefinition)		and: [superclassName = aDefinition superclassName		and: [category = aDefinition category		and: [instVarNames = aDefinition instVarNames		and: [classInstVarNames = aDefinition classInstVarNames		and: [classVarNames = aDefinition classVarNames		and: [poolDictionaryNames = aDefinition poolDictionaryNames		and: [comment = aDefinition comment]]]]]]]! !

!CypressClassDefinition methodsFor: 'loading' stamp: ''!
actualClass	^self resolveGlobalNamed: self name! !

!CypressClassDefinition methodsFor: 'loading' stamp: ''!
actualClassOrNil	^self resolveGlobalNamed: self name or: [nil]! !

!CypressClassDefinition methodsFor: 'converting' stamp: ''!
asCypressClassDefinition	^self! !

!CypressClassDefinition methodsFor: 'accessing' stamp: ''!
category	^category! !

!CypressClassDefinition methodsFor: 'visiting' stamp: ''!
classDefinition: classBlock methodDefinition: methodBlock	classBlock value: self! !

!CypressClassDefinition methodsFor: 'private' stamp: ''!
classInstanceVariablesString    ^ self stringForVariables: self classInstVarNames! !

!CypressClassDefinition methodsFor: 'accessing' stamp: ''!
classInstVarNames	^classInstVarNames! !

!CypressClassDefinition methodsFor: 'accessing' stamp: ''!
className	^self name! !

!CypressClassDefinition methodsFor: 'loading' stamp: ''!
classNeedingMigration: aClass	self halt: 'not implemented yet'! !

!CypressClassDefinition methodsFor: 'private' stamp: ''!
classVariablesString    ^ self stringForVariables: self classVarNames! !

!CypressClassDefinition methodsFor: 'accessing' stamp: ''!
classVarNames	^classVarNames! !

!CypressClassDefinition methodsFor: 'accessing' stamp: ''!
comment	^comment! !

!CypressClassDefinition methodsFor: 'loading' stamp: ''!
createOrReviseByteClass	"To be resolved:		- the question of an 'environment' in which to create the class.		- the question of which SymbolDictionary in which to create the class.	 These are perhaps the same question."	| superClass |	superClass := self resolveGlobalNamed: self superclassName.	^(superClass		byteSubclass: self name		classVars: (self classVarNames collect: [:each | each asSymbol])		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])		poolDictionaries: #()		inDictionary: (self symbolDictionaryForClassNamed: self name)		options: #())			category: category;			comment: self comment! !

!CypressClassDefinition methodsFor: 'loading' stamp: ''!
createOrReviseClass	^self subclassType = ''		ifTrue: [self createOrReviseRegularClass]		ifFalse: 			[self subclassType = 'byteSubclass'				ifTrue: [self createOrReviseByteClass]				ifFalse: 					[self subclassType = 'indexableSubclass'						ifTrue: [self createOrReviseIndexableClass]						ifFalse: 							[self error: 'unknown subclass type: ' , self subclassType printString]]]! !

!CypressClassDefinition methodsFor: 'loading' stamp: ''!
createOrReviseIndexableClass	"To be resolved:		- the question of an 'environment' in which to create the class.		- the question of which SymbolDictionary in which to create the class.	 These are perhaps the same question."	| superClass |	superClass := self resolveGlobalNamed: self superclassName.	^(superClass		indexableSubclass: self name		instVarNames: (self instVarNames collect: [:each | each asSymbol])		classVars: (self classVarNames collect: [:each | each asSymbol])		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])		poolDictionaries: #()		inDictionary: (self symbolDictionaryForClassNamed: self name)		options: #())			category: category;			comment: self comment! !

!CypressClassDefinition methodsFor: 'loading' stamp: ''!
createOrReviseRegularClass	"To be resolved:		- the question of an 'environment' in which to create the class.		- the question of which SymbolDictionary in which to create the class.	 These are perhaps the same question."	| superClass |	superClass := self resolveGlobalNamed: self superclassName.	^(superClass		subclass: self name		instVarNames: (self instVarNames collect: [:each | each asSymbol])		classVars: (self classVarNames collect: [:each | each asSymbol])		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])		poolDictionaries: #()		inDictionary: (self symbolDictionaryForClassNamed: self name)		options: #())			category: category;			comment: self comment! !

!CypressClassDefinition methodsFor: 'accessing' stamp: ''!
description	^ Array with: name! !

!CypressClassDefinition methodsFor: 'loading' stamp: ''!
failedCompiledMethods: someCompiledMethods	someCompiledMethods isEmpty ifTrue: [^self].	self halt: 'not implemented yet'! !

!CypressClassDefinition methodsFor: 'comparing' stamp: ''!
hash	| hash |	hash := name hash.	hash := superclassName hash bitOr: hash.	hash := (category ifNil: ['']) hash bitOr: hash.	instVarNames , classInstVarNames, classVarNames, poolDictionaryNames		do: [:vName | hash := vName hash bitOr: hash].	^hash! !

!CypressClassDefinition methodsFor: 'private' stamp: ''!
instanceVariablesString    ^ self stringForVariables: self instVarNames! !

!CypressClassDefinition methodsFor: 'accessing' stamp: ''!
instVarNames	^instVarNames! !

!CypressClassDefinition methodsFor: 'loading' stamp: ''!
loadClassDefinition	"Create a new version of the defined class. If the class already exists,	 copy the behaviors and state from the old version."	| newClass oldClass |	oldClass := self actualClassOrNil.	newClass := self createOrReviseClass.	(oldClass isNil or: [newClass == oldClass]) ifTrue: [^self].	self classNeedingMigration: newClass.	self		recompileWithSubclassesFrom: oldClass		to: newClass		symbolList: System myUserProfile symbolList.! !

!CypressClassDefinition methodsFor: 'accessing' stamp: ''!
name	^name! !

!CypressClassDefinition methodsFor: 'initialization' stamp: ''!
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment subclassType: aSubclassType	name := aClassName.	superclassName := aSuperclassName.	category := aCategory.	instVarNames := someInstanceVariableNames.	classInstVarNames := someClassInstanceVariableNames.	classVarNames := someClassVariableNames.	poolDictionaryNames := somePoolDictionaryNames.	comment := aComment.	subclassType := aSubclassType! !

!CypressClassDefinition methodsFor: 'private' stamp: ''!
poolDictionariesString	^self stringForVariables: self poolDictionaryNames! !

!CypressClassDefinition methodsFor: 'accessing' stamp: ''!
poolDictionaryNames	^poolDictionaryNames! !

!CypressClassDefinition methodsFor: 'printString' stamp: ''!
printDetailsOn: aStream	aStream nextPutAll: self name! !

!CypressClassDefinition methodsFor: 'dependency' stamp: ''!
provisions	"Answer list of global names defined by this definition"	^{ self name }! !

!CypressClassDefinition methodsFor: 'loading' stamp: ''!
recompileWithSubclassesFrom: oldClass to: newClass symbolList: aSymbolList	| olds news removedClassVariables removedSharedPools organizer subclasses newSubclass |	olds := oldClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].	news := newClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].	removedClassVariables := olds difference: news.	removedSharedPools := oldClass sharedPools difference: newClass sharedPools.	self failedCompiledMethods: (newClass				_copyMethodsAndVariablesFrom: oldClass				except: { $V. removedClassVariables. $P. removedSharedPools }				dictionaries: aSymbolList).	organizer := ClassOrganizer new.	subclasses := organizer subclassesOf: oldClass.	"Do this -after- #subclassesOf:, which has the side effect of replacing the new	  class with the old class in the organizer"	organizer addClass: newClass.	"Iterate over all the first-level subclasses of the old class to create new subclasses"	subclasses do: 			[:oldSubclass |			newSubclass := 					[oldSubclass definition evaluateInContext: nil symbolList: aSymbolList]							on: Error							do: [:ex | ex return: nil].			(newSubclass notNil and: [newSubclass ~~ oldSubclass])				ifTrue: 					[self						classNeedingMigration: newSubclass;						recompileWithSubclassesFrom: oldSubclass							to: newSubclass							symbolList: aSymbolList]]! !

!CypressClassDefinition methodsFor: 'dependency' stamp: ''!
requirements	"Answer list of global names required by this definition"	^{self superclassName}! !

!CypressClassDefinition methodsFor: 'accessing' stamp: ''!
subclassType	^subclassType! !

!CypressClassDefinition methodsFor: 'accessing' stamp: ''!
superclassName	^superclassName! !

!CypressClassDefinition methodsFor: 'loading' stamp: ''!
unloadDefinition	"GemStone could hold multiple definitions of the same class name.	 Ignore aliased references.	 Unload only the first one resolved.	 It is an error if there is not at least one SymbolDictionary holding a	 class with that name."	| dictionarySymbolPair |	dictionarySymbolPair := ((System myUserProfile symbolList				dictionariesAndSymbolsOf: self actualClass)					select: [:each | each last = self name asSymbol]) first.	dictionarySymbolPair first removeKey: dictionarySymbolPair last! !

!CypressDefinition methodsFor: 'comparing' stamp: ''!
= aDefinition	^(aDefinition isKindOf: CypressDefinition)		and: [aDefinition description = self description]! !

!CypressDefinition methodsFor: 'loading' stamp: ''!
actualClass	self subclassResponsibility! !

!CypressDefinition methodsFor: 'visiting' stamp: ''!
classDefinition: classBlock methodDefinition: methodBlock	"default is noop"! !

!CypressDefinition methodsFor: 'accessing' stamp: ''!
description	self subclassResponsibility! !

!CypressDefinition methodsFor: 'comparing' stamp: ''!
hash    ^ self description hash! !

!CypressDefinition methodsFor: 'testing' stamp: ''!
isSameRevisionAs: aDefinition	^ self = aDefinition! !

!CypressDefinition methodsFor: 'loading' stamp: ''!
loadClassDefinition	"default is to do nothing"! !

!CypressDefinition methodsFor: 'loading' stamp: ''!
loadMethodDefinition	"default is to do nothing"! !

!CypressDefinition methodsFor: 'loading' stamp: ''!
postLoad	"noop"! !

!CypressDefinition methodsFor: 'loading' stamp: ''!
postLoadOver: aDefinition	self postLoad! !

!CypressDefinition methodsFor: 'dependency' stamp: ''!
provisions	"Answer list of global names defined by this definition"	^#()! !

!CypressDefinition methodsFor: 'dependency' stamp: ''!
requirements	"Answer list of global names required by this definition"	^#()! !

!CypressDefinition methodsFor: 'loading' stamp: ''!
unloadDefinition	self subclassResponsibility! !

CypressDefinition subclass: #CypressMethodDefinition
	instanceVariableNames: 'classIsMeta source category selector className'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressMethodDefinition class methodsFor: 'instance creation' stamp: ''!
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource	^self new		className: aName asString		classIsMeta: isMetaclass		selector: aSelector asString		category: aCategory asString		source: (self normalizeLineEndings: aSource)! !

!CypressMethodDefinition methodsFor: 'comparing' stamp: ''!
= aDefinition    ^ super = aDefinition        and: [ aDefinition source = self source                and: [ aDefinition category = self category ] ]! !

!CypressMethodDefinition methodsFor: 'loading' stamp: ''!
actualClass	| cls |	cls := self theNonMetaClass.	^self classIsMeta		ifTrue: [ cls class ]		ifFalse: [ cls  ].! !

!CypressMethodDefinition methodsFor: 'converting' stamp: ''!
asCypressMethodDefinition	^self! !

!CypressMethodDefinition methodsFor: 'accessing' stamp: ''!
category	^category! !

!CypressMethodDefinition methodsFor: 'visiting' stamp: ''!
classDefinition: classBlock methodDefinition: methodBlock	methodBlock value: self! !

!CypressMethodDefinition methodsFor: 'accessing' stamp: ''!
classIsMeta	^classIsMeta! !

!CypressMethodDefinition methodsFor: 'accessing' stamp: ''!
className	^className! !

!CypressMethodDefinition methodsFor: 'initialization' stamp: ''!
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource	className := aName.	classIsMeta := isMetaclass.	selector := aSelector.	category := aCategory.	source := self normalizeLineEndings: aSource! !

!CypressMethodDefinition methodsFor: 'accessing' stamp: ''!
description	^ Array			with: className		with: selector		with: classIsMeta! !

!CypressMethodDefinition methodsFor: 'comparing' stamp: ''!
hash	| hash |	hash := classIsMeta asString hash.	hash := source hash bitOr: hash.	hash := category hash bitOr: hash.	hash := className hash bitOr: hash.	^hash! !

!CypressMethodDefinition methodsFor: 'visiting' stamp: ''!
instanceMethod: instanceBlock classMethod: classBlock	^(self classIsMeta		ifTrue: [ classBlock ]		ifFalse: [ instanceBlock ]) value: self! !

!CypressMethodDefinition methodsFor: 'testing' stamp: ''!
isInitializer	^ self selector = 'initialize' and: [self classIsMeta]! !

!CypressMethodDefinition methodsFor: 'loading' stamp: ''!
loadMethodDefinition	self actualClass		compileMethod: self source		dictionaries: System myUserProfile symbolList		category: self category		environmentId: 0! !

!CypressMethodDefinition methodsFor: 'loading' stamp: ''!
postLoadOver: aDefinition	super postLoadOver: aDefinition.	(self isInitializer		and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]) 			ifTrue: [ self theNonMetaClass initialize ].! !

!CypressMethodDefinition methodsFor: 'printing' stamp: ''!
printDetailsOn: aStream	aStream		nextPutAll: self className;		nextPutAll: (self classIsMeta ifTrue: [' class'] ifFalse: ['']);		nextPutAll: '>>';		nextPutAll: self selector.! !

!CypressMethodDefinition methodsFor: 'dependency' stamp: ''!
requirements	"Answer list of global names required by this definition"	^{self className}! !

!CypressMethodDefinition methodsFor: 'accessing' stamp: ''!
selector	^selector! !

!CypressMethodDefinition methodsFor: 'accessing' stamp: ''!
source	^source! !

!CypressMethodDefinition methodsFor: 'loading' stamp: ''!
theNonMetaClass	^self resolveGlobalNamed: self className! !

!CypressMethodDefinition methodsFor: 'loading' stamp: ''!
unloadDefinition	self actualClass removeSelector: self selector asSymbol! !

CypressObject subclass: #CypressDefinitionIndex
	instanceVariableNames: 'definitionMap'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressDefinitionIndex class methodsFor: 'instance creation' stamp: ''!
definitions: aCollection	^ self new addAll: aCollection! !

!CypressDefinitionIndex methodsFor: 'adding' stamp: ''!
add: aDefinition	^ self definitionMap at: aDefinition description put: aDefinition! !

!CypressDefinitionIndex methodsFor: 'adding' stamp: ''!
addAll: aCollection	aCollection do: [:ea | self add: ea]! !

!CypressDefinitionIndex methodsFor: 'querying' stamp: ''!
definitionLike: aDefinition ifPresent: foundBlock ifAbsent: errorBlock	| definition |	definition := self definitionMap at: aDefinition description ifAbsent: [].	^ definition		ifNil: errorBlock		ifNotNil: [foundBlock value: definition]! !

!CypressDefinitionIndex methodsFor: 'accessing' stamp: ''!
definitionMap	definitionMap ifNil: [ definitionMap := Dictionary new ].	^ definitionMap! !

!CypressDefinitionIndex methodsFor: 'accessing' stamp: ''!
definitions	^self definitionMap values! !

!CypressDefinitionIndex methodsFor: 'removing' stamp: ''!
remove: aDefinition	self definitionMap removeKey: aDefinition description ifAbsent: []! !

CypressObject subclass: #CypressDependencySorter
	instanceVariableNames: 'required provided orderedItems'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressDependencySorter methodsFor: 'building' stamp: ''!
add: aPatchOperation	| requirements |	requirements := self unresolvedRequirementsFor: aPatchOperation.	requirements isEmpty		ifTrue: [self addToOrder: aPatchOperation]		ifFalse: [self addRequirements: requirements for: aPatchOperation].	^ aPatchOperation! !

!CypressDependencySorter methodsFor: 'building' stamp: ''!
addAll: aCollection	aCollection do: [:aPatchOperation | self add: aPatchOperation ]! !

!CypressDependencySorter methodsFor: 'private' stamp: ''!
addExternalProvisions: aCollection	(aCollection intersection: self externalRequirements)		do: [:globalName | self addProvision: globalName]! !

!CypressDependencySorter methodsFor: 'private' stamp: ''!
addProvision: aGlobalName	| newlySatisfied |	self provided add: aGlobalName.	newlySatisfied := self required removeKey: aGlobalName ifAbsent: [#()].	self addAll: newlySatisfied.! !

!CypressDependencySorter methodsFor: 'private' stamp: ''!
addRequirement: globalName for: aPatchOperation	(self itemsRequiring: globalName) add: aPatchOperation! !

!CypressDependencySorter methodsFor: 'private' stamp: ''!
addRequirements: aCollection for: aPatchOperation	aCollection do: [:globalName | self addRequirement: globalName for: aPatchOperation]! !

!CypressDependencySorter methodsFor: 'private' stamp: ''!
addToOrder: aPatchOperation	self orderedItems add: aPatchOperation.	aPatchOperation provisions do: [:globalName | self addProvision: globalName ].! !

!CypressDependencySorter methodsFor: 'accessing' stamp: ''!
externalRequirements	| unloaded providedByUnloaded |	unloaded := self itemsWithMissingRequirements.	providedByUnloaded := (unloaded gather: [:e | e provisions]) asSet.	^ self required keys reject: [:globalName | providedByUnloaded includes: globalName ]! !

!CypressDependencySorter methodsFor: 'private' stamp: ''!
itemsRequiring: globalName	^ self required at: globalName ifAbsentPut: [Set new]! !

!CypressDependencySorter methodsFor: 'accessing' stamp: ''!
itemsWithMissingRequirements	| patchOperations |	patchOperations := Set new.	self required values do: [:aSetOfPatchOperations | patchOperations addAll: aSetOfPatchOperations ].	^ patchOperations! !

!CypressDependencySorter methodsFor: 'accessing' stamp: ''!
orderedItems	"ordered list of patch operations"	orderedItems ifNil: [ orderedItems := OrderedCollection new ].	^orderedItems! !

!CypressDependencySorter methodsFor: 'accessing' stamp: ''!
provided	"set of global names provided by definitions already loaded"	provided ifNil: [ provided := Set new ].	^provided! !

!CypressDependencySorter methodsFor: 'accessing' stamp: ''!
required	"dictionary of required global name mapped to list of definitions that require the global"	required ifNil: [ required := Dictionary new ].	^required! !

!CypressDependencySorter methodsFor: 'private' stamp: ''!
unresolvedRequirementsFor: aPatchOperation	"Answer a list of global names that are required by <aPatchOperation>, but not 	 provided by patchOperations that have already been processed"	^ aPatchOperation requirements difference: self provided! !

CypressObject subclass: #CypressLoader
	instanceVariableNames: 'additions removals unloadable provisions errors methodAdditions requirements exceptionClass'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressLoader class methodsFor: 'loading' stamp: ''!
updatePackage: aPackage withSnapshot: aSnapshot	"Answer the loader used to apply the update."	^(self new)		updatePackage: aPackage withSnapshot: aSnapshot;		load! !

!CypressLoader methodsFor: 'updating' stamp: ''!
addFailedPatchOperation: aPatchOperation	self errors add: aPatchOperation! !

!CypressLoader methodsFor: 'accessing' stamp: ''!
additions	additions ifNil: [ additions := OrderedCollection new ].	^additions! !

!CypressLoader methodsFor: 'loading' stamp: ''!
analyze	self 		analyzeAdditions;		analyzeRemovals! !

!CypressLoader methodsFor: 'loading' stamp: ''!
analyzeAdditions	| sorter |	sorter := CypressDependencySorter new 		addAll: self additions;		addExternalProvisions: self provisions;		yourself.	additions := sorter orderedItems.	requirements := sorter externalRequirements.	unloadable := sorter required.! !

!CypressLoader methodsFor: 'loading' stamp: ''!
analyzeRemovals	| sorter |	sorter := CypressDependencySorter new 		addAll: self removals;		yourself.	removals := sorter orderedItems reverse.! !

!CypressLoader methodsFor: 'applying' stamp: ''!
applyAddition: aCypressPatchOperation	self additions add: aCypressPatchOperation! !

!CypressLoader methodsFor: 'applying' stamp: ''!
applyModification: aCypressPatchOperation	self additions add: aCypressPatchOperation! !

!CypressLoader methodsFor: 'applying' stamp: ''!
applyRemoval: aCypressPatchOperation	self removals add: aCypressPatchOperation! !

!CypressLoader methodsFor: 'loading' stamp: ''!
attemptInitialLoad	self		resetErrors;		notifyOnFailedPatchOperations;		loadAdditions: self additions;		unloadRemovals: self removals.! !

!CypressLoader methodsFor: 'loading' stamp: ''!
errorOnFailedPatchOperations	exceptionClass := CypressLoaderError.! !

!CypressLoader methodsFor: 'accessing' stamp: ''!
errors	errors ifNil: [self resetErrors].	^errors! !

!CypressLoader methodsFor: 'loading' stamp: ''!
handleCompileError: aCompileError from: aPatchOperation	| undefinedSymbolErrors otherErrors |	undefinedSymbolErrors := aCompileError errorDetails				select: [:each | each first = 1031].	otherErrors := aCompileError errorDetails				reject: [:each | each first = 1031].	undefinedSymbolErrors do: [:each | self requirements add: each last].	aCompileError pass! !

!CypressLoader methodsFor: 'loading' stamp: ''!
handlePatchOperation: aPatchOperation failure: anException	"Signal the loader exception appropriate to the current phase.	 Note that a handler may suppress the #addFailedPatchOperation: by	 sending #return or #return: to the resignaled exception. Otherwise,	 resumption from a resumable resignalled exception will continue through	 this method."	(exceptionClass patchOperation: aPatchOperation exception: anException) signal.	self addFailedPatchOperation: aPatchOperation.! !

!CypressLoader methodsFor: 'loading' stamp: ''!
load	self		analyze;		reportUnloadableDefinitions;		attemptInitialLoad;		retryFailedLoads;		postLoad.! !

!CypressLoader methodsFor: 'loading' stamp: ''!
loadAdditions: somePatchOperations	"Load class definitions first, then method definitions."	somePatchOperations		do: [:each | self loadClassDefinition: each];		do: [:each | self loadMethodDefinition: each].! !

!CypressLoader methodsFor: 'operations' stamp: ''!
loadClassDefinition: aPatchOperation	[aPatchOperation loadClassDefinition]		on: Error		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].! !

!CypressLoader methodsFor: 'operations' stamp: ''!
loadMethodDefinition: aPatchOperation		[[aPatchOperation loadMethodDefinition]		on: CompileError		do: [:ex | self handleCompileError: ex from: aPatchOperation]]			on: Error			do: [:ex | self handlePatchOperation: aPatchOperation failure: ex]! !

!CypressLoader methodsFor: 'accessing' stamp: ''!
methodAdditions	^#()! !

!CypressLoader methodsFor: 'loading' stamp: ''!
notifyOnFailedPatchOperations	exceptionClass := CypressLoaderErrorNotification.! !

!CypressLoader methodsFor: 'loading' stamp: ''!
postLoad	"This is where the obsoletion is taken into account ..."	self additions do: [:each | self postLoad: each].! !

!CypressLoader methodsFor: 'operations' stamp: ''!
postLoad: aPatchOperation	[aPatchOperation postLoadDefinition]		on: Error		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].! !

!CypressLoader methodsFor: 'accessing' stamp: ''!
provisions	^ provisions ifNil: [provisions := (self allClasses collect: [:cl | cl name asString]) asSet ]! !

!CypressLoader methodsFor: 'accessing' stamp: ''!
removals	removals ifNil: [ removals := OrderedCollection new ].	^removals! !

!CypressLoader methodsFor: 'loading' stamp: ''!
reportUnloadableDefinitions	self unloadable isEmpty ifTrue: [^self].	(CypressLoaderMissingClasses missingRequirementsMap: unloadable) signal.! !

!CypressLoader methodsFor: 'accessing' stamp: ''!
requirements	^requirements! !

!CypressLoader methodsFor: 'loading' stamp: ''!
resetErrors	errors := OrderedCollection new.! !

!CypressLoader methodsFor: 'loading' stamp: ''!
retryFailedLoads	"In case any of the failed loads were resolved by subsequent	 patch operations after the initial attempt or by editting of the	 failed patch operations by exception handling during the notification	 phase (initial attempt)."	| failed |	failed := self errors.	self		resetErrors;		errorOnFailedPatchOperations;		loadAdditions: (self additions intersection: failed);		unloadRemovals: (self removals intersection: failed).! !

!CypressLoader methodsFor: 'accessing' stamp: ''!
unloadable	unloadable ifNil: [ unloadable := OrderedCollection new ].	^unloadable! !

!CypressLoader methodsFor: 'operations' stamp: ''!
unloadDefinition: aPatchOperation	[aPatchOperation unloadDefinition]		on: Error		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].! !

!CypressLoader methodsFor: 'loading' stamp: ''!
unloadRemovals: somePatchOperations	"Removals need to be done after adding classes and methods."	somePatchOperations		do: [:each | self unloadDefinition: each].! !

!CypressLoader methodsFor: 'loading' stamp: ''!
updatePackage: aPackage withSnapshot: aSnapshot	| patch snapshot |	snapshot := aPackage snapshot.	patch := aSnapshot patchRelativeToBase: snapshot.	patch applyTo: self.	snapshot definitions do: [:ea | self provisions addAll: ea provisions]! !

!CypressObject class methodsFor: 'converting' stamp: ''!
normalizeLineEndings: aString	"Answer a copy of aString with the line endings normalized to	 correspond to the current platform, regardless of how they were	 saved. For example, Squeak uses CR and would normalize with	 #withSqueakLineEndings, for example.	 GemStone Smalltalk uses the Unix line ending of LF."	^aString withUnixLineEndings.! !

!CypressObject methodsFor: 'sorting' stamp: ''!
addClasses: subs to: order fromRelevantClasses: classSet organizedBy: org	1 to: subs size		do: 			[:i |			| assoc class |			class := subs at: i.			(classSet includesIdentical: class) ifTrue: [order add: class].			assoc := org associationAt: class otherwise: nil.			assoc ~~ nil				ifTrue: 					[self						addClasses: assoc value						to: order						fromRelevantClasses: classSet						organizedBy: org]]! !

!CypressObject methodsFor: 'accessing' stamp: ''!
allClasses	^System myUserProfile symbolList allSatisfying: [:each | each isBehavior]! !

!CypressObject methodsFor: 'accessing' stamp: ''!
classesInPackageNamed: aString	| packageName candidateName |	packageName := aString asLowercase.	^(System myUserProfile symbolList allSatisfying: 			[:each |			each isBehavior and: 					[candidateName := each category asLowercase.					candidateName = packageName						or: [candidateName beginsWith: packageName , '-']]])		sortAscending: #('name')! !

!CypressObject methodsFor: 'sorting' stamp: ''!
determineClassHierarchicalOrder: someClasses	"Returns an ordered collection of the specified classes such that	 hierarchical dependencies come first."	| org order classSet block |	org := Dictionary new.	org at: #nil put: ClassSet new.	classSet := ClassSet new.	someClasses do: 			[:each |			| sub |			sub := each.			sub isBehavior				ifTrue: 					[| superCls |					classSet add: sub.										[superCls := sub superClass.					superCls ~~ nil] whileTrue: 								[| assoc |								assoc := org associationAt: superCls otherwise: nil.								assoc									ifNil: 										[assoc := Association newWithKey: superCls value: ClassSet new.										org add: assoc].								assoc value add: sub.								sub := superCls].					(org at: #nil) add: sub]].	"Order the subclass sets and weed out unwanted classes."	order := Array new.	self		addClasses: (org at: #nil)		to: order		fromRelevantClasses: classSet		organizedBy: org.	^order! !

!CypressObject methodsFor: 'initializing' stamp: ''!
initialize	"Placeholder: #initialize is not defined by Object in GemStone Smalltalk."! !

!CypressObject methodsFor: 'converting' stamp: ''!
normalizeLineEndings: aString	"Answer a copy of aString with the line endings normalized to	 correspond to the current platform, regardless of how they were	 saved. For example, Squeak uses CR and would normalize with	 #withSqueakLineEndings, for example."	^self class normalizeLineEndings: aString.! !

!CypressObject methodsFor: 'printing' stamp: ''!
printDetailsOn: aStream! !

!CypressObject methodsFor: 'printing' stamp: ''!
printOn: aStream	| className |	className := self class name.	aStream		nextPutAll: (className first isVowel ifTrue:[ 'an ' ] ifFalse:[ 'a ' ]);		nextPutAll: className;		nextPutAll: '('.	self printDetailsOn: aStream.	aStream nextPutAll: ')'.! !

!CypressObject methodsFor: 'accessing' stamp: ''!
resolveGlobalNamed: aString	^self resolveGlobalNamed: aString		or: [CypressError signal: 'Could not resolve global named ' , aString printString]! !

!CypressObject methodsFor: 'accessing' stamp: ''!
resolveGlobalNamed: aString or: aBlock	^((System myUserProfile resolveSymbol: aString) ifNil: [^aBlock value])		value! !

!CypressObject methodsFor: 'converting' stamp: ''!
stringForVariables: variableList	^String streamContents: 			[:stream |			variableList do: [:each | stream nextPutAll: each]				separatedBy: [stream space]]! !

!CypressObject methodsFor: 'accessing' stamp: ''!
symbolDictionaryForClassNamed: aString	"Answer the SymbolDictionary containing the named class.	 If there are multiple answers, answer the first.	 If there are no answers (i.e., the class does not exist), put it in UserGlobals."	^self symbolDictionaryForClassNamed: aString or: [UserGlobals]! !

!CypressObject methodsFor: 'accessing' stamp: ''!
symbolDictionaryForClassNamed: aString or: aBlock	"Answer the SymbolDictionary containing the named class.	 If there are multiple answers, answer the first.	 If there are no answers (i.e., the class does not exist), answer	 the result of evaluating aBlock."	^System myUserProfile symbolList asArray		detect: [:each | each anySatisfy: [:every | every isBehavior and: [every name asString = aString asString]]]		ifNone: aBlock! !

CypressObject subclass: #CypressPackageDefinition
	instanceVariableNames: 'name'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressPackageDefinition class methodsFor: 'instance creation' stamp: ''!
named: aString	^self new		name: aString;		yourself.! !

!CypressPackageDefinition methodsFor: 'comparing' stamp: ''!
= other	^ other species = self species and: [other name sameAs: name]! !

!CypressPackageDefinition methodsFor: 'snapshotting' stamp: ''!
addClass: aClass toDefinitions: definitions	definitions add: aClass asCypressClassDefinition! !

!CypressPackageDefinition methodsFor: 'snapshotting' stamp: ''!
addExtensionMethodsFromClass: aClass toMap: classMap	| defs map |	defs := classMap at: aClass theNonMetaClass ifAbsent: [OrderedCollection new.].	map := Dictionary new.	aClass categorysDo: 			[:category :selectors |			(category asLowercase beginsWith: '*' , self name asLowercase)				ifTrue: [map at: category put: selectors asSortedCollection]].	map keys asSortedCollection do: 			[:category |			(map at: category)				do: [:selector | defs add: (aClass compiledMethodAt: selector) asCypressMethodDefinition]].	defs notEmpty ifTrue: [classMap at: aClass theNonMetaClass put: defs]! !

!CypressPackageDefinition methodsFor: 'snapshotting' stamp: ''!
addMethodsFromClass: aClass toDefinitions: definitions	"Add only those methods which are not extensions from other packages."	((aClass methodDictionary reject: [:each | each category first = $*])		asSortedCollection: [:a :b | a selector <= b selector])			do: [:method | definitions add: method asCypressMethodDefinition]! !

!CypressPackageDefinition methodsFor: 'accessing' stamp: ''!
classes	^self classesInPackageNamed: self name! !

!CypressPackageDefinition methodsFor: 'accessing' stamp: ''!
name	^ name! !

!CypressPackageDefinition methodsFor: 'accessing' stamp: ''!
name: aString	name := aString! !

!CypressPackageDefinition methodsFor: 'printing' stamp: ''!
printDetailsOn: aStream	aStream nextPutAll: name! !

!CypressPackageDefinition methodsFor: 'snapshotting' stamp: ''!
snapshot	| classDefinitions methodDefinitions classMap |	classDefinitions := OrderedCollection new.	methodDefinitions := OrderedCollection new.	(self determineClassHierarchicalOrder: self classes) do: 			[:cls |			self				addClass: cls toDefinitions: classDefinitions;				addMethodsFromClass: cls toDefinitions: methodDefinitions;				addMethodsFromClass: cls class toDefinitions: methodDefinitions].	classMap := Dictionary new.	self allClasses do: 			[:each |			self				addExtensionMethodsFromClass: each toMap: classMap;				addExtensionMethodsFromClass: each class toMap: classMap].	(self determineClassHierarchicalOrder: classMap keys)		do: [:aClass | methodDefinitions addAll: (classMap at: aClass)].	^CypressSnapshot definitions: classDefinitions, methodDefinitions! !

CypressObject subclass: #CypressPatch
	instanceVariableNames: 'operations'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressPatch class methodsFor: 'instance creation' stamp: ''!
fromBase: baseSnapshot toTarget: targetSnapshot	^ (self new)		fromBase: baseSnapshot		toTarget: targetSnapshot! !

!CypressPatch methodsFor: 'applying' stamp: ''!
applyTo: aCypressLoader	operations do: [:ea | ea applyTo: aCypressLoader].! !

!CypressPatch methodsFor: 'initialization' stamp: ''!
fromBase: baseSnapshot toTarget: targetSnapshot	| base target |		operations := OrderedCollection new.	base := CypressDefinitionIndex definitions: baseSnapshot definitions.	target := CypressDefinitionIndex definitions: targetSnapshot definitions.		target definitions do:		[:t |		base			definitionLike: t			ifPresent: [:b | (b isSameRevisionAs: t) ifFalse: [operations add: (CypressModification of: b to: t)]]			ifAbsent: [operations add: (CypressAddition of: t)]].			base definitions do:		[:b |		target			definitionLike: b			ifPresent: [:t | ]			ifAbsent: [operations add: (CypressRemoval of: b)]]! !

!CypressPatch methodsFor: 'accessing' stamp: ''!
operations	^operations! !

CypressObject subclass: #CypressPatchOperation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

CypressPatchOperation subclass: #CypressAddition
	instanceVariableNames: 'definition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressAddition class methodsFor: 'instance creation' stamp: ''!
of: aDefinition	^ self new definition: aDefinition! !

!CypressAddition methodsFor: 'comparing' stamp: ''!
= aPatchOperation	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]! !

!CypressAddition methodsFor: 'applying' stamp: ''!
applyTo: aCypressLoader	aCypressLoader applyAddition: self! !

!CypressAddition methodsFor: 'accessing' stamp: ''!
definition	^definition! !

!CypressAddition methodsFor: 'initialization' stamp: ''!
definition: aDefinition	definition := aDefinition! !

!CypressAddition methodsFor: 'accessing' stamp: ''!
description    ^ 'add: ' , self definition printString! !

!CypressAddition methodsFor: 'loading' stamp: ''!
loadClassDefinition	self definition loadClassDefinition.! !

!CypressAddition methodsFor: 'loading' stamp: ''!
loadMethodDefinition	self definition loadMethodDefinition.! !

!CypressAddition methodsFor: 'loading' stamp: ''!
postLoadDefinition	self definition postLoadOver: nil! !

!CypressAddition methodsFor: 'dependency' stamp: ''!
provisions	"Answer list of global names defined by this definition"	^self definition provisions! !

!CypressAddition methodsFor: 'dependency' stamp: ''!
requirements	"Answer list of global names required by this definition"	^self definition requirements! !

CypressPatchOperation subclass: #CypressModification
	instanceVariableNames: 'obsoletion modification'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressModification class methodsFor: 'instance creation' stamp: ''!
of: base to: target	^ self new base: base target: target! !

!CypressModification methodsFor: 'initialization' stamp: ''!
= aPatchOperation	^(super = aPatchOperation) and: [self obsoletion = aPatchOperation obsoletion and: [ self modification = aPatchOperation modification]]! !

!CypressModification methodsFor: 'applying' stamp: ''!
applyTo: aCypressLoader	aCypressLoader applyModification: self! !

!CypressModification methodsFor: 'initialization' stamp: ''!
base: base target: target	obsoletion := base.	modification := target.! !

!CypressModification methodsFor: 'accessing' stamp: ''!
description    ^ 'modify from: ' , self obsoletion printString , ' to: ' , self modification printString! !

!CypressModification methodsFor: 'loading' stamp: ''!
loadClassDefinition	self modification loadClassDefinition.! !

!CypressModification methodsFor: 'loading' stamp: ''!
loadMethodDefinition	self modification loadMethodDefinition.! !

!CypressModification methodsFor: 'accessing' stamp: ''!
modification	^modification! !

!CypressModification methodsFor: 'accessing' stamp: ''!
obsoletion	^obsoletion! !

!CypressModification methodsFor: 'loading' stamp: ''!
postLoadDefinition	self modification postLoadOver: self obsoletion! !

!CypressModification methodsFor: 'dependency' stamp: ''!
provisions	"Answer list of global names defined by this definition"	^self modification provisions! !

!CypressModification methodsFor: 'dependency' stamp: ''!
requirements	"Answer list of global names required by this definition"	^self modification requirements! !

!CypressPatchOperation methodsFor: 'comparing' stamp: ''!
= aPatchOperation	^aPatchOperation isKindOf: self class! !

!CypressPatchOperation methodsFor: 'applying' stamp: ''!
applyTo: aCypressLoader	self subclassResponsibility! !

!CypressPatchOperation methodsFor: 'accessing' stamp: ''!
description	self subclassResponsibility! !

!CypressPatchOperation methodsFor: 'comparing' stamp: ''!
hash    ^ self description hash! !

!CypressPatchOperation methodsFor: 'loading' stamp: ''!
loadClassDefinition	self subclassResponsibility! !

!CypressPatchOperation methodsFor: 'loading' stamp: ''!
loadMethodDefinition	self subclassResponsibility! !

!CypressPatchOperation methodsFor: 'loading' stamp: ''!
postLoadDefinition	self subclassResponsibility! !

!CypressPatchOperation methodsFor: 'printing' stamp: ''!
printDetailsOn: aStream	aStream nextPutAll: self description.! !

!CypressPatchOperation methodsFor: 'dependency' stamp: ''!
provisions	"Answer list of global names defined by this definition"	self subclassResponsibility! !

!CypressPatchOperation methodsFor: 'dependency' stamp: ''!
requirements	"Answer list of global names required by this definition"	self subclassResponsibility! !

!CypressPatchOperation methodsFor: 'loading' stamp: ''!
unloadDefinition	CypressError signal: 'inappropriate to send #unloadDefinition to an addition or modification operation'! !

CypressPatchOperation subclass: #CypressRemoval
	instanceVariableNames: 'definition'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressRemoval class methodsFor: 'instance creation' stamp: ''!
of: aDefinition	^ self new definition: aDefinition! !

!CypressRemoval methodsFor: 'comparing' stamp: ''!
= aPatchOperation	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]! !

!CypressRemoval methodsFor: 'applying' stamp: ''!
applyTo: aCypressLoader	aCypressLoader applyRemoval: self! !

!CypressRemoval methodsFor: 'accessing' stamp: ''!
definition	^definition! !

!CypressRemoval methodsFor: 'initialization' stamp: ''!
definition: aDefinition	definition := aDefinition! !

!CypressRemoval methodsFor: 'accessing' stamp: ''!
description	^'remove: ', self definition printString! !

!CypressRemoval methodsFor: 'loading' stamp: ''!
loadClassDefinition		CypressError signal: 'inappropriate to send #loadClassDefinition to a removal operation'! !

!CypressRemoval methodsFor: 'loading' stamp: ''!
loadMethodDefinition		CypressError signal: 'inappropriate to send #loadMethodDefinition to a removal operation'! !

!CypressRemoval methodsFor: 'loading' stamp: ''!
postLoadDefinition		CypressError signal: 'inappropriate to send #postLoadDefinition to a removal operation'! !

!CypressRemoval methodsFor: 'dependency' stamp: ''!
provisions	"Answer list of global names defined by this definition"	^#()! !

!CypressRemoval methodsFor: 'dependency' stamp: ''!
requirements	"Answer list of global names required by this definition"	^#()! !

!CypressRemoval methodsFor: 'loading' stamp: ''!
unloadDefinition	self definition unloadDefinition.! !

CypressObject subclass: #CypressSnapshot
	instanceVariableNames: 'definitions'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cypress-Definitions'!

!CypressSnapshot class methodsFor: 'instance creation' stamp: ''!
definitions: aDefinitions	^(self new) definitions: aDefinitions! !

!CypressSnapshot methodsFor: 'comparing' stamp: ''!
= other	^ definitions asArray = other definitions asArray! !

!CypressSnapshot methodsFor: 'enumerating' stamp: ''!
classDefinitions: classBlock methodDefinitions: methodBlock	self definitions do: [:definition |		definition classDefinition: classBlock methodDefinition: methodBlock]! !

!CypressSnapshot methodsFor: 'accessing' stamp: ''!
definitions	^definitions! !

!CypressSnapshot methodsFor: 'accessing' stamp: ''!
definitions: aDefinitions	definitions := aDefinitions! !

!CypressSnapshot methodsFor: 'patching' stamp: ''!
patchRelativeToBase: aSnapshot	^ CypressPatch fromBase: aSnapshot toTarget: self! !

!CypressSnapshot methodsFor: 'loading' stamp: ''!
updatePackage: aPackage	"Answer the loader used to apply the update."	^CypressLoader updatePackage: aPackage withSnapshot: self! !
