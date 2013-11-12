" Package: Cypress-Definitions"!


" Class Declarations "!

(Error
	subclass: 'CypressLoaderError'
	instVarNames: #( patchOperation exception )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: 'CypressLoaderError is used to report a failure applying a specific CypressPatchOperation.
The CypressLoader made a first attempt to apply the Patch Operation and reported a 
CypressLoaderErrorNotification, set aside the Patch Operation, and has retried it after applying
all other Patch Operations.


Instance Variables:

patchOperation:		the CypressPatchOperation that could not be applied.
exception:			the Error which occurred while trying to apply the Patch Operation.
'!

(Error
	subclass: 'CypressLoaderMissingClasses'
	instVarNames: #( requirementsMap )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(Error
	subclass: 'CypressError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(Notification
	subclass: 'CypressLoaderErrorNotification'
	instVarNames: #( patchOperation exception )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: 'CypressLoaderErrorNotification is used to notify a consumer of the CypressLoader that a particular CypressPatchOperation failed.
As a Notification, it resumes by default, logging the error to the Transcript.


Instance Variables:

patchOperation:		the CypressPatchOperation that could not be applied.
exception:			the Error which occurred while trying to apply the Patch Operation.
'!

(Object
	subclass: 'CypressObject'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressObject
	subclass: 'CypressDefinitionIndex'
	instVarNames: #( definitionMap )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressObject
	subclass: 'CypressPatchOperation'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressPatchOperation
	subclass: 'CypressModification'
	instVarNames: #( obsoletion modification )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressPatchOperation
	subclass: 'CypressAddition'
	instVarNames: #( definition )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressPatchOperation
	subclass: 'CypressRemoval'
	instVarNames: #( definition )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressObject
	subclass: 'CypressSnapshot'
	instVarNames: #( definitions )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressObject
	subclass: 'CypressPackageDefinition'
	instVarNames: #( name )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressObject
	subclass: 'CypressDependencySorter'
	instVarNames: #( required provided orderedItems )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressObject
	subclass: 'CypressDefinition'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressDefinition
	subclass: 'CypressClassDefinition'
	instVarNames: #( name superclassName category comment instVarNames classInstVarNames classVarNames poolDictionaryNames subclassType )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressDefinition
	subclass: 'CypressMethodDefinition'
	instVarNames: #( classIsMeta source category selector className )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressObject
	subclass: 'CypressPatch'
	instVarNames: #( operations )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

(CypressObject
	subclass: 'CypressLoader'
	instVarNames: #( additions removals unloadable provisions errors methodAdditions requirements exceptionClass )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Cypress-Definitions';
		comment: ''!

" Class Implementation for CypressLoaderError"!

" ------------------- Class methods for CypressLoaderError"!

!CypressLoaderError class methodsFor: 'instance creation'!
patchOperation: aPatchOperation exception: anException

	^self new
		initializePatchOperation: aPatchOperation exception: anException;
		yourself
! !
" ------------------- Instance methods for CypressLoaderError"!

!CypressLoaderError methodsFor: 'accessing'!
exception
	"Answer the original exception raised when applying the Patch Operation."

	^exception
! !
!CypressLoaderError methodsFor: 'updating'!
exception: anException
	"Assign the original exception raised when applying the Patch Operation."

	exception := anException
! !
!CypressLoaderError methodsFor: 'initializing - private'!
initialize

	super initialize.
	gsResumable := true! !
!CypressLoaderError methodsFor: 'initializing - private'!
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream
						nextPutAll: self patchOperation printString;
						nextPutAll: ' failed because ';
						nextPutAll: self exception printString]! !
!CypressLoaderError methodsFor: 'initializing - private'!
initializePatchOperation: aPatchOperation exception: anException

	self
		patchOperation: aPatchOperation;
		exception: anException;
		initializeMessageText
! !
!CypressLoaderError methodsFor: 'handling'!
logNotification: aString

	GsFile gciLogServer: aString.
	Transcript cr; nextPutAll: aString.
! !
!CypressLoaderError methodsFor: 'accessing'!
patchOperation
	"Answer the Patch Operation that could not be applied."

	^patchOperation
! !
!CypressLoaderError methodsFor: 'updating'!
patchOperation: aCypressPatchOperation
	"Assign the Patch Operation that could not be applied."

	patchOperation := aCypressPatchOperation
! !
" Class Implementation for CypressLoaderMissingClasses"!

" ------------------- Class methods for CypressLoaderMissingClasses"!

!CypressLoaderMissingClasses class methodsFor: 'instance creation'!
missingRequirementsMap: aDictionary
	"Answer an instance of the receiver initialized on the specified
	 missing requirements. aDictionary maps prerequisite names to
	 a collection of dependent definitions."

	^self new
		initializeRequirementsMap: aDictionary;
		yourself
! !
" ------------------- Instance methods for CypressLoaderMissingClasses"!

!CypressLoaderMissingClasses methodsFor: 'initializing - private'!
initialize

	super initialize.
	gsResumable := true! !
!CypressLoaderMissingClasses methodsFor: 'initializing - private'!
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream nextPutAll: 'Missing classes:'.
					self requirementsMap keysAndValuesDo: 
							[:className :definitions |
							stream
								space;
								nextPutAll: className printString , '(' , definitions size printString
											, ')']]! !
!CypressLoaderMissingClasses methodsFor: 'initializing - private'!
initializeRequirementsMap: aDictionary

	self
		requirementsMap: aDictionary;
		initializeMessageText.
! !
!CypressLoaderMissingClasses methodsFor: 'accessing'!
requirementsMap
	"The requirements map is a Dictionary mapping missing class
	 names to a collection of dependent definitions."

   ^requirementsMap
! !
!CypressLoaderMissingClasses methodsFor: 'updating'!
requirementsMap: aDictionary
	"The requirements map is a Dictionary mapping missing class
	 names to a collection of dependent definitions."

	requirementsMap := aDictionary
! !
" Class Implementation for CypressError"!

" Class Implementation for CypressLoaderErrorNotification"!

" ------------------- Class methods for CypressLoaderErrorNotification"!

!CypressLoaderErrorNotification class methodsFor: 'instance creation'!
patchOperation: aPatchOperation exception: anException

	^self new
		initializePatchOperation: aPatchOperation exception: anException;
		yourself! !
" ------------------- Instance methods for CypressLoaderErrorNotification"!

!CypressLoaderErrorNotification methodsFor: 'handling'!
defaultAction
	"Log the notification to the GCI log and the Transcript, then resume."

	self logNotification: 'Notice: ' , self asString.
	^super defaultAction! !
!CypressLoaderErrorNotification methodsFor: 'accessing'!
exception
	"Answer the original exception raised when applying the Patch Operation."

	^exception! !
!CypressLoaderErrorNotification methodsFor: 'updating'!
exception: anException
	"Assign the original exception raised when applying the Patch Operation."

	exception := anException! !
!CypressLoaderErrorNotification methodsFor: 'initializing - private'!
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream
						nextPutAll: self patchOperation printString;
						nextPutAll: ' failed because ';
						nextPutAll: self exception printString]! !
!CypressLoaderErrorNotification methodsFor: 'initializing - private'!
initializePatchOperation: aPatchOperation exception: anException

	self
		patchOperation: aPatchOperation;
		exception: anException;
		initializeMessageText
! !
!CypressLoaderErrorNotification methodsFor: 'handling'!
logNotification: aString

	GsFile gciLogServer: aString.
	Transcript cr; nextPutAll: aString.! !
!CypressLoaderErrorNotification methodsFor: 'accessing'!
patchOperation
	"Answer the Patch Operation that could not be applied."

	^patchOperation! !
!CypressLoaderErrorNotification methodsFor: 'updating'!
patchOperation: aCypressPatchOperation
	"Assign the Patch Operation that could not be applied."

	patchOperation := aCypressPatchOperation! !
" Class Implementation for CypressObject"!

" ------------------- Class methods for CypressObject"!

!CypressObject class methodsFor: 'converting'!
normalizeLineEndings: aString
	"Answer a copy of aString with the line endings normalized to
	 correspond to the current platform, regardless of how they were
	 saved. For example, Squeak uses CR and would normalize with
	 #withSqueakLineEndings, for example.

	 GemStone Smalltalk uses the Unix line ending of LF."

	^aString withUnixLineEndings.
! !
" ------------------- Instance methods for CypressObject"!

!CypressObject methodsFor: 'sorting'!
addClasses: subs to: order fromRelevantClasses: classSet organizedBy: org

	1 to: subs size
		do: 
			[:i |
			| assoc class |
			class := subs at: i.
			(classSet includesIdentical: class) ifTrue: [order add: class].
			assoc := org associationAt: class otherwise: nil.
			assoc ~~ nil
				ifTrue: 
					[self
						addClasses: assoc value
						to: order
						fromRelevantClasses: classSet
						organizedBy: org]]! !
!CypressObject methodsFor: 'accessing'!
allClasses

	^System myUserProfile symbolList allSatisfying: [:each | each isBehavior]! !
!CypressObject methodsFor: 'accessing'!
classesInPackageNamed: aString

	| packageName candidateName |
	packageName := aString asLowercase.
	^(System myUserProfile symbolList allSatisfying: 
			[:each |
			each isBehavior and: 
					[candidateName := each category asLowercase.
					candidateName = packageName
						or: [candidateName beginsWith: packageName , '-']]])
		sortAscending: #('name')! !
!CypressObject methodsFor: 'sorting'!
determineClassHierarchicalOrder: someClasses
	"Returns an ordered collection of the specified classes such that
	 hierarchical dependencies come first."

	| org order classSet |
	org := Dictionary new.
	org at: #nil put: ClassSet new.
	classSet := ClassSet new.
	someClasses do: 
			[:each |
			| sub |
			sub := each.
			sub isBehavior
				ifTrue: 
					[| superCls |
					classSet add: sub.
					
					[superCls := sub superClass.
					superCls ~~ nil] whileTrue: 
								[| assoc |
								assoc := org associationAt: superCls otherwise: nil.
								assoc
									ifNil: 
										[assoc := Association newWithKey: superCls value: ClassSet new.
										org add: assoc].
								assoc value add: sub.
								sub := superCls].
					(org at: #nil) add: sub]].

	"Order the subclass sets and weed out unwanted classes."
	order := Array new.
	self
		addClasses: (org at: #nil)
		to: order
		fromRelevantClasses: classSet
		organizedBy: org.
	^order! !
!CypressObject methodsFor: 'initializing'!
initialize
	"Placeholder: #initialize is not defined by Object in GemStone Smalltalk."
! !
!CypressObject methodsFor: 'converting'!
normalizeLineEndings: aString
	"Answer a copy of aString with the line endings normalized to
	 correspond to the current platform, regardless of how they were
	 saved. For example, Squeak uses CR and would normalize with
	 #withSqueakLineEndings, for example."

	^self class normalizeLineEndings: aString.
! !
!CypressObject methodsFor: 'printing'!
printDetailsOn: aStream
! !
!CypressObject methodsFor: 'printing'!
printOn: aStream

	| className |
	className := self class name.
	aStream
		nextPutAll: (className first isVowel ifTrue:[ 'an ' ] ifFalse:[ 'a ' ]);
		nextPutAll: className;
		nextPutAll: '('.
	self printDetailsOn: aStream.
	aStream nextPutAll: ')'.
! !
!CypressObject methodsFor: 'accessing'!
resolveGlobalNamed: aString

	^self resolveGlobalNamed: aString
		or: [CypressError signal: 'Could not resolve global named ' , aString printString]! !
!CypressObject methodsFor: 'accessing'!
resolveGlobalNamed: aString or: aBlock

	^((System myUserProfile resolveSymbol: aString) ifNil: [^aBlock value])
		value! !
!CypressObject methodsFor: 'converting'!
stringForVariables: variableList

	^String streamContents: 
			[:stream |
			variableList do: [:each | stream nextPutAll: each]
				separatedBy: [stream space]]! !
!CypressObject methodsFor: 'accessing'!
symbolDictionaryForClassNamed: aString
	"Answer the SymbolDictionary containing the named class.
	 If there are multiple answers, answer the first.
	 If there are no answers (i.e., the class does not exist), put it in UserGlobals."

	^self symbolDictionaryForClassNamed: aString or: [UserGlobals]! !
!CypressObject methodsFor: 'accessing'!
symbolDictionaryForClassNamed: aString or: aBlock
	"Answer the SymbolDictionary containing the named class.
	 If there are multiple answers, answer the first.
	 If there are no answers (i.e., the class does not exist), answer
	 the result of evaluating aBlock."

	^System myUserProfile symbolList asArray
		detect: [:each | each anySatisfy: [:every | every isBehavior and: [every name asString = aString asString]]]
		ifNone: aBlock! !
" Class Implementation for CypressDefinitionIndex"!

" ------------------- Class methods for CypressDefinitionIndex"!

!CypressDefinitionIndex class methodsFor: 'instance creation'!
definitions: aCollection
	^ self new addAll: aCollection
! !
" ------------------- Instance methods for CypressDefinitionIndex"!

!CypressDefinitionIndex methodsFor: 'adding'!
add: aDefinition
	^ self definitionMap at: aDefinition description put: aDefinition
! !
!CypressDefinitionIndex methodsFor: 'adding'!
addAll: aCollection
	aCollection do: [:ea | self add: ea]
! !
!CypressDefinitionIndex methodsFor: 'querying'!
definitionLike: aDefinition ifPresent: foundBlock ifAbsent: errorBlock
	| definition |
	definition := self definitionMap at: aDefinition description ifAbsent: [].
	^ definition
		ifNil: errorBlock
		ifNotNil: [foundBlock value: definition]
! !
!CypressDefinitionIndex methodsFor: 'accessing'!
definitionMap
	definitionMap ifNil: [ definitionMap := Dictionary new ].
	^ definitionMap
! !
!CypressDefinitionIndex methodsFor: 'accessing'!
definitions
	^self definitionMap values
! !
!CypressDefinitionIndex methodsFor: 'removing'!
remove: aDefinition
	self definitionMap removeKey: aDefinition description ifAbsent: []
! !
" Class Implementation for CypressPatchOperation"!

" ------------------- Instance methods for CypressPatchOperation"!

!CypressPatchOperation methodsFor: 'comparing'!
= aPatchOperation
	^aPatchOperation isKindOf: self class
! !
!CypressPatchOperation methodsFor: 'applying'!
applyTo: aCypressLoader

	self subclassResponsibility: #applyTo:
! !
!CypressPatchOperation methodsFor: 'accessing'!
description

	self subclassResponsibility: #description
! !
!CypressPatchOperation methodsFor: 'comparing'!
hash
    ^ self description hash
! !
!CypressPatchOperation methodsFor: 'loading'!
loadClassDefinition

	self subclassResponsibility: #loadClassDefinition
! !
!CypressPatchOperation methodsFor: 'loading'!
loadMethodDefinition

	self subclassResponsibility: #loadMethodDefinition! !
!CypressPatchOperation methodsFor: 'loading'!
postLoadDefinition

	self subclassResponsibility: #postLoadDefinition
! !
!CypressPatchOperation methodsFor: 'printing'!
printDetailsOn: aStream

	aStream nextPutAll: self description.
! !
!CypressPatchOperation methodsFor: 'dependency'!
provisions
	"Answer list of global names defined by this definition"

	self subclassResponsibility: #provisions
! !
!CypressPatchOperation methodsFor: 'dependency'!
requirements
	"Answer list of global names required by this definition"

	self subclassResponsibility: #requirements
! !
!CypressPatchOperation methodsFor: 'loading'!
unloadDefinition

	CypressError signal: 'inappropriate to send #unloadDefinition to an addition or modification operation'
! !
" Class Implementation for CypressModification"!

" ------------------- Class methods for CypressModification"!

!CypressModification class methodsFor: 'instance creation'!
of: base to: target
	^ self new base: base target: target
! !
" ------------------- Instance methods for CypressModification"!

!CypressModification methodsFor: 'initialization'!
= aPatchOperation
	^(super = aPatchOperation) and: [self obsoletion = aPatchOperation obsoletion and: [ self modification = aPatchOperation modification]]
! !
!CypressModification methodsFor: 'applying'!
applyTo: aCypressLoader

	aCypressLoader applyModification: self
! !
!CypressModification methodsFor: 'initialization'!
base: base target: target

	obsoletion := base.
	modification := target.
! !
!CypressModification methodsFor: 'accessing'!
description
    ^ 'modify from: ' , self obsoletion printString , ' to: ' , self modification printString
! !
!CypressModification methodsFor: 'loading'!
loadClassDefinition

	self modification loadClassDefinition.
! !
!CypressModification methodsFor: 'loading'!
loadMethodDefinition

	self modification loadMethodDefinition.
! !
!CypressModification methodsFor: 'accessing'!
modification

	^modification
! !
!CypressModification methodsFor: 'accessing'!
obsoletion

	^obsoletion
! !
!CypressModification methodsFor: 'loading'!
postLoadDefinition
	self modification postLoadOver: self obsoletion
! !
!CypressModification methodsFor: 'dependency'!
provisions
	"Answer list of global names defined by this definition"

	^self modification provisions
! !
!CypressModification methodsFor: 'dependency'!
requirements
	"Answer list of global names required by this definition"

	^self modification requirements
! !
" Class Implementation for CypressAddition"!

" ------------------- Class methods for CypressAddition"!

!CypressAddition class methodsFor: 'instance creation'!
of: aDefinition
	^ self new definition: aDefinition
! !
" ------------------- Instance methods for CypressAddition"!

!CypressAddition methodsFor: 'comparing'!
= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
! !
!CypressAddition methodsFor: 'applying'!
applyTo: aCypressLoader

	aCypressLoader applyAddition: self
! !
!CypressAddition methodsFor: 'accessing'!
definition

	^definition
! !
!CypressAddition methodsFor: 'initialization'!
definition: aDefinition

	definition := aDefinition
! !
!CypressAddition methodsFor: 'accessing'!
description
    ^ 'add: ' , self definition printString
! !
!CypressAddition methodsFor: 'loading'!
loadClassDefinition

	self definition loadClassDefinition.
! !
!CypressAddition methodsFor: 'loading'!
loadMethodDefinition

	self definition loadMethodDefinition.
! !
!CypressAddition methodsFor: 'loading'!
postLoadDefinition
	self definition postLoadOver: nil
! !
!CypressAddition methodsFor: 'dependency'!
provisions
	"Answer list of global names defined by this definition"

	^self definition provisions
! !
!CypressAddition methodsFor: 'dependency'!
requirements
	"Answer list of global names required by this definition"

	^self definition requirements
! !
" Class Implementation for CypressRemoval"!

" ------------------- Class methods for CypressRemoval"!

!CypressRemoval class methodsFor: 'instance creation'!
of: aDefinition
	^ self new definition: aDefinition
! !
" ------------------- Instance methods for CypressRemoval"!

!CypressRemoval methodsFor: 'comparing'!
= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
! !
!CypressRemoval methodsFor: 'applying'!
applyTo: aCypressLoader

	aCypressLoader applyRemoval: self
! !
!CypressRemoval methodsFor: 'accessing'!
definition

	^definition
! !
!CypressRemoval methodsFor: 'initialization'!
definition: aDefinition

	definition := aDefinition
! !
!CypressRemoval methodsFor: 'accessing'!
description

	^'remove: ', self definition printString
! !
!CypressRemoval methodsFor: 'loading'!
loadClassDefinition
	
	CypressError signal: 'inappropriate to send #loadClassDefinition to a removal operation'
! !
!CypressRemoval methodsFor: 'loading'!
loadMethodDefinition
	
	CypressError signal: 'inappropriate to send #loadMethodDefinition to a removal operation'
! !
!CypressRemoval methodsFor: 'loading'!
postLoadDefinition
	
	CypressError signal: 'inappropriate to send #postLoadDefinition to a removal operation'
! !
!CypressRemoval methodsFor: 'dependency'!
provisions
	"Answer list of global names defined by this definition"

	^#()
! !
!CypressRemoval methodsFor: 'dependency'!
requirements
	"Answer list of global names required by this definition"

	^#()
! !
!CypressRemoval methodsFor: 'loading'!
unloadDefinition

	self definition unloadDefinition.
! !
" Class Implementation for CypressSnapshot"!

" ------------------- Class methods for CypressSnapshot"!

!CypressSnapshot class methodsFor: 'instance creation'!
definitions: aDefinitions

	^(self new) definitions: aDefinitions
! !
" ------------------- Instance methods for CypressSnapshot"!

!CypressSnapshot methodsFor: 'comparing'!
= other
	^ definitions asArray = other definitions asArray
! !
!CypressSnapshot methodsFor: 'enumerating'!
classDefinitions: classBlock methodDefinitions: methodBlock

	self definitions do: [:definition |
		definition classDefinition: classBlock methodDefinition: methodBlock]
! !
!CypressSnapshot methodsFor: 'accessing'!
definitions

	^definitions
! !
!CypressSnapshot methodsFor: 'accessing'!
definitions: aDefinitions

	definitions := aDefinitions
! !
!CypressSnapshot methodsFor: 'patching'!
patchRelativeToBase: aSnapshot
	^ CypressPatch fromBase: aSnapshot toTarget: self
! !
!CypressSnapshot methodsFor: 'loading'!
updatePackage: aPackage
	"Answer the loader used to apply the update."

	^CypressLoader updatePackage: aPackage withSnapshot: self
! !
" Class Implementation for CypressPackageDefinition"!

" ------------------- Class methods for CypressPackageDefinition"!

!CypressPackageDefinition class methodsFor: 'instance creation'!
named: aString

	^self new
		name: aString;
		yourself.
! !
" ------------------- Instance methods for CypressPackageDefinition"!

!CypressPackageDefinition methodsFor: 'comparing'!
= other
	^ other species = self species and: [other name sameAs: name]
! !
!CypressPackageDefinition methodsFor: 'snapshotting'!
addClass: aClass toDefinitions: definitions

	definitions add: aClass asCypressClassDefinition! !
!CypressPackageDefinition methodsFor: 'snapshotting'!
addExtensionMethodsFromClass: aClass toMap: classMap

	| defs map |
	defs := classMap at: aClass theNonMetaClass ifAbsent: [OrderedCollection new.].
	map := Dictionary new.
	aClass categorysDo: 
			[:category :selectors |
			(category asLowercase beginsWith: '*' , self name asLowercase)
				ifTrue: [map at: category put: selectors asSortedCollection]].
	map keys asSortedCollection do: 
			[:category |
			(map at: category)
				do: [:selector | defs add: (aClass compiledMethodAt: selector) asCypressMethodDefinition]].
	defs notEmpty ifTrue: [classMap at: aClass theNonMetaClass put: defs]! !
!CypressPackageDefinition methodsFor: 'snapshotting'!
addMethodsFromClass: aClass toDefinitions: definitions
	"Add only those methods which are not extensions from other packages."

	((aClass methodDictionary reject: [:each | each category first = $*])
		asSortedCollection: [:a :b | a selector <= b selector])
			do: [:method | definitions add: method asCypressMethodDefinition]! !
!CypressPackageDefinition methodsFor: 'accessing'!
classes

	^self classesInPackageNamed: self name! !
!CypressPackageDefinition methodsFor: 'accessing'!
name
	^ name
! !
!CypressPackageDefinition methodsFor: 'accessing'!
name: aString
	name := aString
! !
!CypressPackageDefinition methodsFor: 'printing'!
printDetailsOn: aStream

	aStream nextPutAll: name
! !
!CypressPackageDefinition methodsFor: 'snapshotting'!
snapshot

	| classDefinitions methodDefinitions classMap |
	classDefinitions := OrderedCollection new.
	methodDefinitions := OrderedCollection new.
	(self determineClassHierarchicalOrder: self classes) do: 
			[:cls |
			self
				addClass: cls toDefinitions: classDefinitions;
				addMethodsFromClass: cls toDefinitions: methodDefinitions;
				addMethodsFromClass: cls class toDefinitions: methodDefinitions].
	classMap := Dictionary new.
	self allClasses do: 
			[:each |
			self
				addExtensionMethodsFromClass: each toMap: classMap;
				addExtensionMethodsFromClass: each class toMap: classMap].
	(self determineClassHierarchicalOrder: classMap keys)
		do: [:aClass | methodDefinitions addAll: (classMap at: aClass)].
	^CypressSnapshot definitions: classDefinitions, methodDefinitions! !
" Class Implementation for CypressDependencySorter"!

" ------------------- Instance methods for CypressDependencySorter"!

!CypressDependencySorter methodsFor: 'building'!
add: aPatchOperation
	| requirements |
	requirements := self unresolvedRequirementsFor: aPatchOperation.
	requirements isEmpty
		ifTrue: [self addToOrder: aPatchOperation]
		ifFalse: [self addRequirements: requirements for: aPatchOperation].
	^ aPatchOperation
! !
!CypressDependencySorter methodsFor: 'building'!
addAll: aCollection
	aCollection do: [:aPatchOperation | self add: aPatchOperation ]
! !
!CypressDependencySorter methodsFor: 'private'!
addExternalProvisions: aCollection
	(aCollection intersection: self externalRequirements)
		do: [:globalName | self addProvision: globalName]
! !
!CypressDependencySorter methodsFor: 'private'!
addProvision: aGlobalName
	| newlySatisfied |
	self provided add: aGlobalName.
	newlySatisfied := self required removeKey: aGlobalName ifAbsent: [#()].
	self addAll: newlySatisfied.
! !
!CypressDependencySorter methodsFor: 'private'!
addRequirement: globalName for: aPatchOperation
	(self itemsRequiring: globalName) add: aPatchOperation
! !
!CypressDependencySorter methodsFor: 'private'!
addRequirements: aCollection for: aPatchOperation
	aCollection do: [:globalName | self addRequirement: globalName for: aPatchOperation]
! !
!CypressDependencySorter methodsFor: 'private'!
addToOrder: aPatchOperation
	self orderedItems add: aPatchOperation.
	aPatchOperation provisions do: [:globalName | self addProvision: globalName ].
! !
!CypressDependencySorter methodsFor: 'accessing'!
externalRequirements
	| unloaded providedByUnloaded |
	unloaded := self itemsWithMissingRequirements.
	providedByUnloaded := (unloaded gather: [:e | e provisions]) asSet.
	^ self required keys reject: [:globalName | providedByUnloaded includes: globalName ]
! !
!CypressDependencySorter methodsFor: 'private'!
itemsRequiring: globalName
	^ self required at: globalName ifAbsentPut: [Set new]
! !
!CypressDependencySorter methodsFor: 'accessing'!
itemsWithMissingRequirements
	| patchOperations |
	patchOperations := Set new.
	self required values do: [:aSetOfPatchOperations | patchOperations addAll: aSetOfPatchOperations ].
	^ patchOperations
! !
!CypressDependencySorter methodsFor: 'accessing'!
orderedItems
	"ordered list of patch operations"

	orderedItems ifNil: [ orderedItems := OrderedCollection new ].
	^orderedItems
! !
!CypressDependencySorter methodsFor: 'accessing'!
provided
	"set of global names provided by definitions already loaded"

	provided ifNil: [ provided := Set new ].
	^provided
! !
!CypressDependencySorter methodsFor: 'accessing'!
required
	"dictionary of required global name mapped to list of definitions that require the global"

	required ifNil: [ required := Dictionary new ].
	^required
! !
!CypressDependencySorter methodsFor: 'private'!
unresolvedRequirementsFor: aPatchOperation
	"Answer a list of global names that are required by <aPatchOperation>, but not 
	 provided by patchOperations that have already been processed"

	^ aPatchOperation requirements difference: self provided
! !
" Class Implementation for CypressDefinition"!

" ------------------- Instance methods for CypressDefinition"!

!CypressDefinition methodsFor: 'comparing'!
= aDefinition

	^(aDefinition isKindOf: CypressDefinition)
		and: [aDefinition description = self description]! !
!CypressDefinition methodsFor: 'loading'!
actualClass

	self subclassResponsibility: #actualClass
! !
!CypressDefinition methodsFor: 'visiting'!
classDefinition: classBlock methodDefinition: methodBlock
	"default is noop"
! !
!CypressDefinition methodsFor: 'accessing'!
description
	self subclassResponsibility: #description
! !
!CypressDefinition methodsFor: 'comparing'!
hash
    ^ self description hash
! !
!CypressDefinition methodsFor: 'testing'!
isSameRevisionAs: aDefinition
	^ self = aDefinition
! !
!CypressDefinition methodsFor: 'loading'!
loadClassDefinition
	"default is to do nothing"
! !
!CypressDefinition methodsFor: 'loading'!
loadMethodDefinition
	"default is to do nothing"
! !
!CypressDefinition methodsFor: 'loading'!
postLoad
	"noop"
! !
!CypressDefinition methodsFor: 'loading'!
postLoadOver: aDefinition

	self postLoad
! !
!CypressDefinition methodsFor: 'dependency'!
provisions
	"Answer list of global names defined by this definition"

	^#()
! !
!CypressDefinition methodsFor: 'dependency'!
requirements
	"Answer list of global names required by this definition"

	^#()
! !
!CypressDefinition methodsFor: 'loading'!
unloadDefinition

	self subclassResponsibility: #unloadDefinition
! !
" Class Implementation for CypressClassDefinition"!

" ------------------- Class methods for CypressClassDefinition"!

!CypressClassDefinition class methodsFor: 'instance creation'!
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment subclassType: subclassType

	^self new
		name: aClassName asString
		superclassName: aSuperclassName asString
		category: aCategory asString
		instVarNames: (someInstanceVariableNames asArray
				collect: [:each | each asString])
		classInstVarNames: (someClassInstanceVariableNames asArray
				collect: [:each | each asString])
		classVarNames: (someClassVariableNames asArray
				collect: [:each | each asString])
		poolDictionaryNames: (somePoolDictionaryNames asArray
				collect: [:each | each asString])
		comment: (self normalizeLineEndings: aComment)
		subclassType: subclassType! !
" ------------------- Instance methods for CypressClassDefinition"!

!CypressClassDefinition methodsFor: 'comparing'!
= aDefinition
	^(super = aDefinition)
		and: [superclassName = aDefinition superclassName
		and: [category = aDefinition category
		and: [instVarNames = aDefinition instVarNames
		and: [classInstVarNames = aDefinition classInstVarNames
		and: [classVarNames = aDefinition classVarNames
		and: [poolDictionaryNames = aDefinition poolDictionaryNames
		and: [comment = aDefinition comment]]]]]]]
! !
!CypressClassDefinition methodsFor: 'loading'!
actualClass

	^self resolveGlobalNamed: self name
! !
!CypressClassDefinition methodsFor: 'loading'!
actualClassOrNil

	^self resolveGlobalNamed: self name or: [nil]
! !
!CypressClassDefinition methodsFor: 'converting'!
asCypressClassDefinition

	^self
! !
!CypressClassDefinition methodsFor: 'accessing'!
category

	^category
! !
!CypressClassDefinition methodsFor: 'visiting'!
classDefinition: classBlock methodDefinition: methodBlock

	classBlock value: self
! !
!CypressClassDefinition methodsFor: 'private'!
classInstanceVariablesString
    ^ self stringForVariables: self classInstVarNames
! !
!CypressClassDefinition methodsFor: 'accessing'!
classInstVarNames

	^classInstVarNames
! !
!CypressClassDefinition methodsFor: 'accessing'!
className

	^self name
! !
!CypressClassDefinition methodsFor: 'loading'!
classNeedingMigration: aClass

	self halt: 'not implemented yet'! !
!CypressClassDefinition methodsFor: 'private'!
classVariablesString
    ^ self stringForVariables: self classVarNames
! !
!CypressClassDefinition methodsFor: 'accessing'!
classVarNames

	^classVarNames
! !
!CypressClassDefinition methodsFor: 'accessing'!
comment

	^comment
! !
!CypressClassDefinition methodsFor: 'loading'!
createOrReviseByteClass
	"To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

	| superClass |
	superClass := self resolveGlobalNamed: self superclassName.
	^(superClass
		byteSubclass: self name
		classVars: (self classVarNames collect: [:each | each asSymbol])
		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])
		poolDictionaries: #()
		inDictionary: (self symbolDictionaryForClassNamed: self name)
		options: #())
			category: category;
			comment: self comment! !
!CypressClassDefinition methodsFor: 'loading'!
createOrReviseClass

	^self subclassType = ''
		ifTrue: [self createOrReviseRegularClass]
		ifFalse: 
			[self subclassType = 'byteSubclass'
				ifTrue: [self createOrReviseByteClass]
				ifFalse: 
					[self subclassType = 'indexableSubclass'
						ifTrue: [self createOrReviseIndexableClass]
						ifFalse: 
							[self error: 'unknown subclass type: ' , self subclassType printString]]]! !
!CypressClassDefinition methodsFor: 'loading'!
createOrReviseIndexableClass
	"To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

	| superClass |
	superClass := self resolveGlobalNamed: self superclassName.
	^(superClass
		indexableSubclass: self name
		instVarNames: (self instVarNames collect: [:each | each asSymbol])
		classVars: (self classVarNames collect: [:each | each asSymbol])
		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])
		poolDictionaries: #()
		inDictionary: (self symbolDictionaryForClassNamed: self name)
		options: #())
			category: category;
			comment: self comment! !
!CypressClassDefinition methodsFor: 'loading'!
createOrReviseRegularClass
	"To be resolved:
		- the question of an 'environment' in which to create the class.
		- the question of which SymbolDictionary in which to create the class.
	 These are perhaps the same question."

	| superClass |
	superClass := self resolveGlobalNamed: self superclassName.
	^(superClass
		subclass: self name
		instVarNames: (self instVarNames collect: [:each | each asSymbol])
		classVars: (self classVarNames collect: [:each | each asSymbol])
		classInstVars: (self classInstVarNames collect: [:each | each asSymbol])
		poolDictionaries: #()
		inDictionary: (self symbolDictionaryForClassNamed: self name)
		options: #())
			category: category;
			comment: self comment! !
!CypressClassDefinition methodsFor: 'accessing'!
description

	^ Array with: name
! !
!CypressClassDefinition methodsFor: 'loading'!
failedCompiledMethods: someCompiledMethods

	someCompiledMethods isEmpty ifTrue: [^self].
	self halt: 'not implemented yet'! !
!CypressClassDefinition methodsFor: 'comparing'!
hash

	| hash |
	hash := name hash.
	hash := superclassName hash bitOr: hash.
	hash := (category ifNil: ['']) hash bitOr: hash.
	instVarNames , classInstVarNames, classVarNames, poolDictionaryNames
		do: [:vName | hash := vName hash bitOr: hash].
	^hash! !
!CypressClassDefinition methodsFor: 'private'!
instanceVariablesString
    ^ self stringForVariables: self instVarNames
! !
!CypressClassDefinition methodsFor: 'accessing'!
instVarNames

	^instVarNames
! !
!CypressClassDefinition methodsFor: 'loading'!
loadClassDefinition
	"Create a new version of the defined class. If the class already exists,
	 copy the behaviors and state from the old version."

	| newClass oldClass |
	oldClass := self actualClassOrNil.
	newClass := self createOrReviseClass.
	(oldClass isNil or: [newClass == oldClass]) ifTrue: [^self].
	self classNeedingMigration: newClass.
	self
		recompileWithSubclassesFrom: oldClass
		to: newClass
		symbolList: System myUserProfile symbolList.
! !
!CypressClassDefinition methodsFor: 'accessing'!
name

	^name
! !
!CypressClassDefinition methodsFor: 'initialization'!
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment subclassType: aSubclassType

	name := aClassName.
	superclassName := aSuperclassName.
	category := aCategory.
	instVarNames := someInstanceVariableNames.
	classInstVarNames := someClassInstanceVariableNames.
	classVarNames := someClassVariableNames.
	poolDictionaryNames := somePoolDictionaryNames.
	comment := aComment.
	subclassType := aSubclassType! !
!CypressClassDefinition methodsFor: 'private'!
poolDictionariesString

	^self stringForVariables: self poolDictionaryNames! !
!CypressClassDefinition methodsFor: 'accessing'!
poolDictionaryNames

	^poolDictionaryNames
! !
!CypressClassDefinition methodsFor: 'printString'!
printDetailsOn: aStream

	aStream nextPutAll: self name
! !
!CypressClassDefinition methodsFor: 'dependency'!
provisions
	"Answer list of global names defined by this definition"

	^{ self name }
! !
!CypressClassDefinition methodsFor: 'loading'!
recompileWithSubclassesFrom: oldClass to: newClass symbolList: aSymbolList

	| olds news removedClassVariables removedSharedPools organizer subclasses newSubclass |
	olds := oldClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].
	news := newClass _classVars ifNil: [#()] ifNotNil: [:vars | vars keys].
	removedClassVariables := olds difference: news.
	removedSharedPools := oldClass sharedPools difference: newClass sharedPools.
	self failedCompiledMethods: (newClass
				_copyMethodsAndVariablesFrom: oldClass
				except: { $V. removedClassVariables. $P. removedSharedPools }
				dictionaries: aSymbolList).
	organizer := ClassOrganizer new.
	subclasses := organizer subclassesOf: oldClass.


	"Do this -after- #subclassesOf:, which has the side effect of replacing the new
	  class with the old class in the organizer"
	organizer addClass: newClass.

	"Iterate over all the first-level subclasses of the old class to create new subclasses"
	subclasses do: 
			[:oldSubclass |
			newSubclass := 
					[oldSubclass definition evaluateInContext: nil symbolList: aSymbolList]
							on: Error
							do: [:ex | ex return: nil].
			(newSubclass notNil and: [newSubclass ~~ oldSubclass])
				ifTrue: 
					[self
						classNeedingMigration: newSubclass;
						recompileWithSubclassesFrom: oldSubclass
							to: newSubclass
							symbolList: aSymbolList]]! !
!CypressClassDefinition methodsFor: 'dependency'!
requirements
	"Answer list of global names required by this definition"

	^{self superclassName}
! !
!CypressClassDefinition methodsFor: 'accessing'!
subclassType

	^subclassType
! !
!CypressClassDefinition methodsFor: 'accessing'!
superclassName

	^superclassName
! !
!CypressClassDefinition methodsFor: 'loading'!
unloadDefinition
	"GemStone could hold multiple definitions of the same class name.
	 Ignore aliased references.
	 Unload only the first one resolved.
	 It is an error if there is not at least one SymbolDictionary holding a
	 class with that name."

	| dictionarySymbolPair |
	dictionarySymbolPair := ((System myUserProfile symbolList
				dictionariesAndSymbolsOf: self actualClass)
					select: [:each | each last = self name asSymbol]) first.
	dictionarySymbolPair first removeKey: dictionarySymbolPair last! !
" Class Implementation for CypressMethodDefinition"!

" ------------------- Class methods for CypressMethodDefinition"!

!CypressMethodDefinition class methodsFor: 'instance creation'!
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	^self new
		className: aName asString
		classIsMeta: isMetaclass
		selector: aSelector asString
		category: aCategory asString
		source: (self normalizeLineEndings: aSource)! !
" ------------------- Instance methods for CypressMethodDefinition"!

!CypressMethodDefinition methodsFor: 'comparing'!
= aDefinition
    ^ super = aDefinition
        and: [ aDefinition source = self source
                and: [ aDefinition category = self category ] ]
! !
!CypressMethodDefinition methodsFor: 'loading'!
actualClass

	| cls |
	cls := self theNonMetaClass.
	^self classIsMeta
		ifTrue: [ cls class ]
		ifFalse: [ cls  ].
! !
!CypressMethodDefinition methodsFor: 'converting'!
asCypressMethodDefinition

	^self
! !
!CypressMethodDefinition methodsFor: 'accessing'!
category

	^category
! !
!CypressMethodDefinition methodsFor: 'visiting'!
classDefinition: classBlock methodDefinition: methodBlock

	methodBlock value: self
! !
!CypressMethodDefinition methodsFor: 'accessing'!
classIsMeta

	^classIsMeta
! !
!CypressMethodDefinition methodsFor: 'accessing'!
className

	^className
! !
!CypressMethodDefinition methodsFor: 'initialization'!
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	className := aName.
	classIsMeta := isMetaclass.
	selector := aSelector.
	category := aCategory.
	source := self normalizeLineEndings: aSource! !
!CypressMethodDefinition methodsFor: 'accessing'!
description
	^ Array	
		with: className
		with: selector
		with: classIsMeta
! !
!CypressMethodDefinition methodsFor: 'comparing'!
hash

	| hash |
	hash := classIsMeta asString hash.
	hash := source hash bitOr: hash.
	hash := category hash bitOr: hash.
	hash := className hash bitOr: hash.
	^hash! !
!CypressMethodDefinition methodsFor: 'visiting'!
instanceMethod: instanceBlock classMethod: classBlock

	^(self classIsMeta
		ifTrue: [ classBlock ]
		ifFalse: [ instanceBlock ]) value: self
! !
!CypressMethodDefinition methodsFor: 'testing'!
isInitializer
	^ self selector = 'initialize' and: [self classIsMeta]
! !
!CypressMethodDefinition methodsFor: 'loading'!
loadMethodDefinition

	self actualClass
		compileMethod: self source
		dictionaries: System myUserProfile symbolList
		category: self category
		environmentId: 0
! !
!CypressMethodDefinition methodsFor: 'loading'!
postLoadOver: aDefinition

	super postLoadOver: aDefinition.
	(self isInitializer
		and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]) 
			ifTrue: [ self theNonMetaClass initialize ].
! !
!CypressMethodDefinition methodsFor: 'printing'!
printDetailsOn: aStream

	aStream
		nextPutAll: self className;
		nextPutAll: (self classIsMeta ifTrue: [' class'] ifFalse: ['']);
		nextPutAll: '>>';
		nextPutAll: self selector.
! !
!CypressMethodDefinition methodsFor: 'dependency'!
requirements
	"Answer list of global names required by this definition"

	^{self className}
! !
!CypressMethodDefinition methodsFor: 'accessing'!
selector

	^selector
! !
!CypressMethodDefinition methodsFor: 'accessing'!
source

	^source
! !
!CypressMethodDefinition methodsFor: 'loading'!
theNonMetaClass

	^self resolveGlobalNamed: self className! !
!CypressMethodDefinition methodsFor: 'loading'!
unloadDefinition

	self actualClass removeSelector: self selector asSymbol
! !
" Class Implementation for CypressPatch"!

" ------------------- Class methods for CypressPatch"!

!CypressPatch class methodsFor: 'instance creation'!
fromBase: baseSnapshot toTarget: targetSnapshot
	^ (self new)
		fromBase: baseSnapshot
		toTarget: targetSnapshot
! !
" ------------------- Instance methods for CypressPatch"!

!CypressPatch methodsFor: 'applying'!
applyTo: aCypressLoader
	operations do: [:ea | ea applyTo: aCypressLoader].
! !
!CypressPatch methodsFor: 'initialization'!
fromBase: baseSnapshot toTarget: targetSnapshot
	| base target |	
	operations := OrderedCollection new.
	base := CypressDefinitionIndex definitions: baseSnapshot definitions.
	target := CypressDefinitionIndex definitions: targetSnapshot definitions.
	
	target definitions do:
		[:t |
		base
			definitionLike: t
			ifPresent: [:b | (b isSameRevisionAs: t) ifFalse: [operations add: (CypressModification of: b to: t)]]
			ifAbsent: [operations add: (CypressAddition of: t)]].
		
	base definitions do:
		[:b |
		target
			definitionLike: b
			ifPresent: [:t | ]
			ifAbsent: [operations add: (CypressRemoval of: b)]]
! !
!CypressPatch methodsFor: 'accessing'!
operations

	^operations
! !
" Class Implementation for CypressLoader"!

" ------------------- Class methods for CypressLoader"!

!CypressLoader class methodsFor: 'loading'!
updatePackage: aPackage withSnapshot: aSnapshot
	"Answer the loader used to apply the update."

	^(self new)
		updatePackage: aPackage withSnapshot: aSnapshot;
		load! !
" ------------------- Instance methods for CypressLoader"!

!CypressLoader methodsFor: 'updating'!
addFailedPatchOperation: aPatchOperation

	self errors add: aPatchOperation
! !
!CypressLoader methodsFor: 'accessing'!
additions

	additions ifNil: [ additions := OrderedCollection new ].
	^additions
! !
!CypressLoader methodsFor: 'loading'!
analyze

	self 
		analyzeAdditions;
		analyzeRemovals
! !
!CypressLoader methodsFor: 'loading'!
analyzeAdditions

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self additions;
		addExternalProvisions: self provisions;
		yourself.
	additions := sorter orderedItems.
	requirements := sorter externalRequirements.
	unloadable := sorter required.
! !
!CypressLoader methodsFor: 'loading'!
analyzeRemovals

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self removals;
		yourself.
	removals := sorter orderedItems reverse.
! !
!CypressLoader methodsFor: 'applying'!
applyAddition: aCypressPatchOperation

	self additions add: aCypressPatchOperation
! !
!CypressLoader methodsFor: 'applying'!
applyModification: aCypressPatchOperation

	self additions add: aCypressPatchOperation
! !
!CypressLoader methodsFor: 'applying'!
applyRemoval: aCypressPatchOperation

	self removals add: aCypressPatchOperation
! !
!CypressLoader methodsFor: 'loading'!
attemptInitialLoad

	self
		resetErrors;
		notifyOnFailedPatchOperations;
		loadAdditions: self additions;
		unloadRemovals: self removals.
! !
!CypressLoader methodsFor: 'loading'!
errorOnFailedPatchOperations

	exceptionClass := CypressLoaderError.
! !
!CypressLoader methodsFor: 'accessing'!
errors
	errors ifNil: [self resetErrors].
	^errors
! !
!CypressLoader methodsFor: 'loading'!
handleCompileError: aCompileError from: aPatchOperation

	| undefinedSymbolErrors otherErrors |
	undefinedSymbolErrors := aCompileError errorDetails
				select: [:each | each first = 1031].
	otherErrors := aCompileError errorDetails
				reject: [:each | each first = 1031].
	undefinedSymbolErrors do: [:each | self requirements add: each last].
	aCompileError pass! !
!CypressLoader methodsFor: 'loading'!
handlePatchOperation: aPatchOperation failure: anException
	"Signal the loader exception appropriate to the current phase.
	 Note that a handler may suppress the #addFailedPatchOperation: by
	 sending #return or #return: to the resignaled exception. Otherwise,
	 resumption from a resumable resignalled exception will continue through
	 this method."

	(exceptionClass patchOperation: aPatchOperation exception: anException) signal.
	self addFailedPatchOperation: aPatchOperation.! !
!CypressLoader methodsFor: 'loading'!
load

	self
		analyze;
		reportUnloadableDefinitions;
		attemptInitialLoad;
		retryFailedLoads;
		postLoad.
! !
!CypressLoader methodsFor: 'loading'!
loadAdditions: somePatchOperations
	"Load class definitions first, then method definitions."

	somePatchOperations
		do: [:each | self loadClassDefinition: each];
		do: [:each | self loadMethodDefinition: each].
! !
!CypressLoader methodsFor: 'operations'!
loadClassDefinition: aPatchOperation

	[aPatchOperation loadClassDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
! !
!CypressLoader methodsFor: 'operations'!
loadMethodDefinition: aPatchOperation

	
	[[aPatchOperation loadMethodDefinition]
		on: CompileError
		do: [:ex | self handleCompileError: ex from: aPatchOperation]]
			on: Error
			do: [:ex | self handlePatchOperation: aPatchOperation failure: ex]! !
!CypressLoader methodsFor: 'accessing'!
methodAdditions

	^#()
! !
!CypressLoader methodsFor: 'loading'!
notifyOnFailedPatchOperations

	exceptionClass := CypressLoaderErrorNotification.
! !
!CypressLoader methodsFor: 'loading'!
postLoad
	"This is where the obsoletion is taken into account ..."

	self additions do: [:each | self postLoad: each].
! !
!CypressLoader methodsFor: 'operations'!
postLoad: aPatchOperation

	[aPatchOperation postLoadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
! !
!CypressLoader methodsFor: 'accessing'!
provisions
	^ provisions ifNil: [provisions := (self allClasses collect: [:cl | cl name asString]) asSet ]
! !
!CypressLoader methodsFor: 'accessing'!
removals

	removals ifNil: [ removals := OrderedCollection new ].
	^removals
! !
!CypressLoader methodsFor: 'loading'!
reportUnloadableDefinitions

	self unloadable isEmpty ifTrue: [^self].
	(CypressLoaderMissingClasses missingRequirementsMap: unloadable) signal.
! !
!CypressLoader methodsFor: 'accessing'!
requirements

	^requirements
! !
!CypressLoader methodsFor: 'loading'!
resetErrors

	errors := OrderedCollection new.
! !
!CypressLoader methodsFor: 'loading'!
retryFailedLoads
	"In case any of the failed loads were resolved by subsequent
	 patch operations after the initial attempt or by editting of the
	 failed patch operations by exception handling during the notification
	 phase (initial attempt)."

	| failed |
	failed := self errors.
	self
		resetErrors;
		errorOnFailedPatchOperations;
		loadAdditions: (self additions intersection: failed);
		unloadRemovals: (self removals intersection: failed).
! !
!CypressLoader methodsFor: 'accessing'!
unloadable

	unloadable ifNil: [ unloadable := OrderedCollection new ].
	^unloadable
! !
!CypressLoader methodsFor: 'operations'!
unloadDefinition: aPatchOperation

	[aPatchOperation unloadDefinition]
		on: Error
		do: [:ex | self handlePatchOperation: aPatchOperation failure: ex].
! !
!CypressLoader methodsFor: 'loading'!
unloadRemovals: somePatchOperations
	"Removals need to be done after adding classes and methods."

	somePatchOperations
		do: [:each | self unloadDefinition: each].
! !
!CypressLoader methodsFor: 'loading'!
updatePackage: aPackage withSnapshot: aSnapshot

	| patch snapshot |
	snapshot := aPackage snapshot.
	patch := aSnapshot patchRelativeToBase: snapshot.
	patch applyTo: self.
	snapshot definitions do: [:ea | self provisions addAll: ea provisions]! !
" Class Extensions "!

" Class Extension for Class"!

" ------------------- Instance methods for Class"!

!Class methodsFor: '*Cypress-Definitions'!
asCypressClassDefinition

	^CypressClassDefinition
		name: self name
		superclassName: self superclass name
		category: self category
		instVarNames: self instVarNames
		classInstVarNames: self class instVarNames
		classVarNames: self classVarNames
		poolDictionaryNames: self sharedPools
		comment: self comment
		subclassType: self subclassType.
! !
!Class methodsFor: '*Cypress-Definitions'!
subclassType
	"Answer a description of the receiver to identify whether it is a regular class,
	 a byte subclass, or an indexable subclass."

	^(self isBytes and: [self superClass isBytes not])
		ifTrue: ['byteSubclass']
		ifFalse: 
			[(self isIndexable and: [self superClass isIndexable not])
				ifTrue: ['indexableSubclass']
				ifFalse: ['']]! !
" Class Extension for GsNMethod"!

" ------------------- Instance methods for GsNMethod"!

!GsNMethod methodsFor: '*Cypress-Definitions'!
asCypressMethodDefinition
	"Try to coerce Unicode source to simple Strings when possible."

	^CypressMethodDefinition
		className: self methodClass theNonMetaClass name
		classIsMeta: self methodClass isMeta
		selector: self selector
		category: self category
		source: self sourceString asString! !
" Class initializers "!




" End of Package: Cypress-Definitions"!


