doit
(Object subclass: 'CypressObject'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressObject subclass: 'CypressDefinition'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressDefinition subclass: 'CypressClassDefinition'  instVarNames: #( name superclassName category                    comment instVarNames classInstVarNames)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressDefinition subclass: 'CypressMethodDefinition'  instVarNames: #( classIsMeta source category                    selector className)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressObject subclass: 'CypressDefinitionIndex'  instVarNames: #( definitionMap)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressObject subclass: 'CypressDependencySorter'  instVarNames: #( required provided orderedItems)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressObject subclass: 'CypressLoader'  instVarNames: #( additions removals unloadable                    provisions errors methodAdditions requirements)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressObject subclass: 'CypressPackageDefinition'  instVarNames: #( name)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressObject subclass: 'CypressPatch'  instVarNames: #( operations)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressObject subclass: 'CypressPatchOperation'  instVarNames: #()  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressPatchOperation subclass: 'CypressAddition'  instVarNames: #( definition)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressPatchOperation subclass: 'CypressModification'  instVarNames: #( obsoletion modification)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressPatchOperation subclass: 'CypressRemoval'  instVarNames: #( definition)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
(CypressObject subclass: 'CypressSnapshot'  instVarNames: #( definitions)  classVars: #()  classInstVars: #()  poolDictionaries: #()  inDictionary: UserGlobals  options: #()) category: 'Cypress-Definitions'
.
%
doit
CypressObject immediateInvariant.
%
doit
CypressDefinition immediateInvariant.
%
doit
CypressClassDefinition immediateInvariant.
%
doit
CypressMethodDefinition immediateInvariant.
%
doit
CypressDefinitionIndex immediateInvariant.
%
doit
CypressDependencySorter immediateInvariant.
%
doit
CypressLoader immediateInvariant.
%
doit
CypressPackageDefinition immediateInvariant.
%
doit
CypressPatch immediateInvariant.
%
doit
CypressPatchOperation immediateInvariant.
%
doit
CypressAddition immediateInvariant.
%
doit
CypressModification immediateInvariant.
%
doit
CypressRemoval immediateInvariant.
%
doit
CypressSnapshot immediateInvariant.
%

! Remove existing behavior from CypressObject
doit
CypressObject removeAllMethods.
CypressObject class removeAllMethods.
%
! ------------------- Class methods for CypressObject
! ------------------- Instance methods for CypressObject
category: 'initializing'
set compile_env: 0
method: CypressObject
initialize
	"Placeholder: #initialize is not defined by Object in GemStone Smalltalk."
%

! Remove existing behavior from CypressDefinition
doit
CypressDefinition removeAllMethods.
CypressDefinition class removeAllMethods.
%
! ------------------- Class methods for CypressDefinition
! ------------------- Instance methods for CypressDefinition
category: 'accessing'
set compile_env: 0
method: CypressDefinition
description
	self subclassResponsibility
%
category: 'comparing'
set compile_env: 0
method: CypressDefinition
= aDefinition
	^(aDefinition isKindOf: CypressDefinition) and: [self isRevisionOf: aDefinition]
%
category: 'comparing'
set compile_env: 0
method: CypressDefinition
hash
    ^ self description hash
%
category: 'dependency'
set compile_env: 0
method: CypressDefinition
provisions
	"Answer list of global names defined by this definition"

	^#()
%
category: 'dependency'
set compile_env: 0
method: CypressDefinition
requirements
	"Answer list of global names required by this definition"

	^#()
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
actualClass

	self subclassResponsibility
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
loadClassDefinition
	"default is to do nothing"
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
loadMethodDefinition
	"default is to do nothing"
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
postLoad
	"noop"
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
postLoadOver: aDefinition

	self postLoad
%
category: 'loading'
set compile_env: 0
method: CypressDefinition
unloadDefinition

	self subclassResponsibility
%
category: 'testing'
set compile_env: 0
method: CypressDefinition
isRevisionOf: aDefinition
	^ (aDefinition isKindOf: CypressDefinition) and: [aDefinition description = self description]
%
category: 'testing'
set compile_env: 0
method: CypressDefinition
isSameRevisionAs: aDefinition
	^ self = aDefinition
%
category: 'visiting'
set compile_env: 0
method: CypressDefinition
classDefinition: classBlock methodDefinition: methodBlock
	"default is noop"
%

! Remove existing behavior from CypressClassDefinition
doit
CypressClassDefinition removeAllMethods.
CypressClassDefinition class removeAllMethods.
%
! ------------------- Class methods for CypressClassDefinition
category: 'instance creation'
set compile_env: 0
classmethod: CypressClassDefinition
name: aClassName 
superclassName: aSuperclassName
category: aCategory
instVarNames: anInstanceVariableNames
classInstVarNames: aClassInstanceVariableNames
comment: aComment

	^(self new) 
		name: aClassName 
		superclassName: aSuperclassName
		category: aCategory
		instVarNames: anInstanceVariableNames
		classInstVarNames: aClassInstanceVariableNames
		comment: aComment
%
! ------------------- Instance methods for CypressClassDefinition
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
category

	^category
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
classInstVarNames

	^classInstVarNames
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
className

	^self name
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
comment

	^comment
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
description

	^ Array with: name
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
instVarNames

	^instVarNames
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
name

	^name
%
category: 'accessing'
set compile_env: 0
method: CypressClassDefinition
superclassName

	^superclassName
%
category: 'comparing'
set compile_env: 0
method: CypressClassDefinition
= aDefinition
	^(super = aDefinition)
		and: [superclassName = aDefinition superclassName
		and: [category = aDefinition category
		and: [instVarNames = aDefinition instVarNames
		and: [classInstVarNames = aDefinition classInstVarNames
		and: [comment = aDefinition comment]]]]]
%
category: 'comparing'
set compile_env: 0
method: CypressClassDefinition
hash
    | hash |
    hash := String stringHash: name initialHash: 0.
    hash := String stringHash: superclassName initialHash: hash.
    hash := String stringHash: (category ifNil: [ '' ]) initialHash: hash.
    instVarNames , classInstVarNames do: [ :vName | hash := String stringHash: vName initialHash: hash ].
    ^ hash
%
category: 'converting'
set compile_env: 0
method: CypressClassDefinition
asCypressClassDefinition

	^self
%
category: 'dependency'
set compile_env: 0
method: CypressClassDefinition
provisions
	"Answer list of global names defined by this definition"

	^{ self name }
%
category: 'dependency'
set compile_env: 0
method: CypressClassDefinition
requirements
	"Answer list of global names required by this definition"

	^{self name}
%
category: 'initialization'
set compile_env: 0
method: CypressClassDefinition
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: anInstanceVariableNames classInstVarNames: aClassInstanceVariableNames comment: aComment

	name := aClassName.
	superclassName := aSuperclassName.
	category := aCategory.
	instVarNames := anInstanceVariableNames.
	classInstVarNames := aClassInstanceVariableNames.
	comment := aComment
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
actualClass

	^Smalltalk current at: self name
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
createClass
    | superClass |
    superClass := Smalltalk globals at: self superclassName.
    ^ ClassBuilder new
        name: self name
        inEnvironment: superClass environment
        subclassOf: superClass
        type: #'normal'
        instanceVariableNames: self instanceVariablesString
        classVariableNames: self classInstanceVariablesString
        poolDictionaries: ''
        category: category
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
loadClassDefinition

	 | cls |
	cls := self createClass.
	cls class instanceVariableNames: self classInstanceVariablesString.
	self comment notEmpty ifTrue: [ cls comment: self comment ]
%
category: 'loading'
set compile_env: 0
method: CypressClassDefinition
unloadDefinition

	Smalltalk current removeClass: self actualClass.
%
category: 'printString'
set compile_env: 0
method: CypressClassDefinition
printString

	| str |
	str := WriteStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self name;
		nextPutAll: ')'.
	^str contents
%
category: 'private'
set compile_env: 0
method: CypressClassDefinition
classInstanceVariablesString
    ^ self stringForVariables: self classInstVarNames
%
category: 'private'
set compile_env: 0
method: CypressClassDefinition
instanceVariablesString
    ^ self stringForVariables: self instVarNames
%
category: 'private'
set compile_env: 0
method: CypressClassDefinition
stringForVariables: variableList
    ^ String
        streamContents: [ :stream | variableList do: [ :ea | stream nextPutAll: ea ] separatedBy: [ stream space ] ]
%
category: 'visiting'
set compile_env: 0
method: CypressClassDefinition
classDefinition: classBlock methodDefinition: methodBlock

	classBlock value: self
%

! Remove existing behavior from CypressMethodDefinition
doit
CypressMethodDefinition removeAllMethods.
CypressMethodDefinition class removeAllMethods.
%
! ------------------- Class methods for CypressMethodDefinition
category: 'instance creation'
set compile_env: 0
classmethod: CypressMethodDefinition
className: aName
classIsMeta: isMetaclass
selector: aSelector
category: aCategory
source: aSource

	^(self new)
		className: aName
		classIsMeta: isMetaclass
		selector: aSelector
		category: aCategory
		source: aSource
%
! ------------------- Instance methods for CypressMethodDefinition
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
category

	^category
%
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
classIsMeta

	^classIsMeta
%
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
className

	^className
%
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
description
	^ Array	
		with: className
		with: selector
		with: classIsMeta
%
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
selector

	^selector
%
category: 'accessing'
set compile_env: 0
method: CypressMethodDefinition
source

	^source
%
category: 'comparing'
set compile_env: 0
method: CypressMethodDefinition
= aDefinition
    ^ super = aDefinition
        and: [ aDefinition source = self source
                and: [ aDefinition category = self category ] ]
%
category: 'comparing'
set compile_env: 0
method: CypressMethodDefinition
hash
    | hash |
    hash := String stringHash: classIsMeta asString initialHash: 0.
    hash := String stringHash: source initialHash: hash.
    hash := String stringHash: category initialHash: hash.
    hash := String stringHash: className initialHash: hash.
    ^ hash
%
category: 'converting'
set compile_env: 0
method: CypressMethodDefinition
asCypressMethodDefinition

	^self
%
category: 'dependency'
set compile_env: 0
method: CypressMethodDefinition
requirements
	"Answer list of global names required by this definition"

	^{self className}
%
category: 'initialization'
set compile_env: 0
method: CypressMethodDefinition
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource
    className := aName.
    classIsMeta := isMetaclass.
    selector := aSelector.
    category := aCategory.
    source := aSource withSqueakLineEndings
%
category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
actualClass

	| cls |
	cls := self theNonMetaClass.
	^self classIsMeta
		ifTrue: [ cls class ]
		ifFalse: [ cls  ].
%
category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
loadMethodDefinition
    self actualClass compile: self source classified: self category
%
category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
postLoadOver: aDefinition

	super postLoadOver: aDefinition.
	(self isInitializer
		and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]) 
			ifTrue: [ self theNonMetaClass initialize ].
%
category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
theNonMetaClass
	^Smalltalk globals at: self className asSymbol
%
category: 'loading'
set compile_env: 0
method: CypressMethodDefinition
unloadDefinition

	self actualClass removeSelector: self selector asSymbol
%
category: 'printing'
set compile_env: 0
method: CypressMethodDefinition
printString

	| str |
	str := WriteStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self className.
	self classIsMeta
		ifTrue: [ str nextPutAll: ' class' ].
	str 
		nextPutAll: '>>';
		nextPutAll: self selector;
		nextPutAll: ')'.
	^str contents
%
category: 'testing'
set compile_env: 0
method: CypressMethodDefinition
isInitializer
	^ self selector = 'initialize' and: [self classIsMeta]
%
category: 'visiting'
set compile_env: 0
method: CypressMethodDefinition
classDefinition: classBlock methodDefinition: methodBlock

	methodBlock value: self
%
category: 'visiting'
set compile_env: 0
method: CypressMethodDefinition
instanceMethod: instanceBlock classMethod: classBlock

	^(self classIsMeta
		ifTrue: [ classBlock ]
		ifFalse: [ instanceBlock ]) value: self
%

! Remove existing behavior from CypressDefinitionIndex
doit
CypressDefinitionIndex removeAllMethods.
CypressDefinitionIndex class removeAllMethods.
%
! ------------------- Class methods for CypressDefinitionIndex
category: 'instance creation'
set compile_env: 0
classmethod: CypressDefinitionIndex
definitions: aCollection
	^ self new addAll: aCollection
%
! ------------------- Instance methods for CypressDefinitionIndex
category: 'accessing'
set compile_env: 0
method: CypressDefinitionIndex
definitionMap
	definitionMap ifNil: [ definitionMap := Dictionary new ].
	^ definitionMap
%
category: 'accessing'
set compile_env: 0
method: CypressDefinitionIndex
definitions
	^self definitionMap values
%
category: 'adding'
set compile_env: 0
method: CypressDefinitionIndex
add: aDefinition
	^ self definitionMap at: aDefinition description put: aDefinition
%
category: 'adding'
set compile_env: 0
method: CypressDefinitionIndex
addAll: aCollection
	aCollection do: [:ea | self add: ea]
%
category: 'querying'
set compile_env: 0
method: CypressDefinitionIndex
definitionLike: aDefinition ifPresent: foundBlock ifAbsent: errorBlock
	| definition |
	definition := self definitionMap at: aDefinition description ifAbsent: [].
	^ definition
		ifNil: errorBlock
		ifNotNil: [foundBlock value: definition]
%
category: 'removing'
set compile_env: 0
method: CypressDefinitionIndex
remove: aDefinition
	self definitionMap removeKey: aDefinition description ifAbsent: []
%

! Remove existing behavior from CypressDependencySorter
doit
CypressDependencySorter removeAllMethods.
CypressDependencySorter class removeAllMethods.
%
! ------------------- Class methods for CypressDependencySorter
! ------------------- Instance methods for CypressDependencySorter
category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
externalRequirements
	| unloaded providedByUnloaded |
	unloaded := self itemsWithMissingRequirements.
	providedByUnloaded := (unloaded gather: [:e | e provisions]) asSet.
	^ self required keys reject: [:globalName | providedByUnloaded includes: globalName ]
%
category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
itemsWithMissingRequirements
	| patchOperations |
	patchOperations := Set new.
	self required values do: [:aSetOfPatchOperations | patchOperations addAll: aSetOfPatchOperations ].
	^ patchOperations
%
category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
orderedItems
	"ordered list of patch operations"

	orderedItems ifNil: [ orderedItems := OrderedCollection new ].
	^orderedItems
%
category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
provided
	"set of global names provided by definitions already loaded"

	provided ifNil: [ provided := Set new ].
	^provided
%
category: 'accessing'
set compile_env: 0
method: CypressDependencySorter
required
	"dictionary of required global name mapped to list of definitions that require the global"

	required ifNil: [ required := Dictionary new ].
	^required
%
category: 'building'
set compile_env: 0
method: CypressDependencySorter
add: aPatchOperation
	| requirements |
	requirements := self unresolvedRequirementsFor: aPatchOperation.
	requirements isEmpty
		ifTrue: [self addToOrder: aPatchOperation]
		ifFalse: [self addRequirements: requirements for: aPatchOperation].
	^ aPatchOperation
%
category: 'building'
set compile_env: 0
method: CypressDependencySorter
addAll: aCollection
	aCollection do: [:aPatchOperation | self add: aPatchOperation ]
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
addExternalProvisions: aCollection
	(aCollection intersection: self externalRequirements)
		do: [:globalName | self addProvision: globalName]
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
addProvision: aGlobalName
	| newlySatisfied |
	self provided add: aGlobalName.
	newlySatisfied := self required removeKey: aGlobalName ifAbsent: [#()].
	self addAll: newlySatisfied.
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
addRequirement: globalName for: aPatchOperation
	(self itemsRequiring: globalName) add: aPatchOperation
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
addRequirements: aCollection for: aPatchOperation
	aCollection do: [:globalName | self addRequirement: globalName for: aPatchOperation]
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
addToOrder: aPatchOperation
	self orderedItems add: aPatchOperation.
	aPatchOperation provisions do: [:globalName | self addProvision: globalName ].
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
itemsRequiring: globalName
	^ self required at: globalName ifAbsentPut: [Set new]
%
category: 'private'
set compile_env: 0
method: CypressDependencySorter
unresolvedRequirementsFor: aPatchOperation
	"Answer a list of global names that are required by <aPatchOperation>, but not 
	 provided by patchOperations that have already been processed"

	^ aPatchOperation requirements difference: self provided
%

! Remove existing behavior from CypressLoader
doit
CypressLoader removeAllMethods.
CypressLoader class removeAllMethods.
%
! ------------------- Class methods for CypressLoader
category: 'loading'
set compile_env: 0
classmethod: CypressLoader
updatePackage: aPackage withSnapshot: aSnapshot
	self new
		updatePackage: aPackage withSnapshot: aSnapshot;
		load
%
! ------------------- Instance methods for CypressLoader
category: 'accessing'
set compile_env: 0
method: CypressLoader
additions

	additions ifNil: [ additions := OrderedCollection new ].
	^additions
%
category: 'accessing'
set compile_env: 0
method: CypressLoader
errors
	errors ifNil: [ errors := OrderedCollection new ].
	^errors
%
category: 'accessing'
set compile_env: 0
method: CypressLoader
methodAdditions

	^#()
%
category: 'accessing'
set compile_env: 0
method: CypressLoader
provisions
	^ provisions ifNil: [provisions := (Smalltalk allClasses collect: [:cl | cl name]) asSet ]
%
category: 'accessing'
set compile_env: 0
method: CypressLoader
removals

	removals ifNil: [ removals := OrderedCollection new ].
	^removals
%
category: 'accessing'
set compile_env: 0
method: CypressLoader
unloadable

	unloadable ifNil: [ unloadable := OrderedCollection new ].
	^unloadable
%
category: 'applying'
set compile_env: 0
method: CypressLoader
applyAddition: aCypressPatchOperation

	self additions add: aCypressPatchOperation
%
category: 'applying'
set compile_env: 0
method: CypressLoader
applyModification: aCypressPatchOperation

	self additions add: aCypressPatchOperation
%
category: 'applying'
set compile_env: 0
method: CypressLoader
applyRemoval: aCypressPatchOperation

	self removals add: aCypressPatchOperation
%
category: 'loading'
set compile_env: 0
method: CypressLoader
analyze

	self 
		analyzeAdditions;
		analyzeRemovals
%
category: 'loading'
set compile_env: 0
method: CypressLoader
analyzeAdditions

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self additions;
		addExternalProvisions: self provisions;
		yourself.
	additions := sorter orderedItems.
	requirements := sorter externalRequirements.
	unloadable := sorter itemsWithMissingRequirements.
%
category: 'loading'
set compile_env: 0
method: CypressLoader
analyzeRemovals

	| sorter |
	sorter := CypressDependencySorter new 
		addAll: self removals;
		yourself.
	removals := sorter orderedItems reversed.
%
category: 'loading'
set compile_env: 0
method: CypressLoader
basicLoad
	errors := OrderedCollection new.
	self additions do: [:ea | self loadClassDefinition: ea ]. "load class definitions first"
	self additions do: [:ea | self loadMethodDefinition: ea ] . "load method definitions now"
	self removals do: [:ea | self unloadDefinition: ea ]. "now we can remove things"
	self errors do: [:ea | ea addMethodAdditionTo: methodAdditions]. "not sure about methodAddtions...yet"
	self methodAdditions do: [:ea | self loadMethodAddition: ea ]. "ditto"
	self additions do: [:ea | self postLoad: ea ]. "this is where the obsoletion is taken into account ..."
%
category: 'loading'
set compile_env: 0
method: CypressLoader
load

	self analyze.
	self unloadable isEmpty ifFalse: [self unloadableDefinitionsError].
	self basicLoad
%
category: 'loading'
set compile_env: 0
method: CypressLoader
updatePackage: aPackage withSnapshot: aSnapshot
	|  patch snapshot |
	snapshot := aPackage snapshot.
	patch := aSnapshot patchRelativeToBase: snapshot.
	patch applyTo: self.
	snapshot definitions do: [:ea | self provisions addAll: ea provisions]
%
category: 'operations'
set compile_env: 0
method: CypressLoader
loadClassDefinition: aPatchOperation
    [ aPatchOperation loadClassDefinition ]
        on: Error
        do: [ :ex | self errors add: aPatchOperation ]
%
category: 'operations'
set compile_env: 0
method: CypressLoader
loadMethodDefinition: aPatchOperation
    [ aPatchOperation loadMethodDefinition ]
        on: Error
        do: [ :ex | self errors add: aPatchOperation ]
%
category: 'operations'
set compile_env: 0
method: CypressLoader
postLoad: aPatchOperation
	aPatchOperation postLoadDefinition
%
category: 'operations'
set compile_env: 0
method: CypressLoader
unloadDefinition: aPatchOperation
    [ aPatchOperation unloadDefinition ]
        on: Error
        do: [ :ex | self errors add: aPatchOperation ]
%

! Remove existing behavior from CypressPackageDefinition
doit
CypressPackageDefinition removeAllMethods.
CypressPackageDefinition class removeAllMethods.
%
! ------------------- Class methods for CypressPackageDefinition
! ------------------- Instance methods for CypressPackageDefinition
category: 'accessing'
set compile_env: 0
method: CypressPackageDefinition
name
	^ name
%
category: 'accessing'
set compile_env: 0
method: CypressPackageDefinition
name: aString
	name := aString
%
category: 'comparing'
set compile_env: 0
method: CypressPackageDefinition
= other
	^ other species = self species and: [other name sameAs: name]
%
category: 'printing'
set compile_env: 0
method: CypressPackageDefinition
printString
	^super printString, '(', name, ')'
%
category: 'snapshotting'
set compile_env: 0
method: CypressPackageDefinition
snapshot
    | package definitions map classMap |
    package := MCPackage named: self name.
    definitions := OrderedCollection new.
    (Class superclassOrder: package packageInfo classes)
        do: [ :cls | 
            definitions add: cls asCypressClassDefinition.
            (cls methodDictionary values sorted: [ :a :b | a selector <= b selector ])
                do: [ :method | 
                    (method category at: 1) = $*
                        ifFalse: [ definitions add: method asCypressMethodDefinition ] ].
            (cls class methodDictionary values sorted: [ :a :b | a selector <= b selector ])
                do: [ :method | 
                    (method category at: 1) = $*
                        ifFalse: [ definitions add: method asCypressMethodDefinition ] ] ].
    classMap := Dictionary new.
    Smalltalk allClasses
        do: [ :each | 
            {each.
            (each class)}
                do: [ :aClass | 
                    | defs |
                    defs := OrderedCollection new.
                    map := Dictionary new.
                    aClass organization categories
                        do: [ :category | 
                            | methods |
                            methods := aClass organization listAtCategoryNamed: category.
                            (category asLowercase beginsWith: '*' , self name asLowercase)
                                ifTrue: [ map at: category put: methods ] ].
                    (map keys sorted: [ :a :b | a <= b ])
                        do: [ :category | 
                            ((map at: category) sorted: [ :a :b | a selector <= b selector ])
                                do: [ :method | defs add: (aClass compiledMethodAt: method) asCypressMethodDefinition ] ].
                    defs notEmpty
                        ifTrue: [ classMap at: each put: defs ] ] ].
    (Class superclassOrder: classMap keys) do: [ :aClass | definitions addAll: (classMap at: aClass) ].
    ^ CypressSnapshot definitions: definitions
%

! Remove existing behavior from CypressPatch
doit
CypressPatch removeAllMethods.
CypressPatch class removeAllMethods.
%
! ------------------- Class methods for CypressPatch
category: 'instance creation'
set compile_env: 0
classmethod: CypressPatch
fromBase: baseSnapshot toTarget: targetSnapshot
	^ (self new)
		fromBase: baseSnapshot
		toTarget: targetSnapshot
%
! ------------------- Instance methods for CypressPatch
category: 'accessing'
set compile_env: 0
method: CypressPatch
operations

	^operations
%
category: 'applying'
set compile_env: 0
method: CypressPatch
applyTo: aCypressLoader
	operations do: [:ea | ea applyTo: aCypressLoader].
%
category: 'initialization'
set compile_env: 0
method: CypressPatch
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
%

! Remove existing behavior from CypressPatchOperation
doit
CypressPatchOperation removeAllMethods.
CypressPatchOperation class removeAllMethods.
%
! ------------------- Class methods for CypressPatchOperation
! ------------------- Instance methods for CypressPatchOperation
category: 'accessing'
set compile_env: 0
method: CypressPatchOperation
description

	self subclassResponsibility
%
category: 'applying'
set compile_env: 0
method: CypressPatchOperation
applyTo: aCypressLoader

	self subclassResponsibility
%
category: 'comparing'
set compile_env: 0
method: CypressPatchOperation
= aPatchOperation
	^aPatchOperation isKindOf: self class
%
category: 'comparing'
set compile_env: 0
method: CypressPatchOperation
hash
    ^ self description hash
%
category: 'dependency'
set compile_env: 0
method: CypressPatchOperation
provisions
	"Answer list of global names defined by this definition"

	self subclassResponsibility
%
category: 'dependency'
set compile_env: 0
method: CypressPatchOperation
requirements
	"Answer list of global names required by this definition"

	self subclassResponsibility
%
category: 'loading'
set compile_env: 0
method: CypressPatchOperation
loadClassDefinition

	self subclassResponsibility
%
category: 'loading'
set compile_env: 0
method: CypressPatchOperation
loadMethodDefinition
	self subclassResponsibility
%
category: 'loading'
set compile_env: 0
method: CypressPatchOperation
postLoadDefinition
	self subclassResponsibility
%
category: 'loading'
set compile_env: 0
method: CypressPatchOperation
unloadDefinition

	self error: 'inappropriate to send #unloadDefinition to an addition or modification operation'
%

! Remove existing behavior from CypressAddition
doit
CypressAddition removeAllMethods.
CypressAddition class removeAllMethods.
%
! ------------------- Class methods for CypressAddition
category: 'instance creation'
set compile_env: 0
classmethod: CypressAddition
of: aDefinition
	^ self new definition: aDefinition
%
! ------------------- Instance methods for CypressAddition
category: 'accessing'
set compile_env: 0
method: CypressAddition
definition

	^definition
%
category: 'accessing'
set compile_env: 0
method: CypressAddition
description
    ^ 'add: ' , self definition printString
%
category: 'applying'
set compile_env: 0
method: CypressAddition
applyTo: aCypressLoader

	aCypressLoader applyAddition: self
%
category: 'comparing'
set compile_env: 0
method: CypressAddition
= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
%
category: 'dependency'
set compile_env: 0
method: CypressAddition
provisions
	"Answer list of global names defined by this definition"

	^self definition provisions
%
category: 'dependency'
set compile_env: 0
method: CypressAddition
requirements
	"Answer list of global names required by this definition"

	^self definition requirements
%
category: 'initialization'
set compile_env: 0
method: CypressAddition
definition: aDefinition

	definition := aDefinition
%
category: 'loading'
set compile_env: 0
method: CypressAddition
loadClassDefinition

	self definition loadClassDefinition
%
category: 'loading'
set compile_env: 0
method: CypressAddition
loadMethodDefinition
	self definition loadMethodDefinition
%
category: 'loading'
set compile_env: 0
method: CypressAddition
postLoadDefinition
	self definition postLoadOver: nil
%
category: 'printing'
set compile_env: 0
method: CypressAddition
printString

	| str |
	str := WriteStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self description;
		nextPutAll: ')'.
	^str contents
%

! Remove existing behavior from CypressModification
doit
CypressModification removeAllMethods.
CypressModification class removeAllMethods.
%
! ------------------- Class methods for CypressModification
category: 'instance creation'
set compile_env: 0
classmethod: CypressModification
of: base to: target
	^ self new base: base target: target
%
! ------------------- Instance methods for CypressModification
category: 'accessing'
set compile_env: 0
method: CypressModification
description
    ^ 'modify from: ' , self obsoletion printString , ' to: ' , self modification printString
%
category: 'accessing'
set compile_env: 0
method: CypressModification
modification

	^modification
%
category: 'accessing'
set compile_env: 0
method: CypressModification
obsoletion

	^obsoletion
%
category: 'applying'
set compile_env: 0
method: CypressModification
applyTo: aCypressLoader

	aCypressLoader applyModification: self
%
category: 'dependency'
set compile_env: 0
method: CypressModification
provisions
	"Answer list of global names defined by this definition"

	^self modification provisions
%
category: 'dependency'
set compile_env: 0
method: CypressModification
requirements
	"Answer list of global names required by this definition"

	^self modification requirements
%
category: 'initialization'
set compile_env: 0
method: CypressModification
= aPatchOperation
	^(super = aPatchOperation) and: [self obsoletion = aPatchOperation obsoletion and: [ self modification = aPatchOperation modification]]
%
category: 'initialization'
set compile_env: 0
method: CypressModification
base: base target: target

	obsoletion := base.
	modification := target.
%
category: 'loading'
set compile_env: 0
method: CypressModification
loadClassDefinition

	self modification loadClassDefinition
%
category: 'loading'
set compile_env: 0
method: CypressModification
loadMethodDefinition
	self modification loadMethodDefinition
%
category: 'loading'
set compile_env: 0
method: CypressModification
postLoadDefinition
	self modification postLoadOver: self obsoletion
%
category: 'printing'
set compile_env: 0
method: CypressModification
printString

	| str |
	str := WriteStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self description;
		nextPutAll: ')'.
	^str contents
%

! Remove existing behavior from CypressRemoval
doit
CypressRemoval removeAllMethods.
CypressRemoval class removeAllMethods.
%
! ------------------- Class methods for CypressRemoval
category: 'instance creation'
set compile_env: 0
classmethod: CypressRemoval
of: aDefinition
	^ self new definition: aDefinition
%
! ------------------- Instance methods for CypressRemoval
category: 'accessing'
set compile_env: 0
method: CypressRemoval
definition

	^definition
%
category: 'accessing'
set compile_env: 0
method: CypressRemoval
description

	^'remove: ', self definition printString
%
category: 'applying'
set compile_env: 0
method: CypressRemoval
applyTo: aCypressLoader

	aCypressLoader applyRemoval: self
%
category: 'comparing'
set compile_env: 0
method: CypressRemoval
= aPatchOperation
	^(super = aPatchOperation) and: [self definition = aPatchOperation definition]
%
category: 'dependency'
set compile_env: 0
method: CypressRemoval
provisions
	"Answer list of global names defined by this definition"

	^#()
%
category: 'dependency'
set compile_env: 0
method: CypressRemoval
requirements
	"Answer list of global names required by this definition"

	^#()
%
category: 'initialization'
set compile_env: 0
method: CypressRemoval
definition: aDefinition

	definition := aDefinition
%
category: 'loading'
set compile_env: 0
method: CypressRemoval
loadClassDefinition
	
	self error: 'inappropriate to send #loadClassDefinition to a removal operation'
%
category: 'loading'
set compile_env: 0
method: CypressRemoval
loadMethodDefinition
	
	self error: 'inappropriate to send #loadMethodDefinition to a removal operation'
%
category: 'loading'
set compile_env: 0
method: CypressRemoval
postLoadDefinition
	
	self error: 'inappropriate to send #postLoadDefinition to a removal operation'
%
category: 'loading'
set compile_env: 0
method: CypressRemoval
unloadDefinition

	self definition unloadDefinition
%
category: 'printing'
set compile_env: 0
method: CypressRemoval
printString

	| str |
	str := WriteStream on: String new.
	str 
		nextPutAll: super printString;
		nextPutAll: ' (';
		nextPutAll: self description;
		nextPutAll: ')'.
	^str contents
%

! Remove existing behavior from CypressSnapshot
doit
CypressSnapshot removeAllMethods.
CypressSnapshot class removeAllMethods.
%
! ------------------- Class methods for CypressSnapshot
category: 'instance creation'
set compile_env: 0
classmethod: CypressSnapshot
definitions: aDefinitions

	^(self new) definitions: aDefinitions
%
! ------------------- Instance methods for CypressSnapshot
category: 'accessing'
set compile_env: 0
method: CypressSnapshot
definitions

	^definitions
%
category: 'accessing'
set compile_env: 0
method: CypressSnapshot
definitions: aDefinitions

	definitions := aDefinitions
%
category: 'comparing'
set compile_env: 0
method: CypressSnapshot
= other
	^ definitions asArray = other definitions asArray
%
category: 'enumerating'
set compile_env: 0
method: CypressSnapshot
classDefinitions: classBlock methodDefinitions: methodBlock

	self definitions do: [:definition |
		definition classDefinition: classBlock methodDefinition: methodBlock]
%
category: 'loading'
set compile_env: 0
method: CypressSnapshot
updatePackage: aPackage
	CypressLoader updatePackage: aPackage withSnapshot: self
%
category: 'patching'
set compile_env: 0
method: CypressSnapshot
patchRelativeToBase: aSnapshot
	^ CypressPatch fromBase: aSnapshot toTarget: self
%
