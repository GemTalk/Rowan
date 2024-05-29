Class {
	#name : 'RwTraitAuditTool',
	#superclass : 'RwAbstractTool',
	#instVars : [
		'theAuditDetails'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'audit' }
RwTraitAuditTool >> auditGlobalFor: aLoadedTrait [
	"answer true if there are no audit issues with the trait"

	(Rowan globalNamed: aLoadedTrait name)
		ifNil: [ 
			"there is no matching Class for LoadedClass"
			self theAuditDetails
				add:
					(RwAuditTraitDetail
						for: aLoadedTrait
						reason: #'missingGemStoneTraitForLoadedTrait'
						message: 'Missing gemstone trait for loaded trait: ' , aLoadedTrait name).
			^ false ]
		ifNotNil: [ :theTrait | 
			theTrait == aLoadedTrait handle
				ifFalse: [ 
					"the loadedTrait is for a different trait installed in symbol dictionaries"
					self theAuditDetails
						add:
							(RwAuditTraitDetail
								for: aLoadedTrait
								reason: #'traitsNotIdentical'
								message:
									'Installed trait is not identical to the loaded trait ' , aLoadedTrait name).
					^ false ] ].
	^ true
]

{ #category : 'audit' }
RwTraitAuditTool >> auditLoadedTrait: aLoadedTrait [
	| aTrait |
	(self auditGlobalFor: aLoadedTrait)
		ifTrue: [ 
			"audit trait details, since there are no issues with the trait itself"
			aTrait := aLoadedTrait handle.
			self auditLoadedTraitProperties: aLoadedTrait forTrait: aTrait.
			self auditTraitMethods: aLoadedTrait forTrait: aTrait ].
	^ self theAuditDetails
]

{ #category : 'audit' }
RwTraitAuditTool >> auditLoadedTraitProperties: aLoadedTrait forTrait: aTrait [
	"Check #( 'instvars', 'superclass', 'classinstvars',  'gs_SymbolDictionary', 'comment', 'classvars', 'pools', 'category')"

	| aDict traitProperty varNames |
	aLoadedTrait instVarNames
		= (varNames := aTrait instVarNames collect: [ :e | e asString ])
		ifFalse: [ 
			self theAuditDetails
				add:
					((RwAuditTraitPropertyDetail
						for: aLoadedTrait
						message:
							'For trait ' , aLoadedTrait name , ' instVarNames changed in Trait ('
								, varNames printString , ') v loaded Trait ('
								, aLoadedTrait instVarNames printString , ')')
						reason: #'differentInstVars';
						loadedPropertyValue: aLoadedTrait iInstVarNames;
						traitPropertyValue: aTrait instVarNames;
						trait: aTrait;
						yourself) ].
	aLoadedTrait classInstVarNames
		= (varNames := aTrait classInstVarNames collect: [ :e | e asString ])
		ifFalse: [ 
			self theAuditDetails
				add:
					((RwAuditTraitPropertyDetail
						for: aLoadedTrait
						message:
							'For trait ' , aLoadedTrait name , ' classInstVarNames changed in Trait ('
								, varNames printString , ') v loaded Trait ('
								, aLoadedTrait classInstVarNames printString , ')')
						reason: #'differentClassInstVars';
						loadedPropertyValue: aLoadedTrait classInstVarNames;
						classPropertyValue: aTrait instVarNames;
						trait: aTrait;
						yourself) ].
	aLoadedTrait classVarNames
		=
			(traitProperty := ((aTrait classVarNames ifNil: [ #() ])
				collect: [ :e | e asString ]) asSortedCollection asArray)
		ifFalse: [ 
			self theAuditDetails
				add:
					((RwAuditTraitPropertyDetail
						for: aLoadedTrait
						message:
							'For trait ' , aLoadedTrait name , ' classVars changed in compiled class ('
								, traitProperty printString , ') v loaded trait ('
								, aLoadedTrait classVarNames printString , ')')
						reason: #'differentClassVars';
						loadedPropertyValue: aLoadedTrait classVarNames;
						traitPropertyValue: traitProperty;
						trait: aTrait;
						yourself) ].
	(aDict := System myUserProfile
		resolveSymbol: aLoadedTrait classSymbolDictionaryName asSymbol)
		ifNil: [ 
			self theAuditDetails
				add:
					((RwAuditClassPropertyDetail
						for: aLoadedTrait
						message:
							'For class ' , aLoadedTrait name , ' unable to find SymbolDictionary '
								, aLoadedTrait classSymbolDictionaryName)
						reason: #'missingSymbolDictionary';
						loadedPropertyValue: aLoadedTrait classSymbolDictionaryName;
						trait: aTrait;
						yourself) ]
		ifNotNil: [ :smbd | 
			smbd value
				at: aLoadedTrait name asSymbol
				ifAbsent: [ 
					self theAuditDetails
						add:
							((RwAuditTraitPropertyDetail
								for: aLoadedTrait
								message:
									'Trait (' , aLoadedTrait name , ') not found in symbol dictionary ('
										, aLoadedTrait classSymbolDictionaryName asSymbol
										, ') of loaded trait')
								reason: #'missingTraitInSymbolDictionary';
								loadedPropertyValue: aLoadedTrait classSymbolDictionaryName;
								classPropertyValue:
										(GsCurrentSession currentSession symbolList dictionariesAndSymbolsOf: aTrait)
												yourself) ] ]
]

{ #category : 'audit' }
RwTraitAuditTool >> auditTraitMethods: aLoadedTrait forTrait: aTrait [
	"every method is a trait should have a loaded method associated with it"

	aTrait localSelectors
		do: [ :aSelector | 
			aLoadedTrait loadedInstanceMethods
				at: aSelector
				ifAbsent: [ 
					| notification |
					notification := (RwAuditTraitMethodErrorNotification
						method: aSelector
						isMeta: false
						inTraitNamed: aTrait name
						intoPackageNamed: aLoadedTrait loadedPackage name)
						description: 'Missing loaded trait method';
						reason: #'missingLoadedTraitMethod';
						yourself.
					notification signal
						ifTrue: [ 
							"record audit detail"
							self theAuditDetails
								add:
									((RwAuditMethodDetail
										for: aLoadedTrait
										message:
											'Missing loaded trait method: ' , aLoadedTrait printString , '>>' , aSelector)
										reason: #'missingLoadedtraitMethod';
										loadedMethod: nil;
										method: nil;
										selector: aSelector;
										behavior: aTrait;
										yourself) ] ] ].
	aTrait classTrait localSelectors
		do: [ :aSelector | 
			aLoadedTrait loadedClassMethods
				at: aSelector
				ifAbsent: [ 
					| notification |
					notification := (RwAuditTraitMethodErrorNotification
						method: aSelector
						isMeta: true
						inTraitNamed: aTrait name
						intoPackageNamed: aLoadedTrait loadedPackage name)
						description: 'Missing loaded trait method';
						reason: #'missingLoadedTraitMethod';
						yourself.
					notification signal
						ifTrue: [ 
							"record audit detail"
							self theAuditDetails
								add:
									((RwAuditMethodDetail
										for: aLoadedTrait
										message:
											'Missing loaded trait method: ' , aLoadedTrait printString , ' classTrait >>'
												, aSelector)
										reason: #'missingLoadedtraitMethod';
										loadedMethod: nil;
										method: nil;
										selector: aSelector;
										behavior: aTrait;
										yourself) ] ] ]
]

{ #category : 'accessing' }
RwTraitAuditTool >> theAuditDetails [
	^ theAuditDetails ifNil: [ theAuditDetails := Array new ]
]

{ #category : 'accessing' }
RwTraitAuditTool >> theAuditDetails: object [
	theAuditDetails := object
]
