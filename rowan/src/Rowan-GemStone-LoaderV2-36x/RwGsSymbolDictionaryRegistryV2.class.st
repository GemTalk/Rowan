Class {
	#name : 'RwGsSymbolDictionaryRegistryV2',
	#superclass : 'Object',
	#instVars : [
		'symbolDictionary',
		'packageRegistry'
	],
	#category : 'Rowan-GemStone-LoaderV2-36x'
}

{ #category : 'package adopt' }
RwGsSymbolDictionaryRegistryV2 >> adoptCompiledMethod: compiledMethod classExtension: classExtension for: behavior protocol: protocolString toPackageNamed: packageName [
	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassOrExtension |
	classExtension
		ifTrue: [ 
			"to adopt and extension method, it is much less complicated"
			^ self
				addExtensionCompiledMethod: compiledMethod
				for: behavior
				protocol: protocolString
				toPackageNamed: packageName ].

	methodDictionary := (behavior persistentMethodDictForEnv: 0)
		ifNil: [ Dictionary new ].
	selector := compiledMethod selector.
	compiledMethod
		==
			(methodDictionary
				at: selector
				ifAbsent: [ self error: 'expected an existing compiled method' ])
		ifFalse: [ 
			self
				error:
					'The given compiled method does not the existing compiled method in the class.' ].

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior _moveMethod: selector toCategory: protocolSymbol.

	existing := compiledMethod _owner.
	existing
		ifNotNil: [ :lm | 
			"existing LoadedMethod found for compiled method ... ignore already packaged methods"
			lm isRowanLoadedMethod
				ifFalse: [ self error: 'Expected a loaded method for  ' , compiledMethod printString ].
			^ self ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.
	compiledMethod _owner: loadedMethod.

	loadedPackage := self
		loadedPackageNamed: packageName
		ifAbsent: [ 
			self
				error: 'Internal error -- attempt to add a method to a nonexistent package.' ].

	loadedClassOrExtension := loadedPackage
		loadedClassOrClassExtensionForClass: behavior
		ifAbsent: [ 
			self
				error:
					'Internal error -- attempt to add a method to a package in which its class is neither defined nor extended.' ].
	loadedClassOrExtension addLoadedMethod: loadedMethod
]

{ #category : 'initialization' }
RwGsSymbolDictionaryRegistryV2 >> initialize [

	| symbolDictObjectSecurityPolicy |
	packageRegistry := KeyValueDictionary new.	"keyed by package name"

	symbolDictObjectSecurityPolicy := symbolDictionary objectSecurityPolicy.
	packageRegistry objectSecurityPolicy: symbolDictObjectSecurityPolicy.
]
