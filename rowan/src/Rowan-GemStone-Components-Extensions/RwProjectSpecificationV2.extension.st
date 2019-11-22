Extension { #name : 'RwProjectSpecificationV2' }

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> _gemstoneAllUsersName [

	^ 'allusers'
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> _gemstoneDefaultMethodEnv [
	^ 0
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> _gemstoneDefaultSymbolDictName [

	^ 'UserGlobals'
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> _gemstoneDefaultUseSessionMethodsForExtensions [

	^ false
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> gemstoneDefaultMethodEnvForUser: userId [
	| gemstoneProperties userProperties |
	gemstoneProperties := self platformProperties
		at: 'gemstone'
		ifAbsent: [ ^ self _gemstoneDefaultMethodEnv ].
	userProperties := gemstoneProperties
		at: userId
		ifAbsent: [ 
			gemstoneProperties
				at: self _gemstoneAllUsersName
				ifAbsent: [ ^ self _gemstoneDefaultMethodEnv ] ].
	^ userProperties
		at: #'defaultMethodEnv'
		ifAbsent: [ self _gemstoneDefaultMethodEnv ]
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> gemstoneDefaultSymbolDictNameForUser: userId [
	| gemstoneProperties userProperties |
	gemstoneProperties := self platformProperties
		at: 'gemstone'
		ifAbsent: [ ^ self _gemstoneDefaultSymbolDictName ].
	userProperties := gemstoneProperties
		at: userId
		ifAbsent: [ 
			gemstoneProperties
				at: self _gemstoneAllUsersName
				ifAbsent: [ ^ self _gemstoneDefaultSymbolDictName ] ].
	^ userProperties
		at: #'defaultSymbolDictName'
		ifAbsent: [ self _gemstoneDefaultSymbolDictName ]
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> gemstoneDefaultUseSessionMethodsForExtensionsForUser: userId [
	| gemstoneProperties userProperties |
	gemstoneProperties := self platformProperties
		at: 'gemstone'
		ifAbsent: [ ^ self _gemstoneDefaultUseSessionMethodsForExtensions ].
	userProperties := gemstoneProperties
		at: userId
		ifAbsent: [ 
			gemstoneProperties
				at: self _gemstoneAllUsersName
				ifAbsent: [ ^ self _gemstoneDefaultUseSessionMethodsForExtensions ] ].
	^ userProperties
		at: #'defaultUseSessionMethodsForExtensions'
		ifAbsent: [ self _gemstoneDefaultUseSessionMethodsForExtensions ]
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> gemstoneSetDefaultMethodEnvForUser: userId to: env [

	((self platformProperties at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #defaultMethodEnv put: env
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> gemstoneSetDefaultMethodEnvTo: env [
	self gemstoneSetDefaultMethodEnvForUser: self _gemstoneAllUsersName to: env
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> gemstoneSetDefaultSymbolDictNameForUser: userId to: symbolDictName [

	((self platformProperties at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #defaultSymbolDictName put: symbolDictName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> gemstoneSetDefaultSymbolDictNameTo: symbolDictName [
	self gemstoneSetDefaultSymbolDictNameForUser: self _gemstoneAllUsersName to: symbolDictName
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: userId to: aBool [

	((self platformProperties at: 'gemstone' ifAbsentPut: [ Dictionary new ])
		at: userId ifAbsentPut: [ Dictionary new ])
			at: #defaultUseSessionMethodsForExtensions put: aBool
]

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectSpecificationV2 >> gemstoneSetDefaultUseSessionMethodsForExtensionsTo: aBool [
	self gemstoneSetDefaultUseSessionMethodsForExtensionsForUser: self _gemstoneAllUsersName to: aBool
]
