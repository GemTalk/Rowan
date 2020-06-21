Extension { #name : 'Behavior' }

{ #category : '*rowan-gemstone-kernel-36x' }
Behavior >> _rwGciCompileMethod: sourceString dictionaries: symbolList category: aCategoryString environmentId: envId [
	"used by GCI implementation , when session methods are enabled.
   Returns nil for successful compilation, a warning String,
   or signals a CompileError"

	"uses Rowan API to ensure that the method is properly packaged by Rowan"

	| warnStr |
	[ 
	Rowan gemstoneTools topaz
		addOrUpdateMethod: sourceString
		inProtocol: aCategoryString asString
		forBehavior: self
		symbolList: symbolList
		environmentId: envId ]
		onException: CompileWarning
		do: [ :ex | 
			"handle CompileWarning"
			warnStr := ex warningString.
			ex resume ].
	^ warnStr
]

{ #category : '*rowan-gemstone-kernel-36x' }
Behavior >> rwMethodCategories [
	^ self _unifiedCategorys: 0
]
