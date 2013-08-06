private
splitMethodName: methodName

	| extension |
	extension := #('.json' '.st')
		detect: [:each | methodName endsWith: each] 
		ifNone: [CypressError signal: 'invalid structure element: ', methodName].
	^Array
		with: (methodName copyWithoutSuffix: extension)
		with: extension.
