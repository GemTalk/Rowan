"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
compareDictionary: myDictionary againstBaseDictionary: baseDictionary into: anElementsModification

	| keys |
	keys := myDictionary keys copy.
	keys addAll: baseDictionary keys.
	keys do: 
			[:key |
			| before after |
			before := baseDictionary at: key ifAbsent: [nil].
			after := myDictionary at: key ifAbsent: [nil].
			before = nil
				ifTrue: [anElementsModification elementAdded: after]
				ifFalse: 
					[after = nil
						ifTrue: [anElementsModification elementRemoved: before]
						ifFalse: 
							[| modification |
							modification := after compareAgainstBase: before.
							modification isEmpty
								ifFalse: [anElementsModification addElementModification: modification]]]]