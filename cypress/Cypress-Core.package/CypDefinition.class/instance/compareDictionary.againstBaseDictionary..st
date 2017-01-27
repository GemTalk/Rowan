"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
compareDictionary: myDictionary againstBaseDictionary: baseDictionary

	| keys modification |
	modification := CypModificationSet new.
	keys := myDictionary keys copy addAll: baseDictionary keys.
	keys do: 
			[:key |
			| before after |
			before := baseDictionary at: key ifAbsent: [nil].
			after := myDictionary at: key ifAbsent: [nil].
			before = nil
				ifTrue: [modification add: (CypAddition of: after)]
				ifFalse: 
					[after = nil
						ifTrue: [modification add: (CypDeletion of: before)]
						ifFalse: 
							[| mod |
							mod := after compareAgainstBase: before.
							mod isEmpty ifFalse: [modification add: mod]]]].
	^modification