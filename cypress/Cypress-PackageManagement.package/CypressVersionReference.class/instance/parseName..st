"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
parseName: aString

	| basicName lastDotIndex packageDotIndex lastMinusIndex |
	basicName := (aString isEmpty
				or: [aString last isDigit or: [(aString includes: $() not]])
					ifTrue: [aString]
					ifFalse: 
						["up to last (, but not if there's a . after it"
						| parenIndex dotIndex |
						parenIndex := 0.
						dotIndex := 0.
						aString size to: 1
							by: -1
							do: 
								[:i |
								| c |
								c := aString at: i.
								(c = $. and: [dotIndex = 0])
									ifTrue: 
										[dotIndex := i.
										parenIndex := 0].
								(c = $( and: [parenIndex = 0]) ifTrue: [parenIndex := i]].
						aString copyFrom: 1 to: parenIndex - 1].
	lastMinusIndex := 0.
	lastDotIndex := 0.
	basicName size to: 1
		by: -1
		do: 
			[:i |
			| c |
			c := basicName at: i.
			(c = $- and: [lastMinusIndex = 0]) ifTrue: [lastMinusIndex := i].
			(c = $. and: [lastDotIndex = 0]) ifTrue: [lastDotIndex := i]].
	lastMinusIndex = 0 ifTrue: [lastMinusIndex := basicName size + 1].
	package := basicName copyFrom: 1 to: lastMinusIndex - 1.
	branch := ''.
	packageDotIndex := package indexOf: $..
	packageDotIndex > 0
		ifTrue: 
			[branch := package copyFrom: packageDotIndex + 1 to: package size.
			package := package copyFrom: 1 to: packageDotIndex - 1].
	author := lastMinusIndex = 0
				ifTrue: ['']
				ifFalse: 
					[lastDotIndex < lastMinusIndex ifTrue: [lastDotIndex := basicName size + 1].
					basicName copyFrom: lastMinusIndex + 1 to: lastDotIndex - 1].
	versionNumber := (basicName copyAfterLast: $-) copyAfterLast: $..
	(versionNumber notEmpty
		and: [versionNumber allSatisfy: [:each | each isDigit]])
			ifTrue: [versionNumber := versionNumber asInteger]
			ifFalse: [versionNumber := 0]