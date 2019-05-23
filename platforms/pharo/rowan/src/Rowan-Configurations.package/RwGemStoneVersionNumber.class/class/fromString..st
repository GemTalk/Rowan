instance creation
fromString: aString

	| new components |
	components := OrderedCollection new.
	(aString substrings: '.') do: [:subString | 
		components add: subString asInteger].
	new := self new: components size.
	1 to: components size do: [:i | new at: i put: (components at: i) ].
	^new
