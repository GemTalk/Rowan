"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
fromString: aString
	"trim surrounding whitespace"

	| remainder index scheme fragment newUrl |
	remainder := aString trimSeparators.

	"extract the fragment, if any"
	index := remainder indexOf: $#.
	index > 0
		ifTrue: 
			[fragment := remainder copyFrom: index + 1 to: remainder size.
			remainder := remainder copyFrom: 1 to: index - 1].

	"choose class based on the scheme name, and let that class do the bulk of the parsing"
	scheme := self schemeNameForString: remainder.
	newUrl := (self urlClassForScheme: scheme) new
				privateInitializeFromText: remainder.
	newUrl privateFragment: fragment.
	^newUrl