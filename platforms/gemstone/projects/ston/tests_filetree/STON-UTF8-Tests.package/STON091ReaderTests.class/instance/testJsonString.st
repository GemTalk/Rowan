tests
testJsonString
	"Allow double quotes for backwards JSON compatibility"

	| string |
	self assert: (self materialize: '"foo"') = 'foo'.
	self assert: (self materialize: '"FOO"') = 'FOO'.
	self assert: (self materialize: '"\u00C3\u00A9l\u00C3\u00A8ve en Fran\u00C3\u00A7ais"') = 'élève en Français'.
	string := String
		withAll:
			{$".
			$'.
			$\.
			(Character tab).
			(Character cr).
			(Character lf).
			(Character newPage).
			(Character backspace)}.
	self assert: (self materialize: '"\"\''\\\t\r\n\f\b"') = string