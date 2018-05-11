tests
testDoubleQuotedString
	| string |
	self assert: (self serializeJson: 'foo') = '"foo"'.
	self assert: (self serializeJson: 'FOO') = '"FOO"'.
	self assert: (self serializeJson: 'élève en Français') = '"\u00C3\u00A9l\u00C3\u00A8ve en Fran\u00C3\u00A7ais"'.
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
	self assert: (self serializeJson: string) = '"\"\''\\\t\r\n\f\b"'