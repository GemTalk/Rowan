tests-issue 217
testIssue217

	"https://github.com/dalehenrich/Rowan/issues/217"

	| spec specExportString |
	[spec := (Rowan image _projectForNonTestProject: 'Rowan') specification asSpecification ] 
		on: Deprecated 
		do: [:ex | 
			"ignore any Deprecation errors ... we're specification testing the speciciation"
			ex resume ].
	specExportString := STON toStringPretty: spec.
	self assert: (specExportString includes: Character lf).
