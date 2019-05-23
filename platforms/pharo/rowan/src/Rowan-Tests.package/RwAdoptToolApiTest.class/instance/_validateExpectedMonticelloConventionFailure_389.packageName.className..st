private
_validateExpectedMonticelloConventionFailure_389: audit packageName: packageName className: className

	"validate expected audit failures - according to Monticello conventions"

	| failures unexpectedFailures |
	failures := audit at: packageName.
	self assert: failures size = 1.
	failures := failures at: className.
	self assert: failures size = 2.

	"extension categories not named according to Monticello conventions (no leading $*)"
	unexpectedFailures := failures reject: [:each | (each value = 'Missing instance method extension category ') or: [ each value = 'Missing class method extension category ' ] ].
	self assert: unexpectedFailures isEmpty