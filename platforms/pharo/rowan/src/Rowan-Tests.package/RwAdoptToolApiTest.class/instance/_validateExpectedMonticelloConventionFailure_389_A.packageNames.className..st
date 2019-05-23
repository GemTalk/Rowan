private
_validateExpectedMonticelloConventionFailure_389_A: audit packageNames: packageNames className: className

	"validate expected audit failures - according to Monticello conventions"

	| unexpectedFailures |
	self _validateExpectedMonticelloConventionFailure_389: audit packageName: (packageNames at: 2) className: className.
	audit removeKey: (packageNames at: 2).

	"class category not following Monticello conventions"
	unexpectedFailures := ((audit at: (packageNames at: 1)) at: className)
		reject: [:each | (each value = 'Class category has changed in compiled class v loaded class')
			or: [ (each value = 'Missing instance method extension category ') or: [ (each value = 'Missing class method extension category ')
			or: [ (each value = 'Missing loaded instance method. ') or: [ (each value = 'Missing loaded class method. ') ]] ] ] ].
	self assert: unexpectedFailures isEmpty