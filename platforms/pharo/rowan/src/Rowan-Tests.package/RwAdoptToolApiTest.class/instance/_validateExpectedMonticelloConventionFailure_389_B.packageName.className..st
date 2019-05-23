private
_validateExpectedMonticelloConventionFailure_389_B: audit packageName: packageName className: className

	"validate expected audit failures - according to Monticello conventions"

	self _validateExpectedMonticelloConventionFailure_389: audit packageName: packageName className: className.
	audit removeKey: packageName.
	self assert: audit isEmpty