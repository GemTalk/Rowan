testing
testEmptyString
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: ''
			parsesToPackageName: ''
			author: ''
			branch: ''
			version: 0
