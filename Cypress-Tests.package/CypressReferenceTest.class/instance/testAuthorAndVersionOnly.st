testing
testAuthorAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '-dhk.1'
			parsesToPackageName: ''
			author: 'dhk'
			branch: ''
			version: 1
