testing
testBranchAndAuthorOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.v3-dhk'
			parsesToPackageName: ''
			author: 'dhk'
			branch: 'v3'
			version: 0
