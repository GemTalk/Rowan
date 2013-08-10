testing
testBranchAuthorAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.v3-dhk.1'
			parsesToPackageName: ''
			author: 'dhk'
			branch: 'v3'
			version: 1
