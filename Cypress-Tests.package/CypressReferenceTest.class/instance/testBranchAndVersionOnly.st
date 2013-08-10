testing
testBranchAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.v3.1'
			parsesToPackageName: ''
			author: ''
			branch: 'v3.1'
			version: 0;
		assert: '.v3-.1'
			parsesToPackageName: ''
			author: ''
			branch: 'v3'
			version: 1
