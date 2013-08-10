testing
testBranchAlone
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '.v3'
			parsesToPackageName: ''
			author: ''
			branch: 'v3'
			version: 0
