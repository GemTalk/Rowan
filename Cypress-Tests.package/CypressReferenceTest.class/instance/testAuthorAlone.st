testing
testAuthorAlone
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: '-dhk'
			parsesToPackageName: ''
			author: 'dhk'
			branch: ''
			version: 0
