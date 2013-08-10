testing
testPackageAuthorAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements-dhk.1'
			parsesToPackageName: 'Announcements'
			author: 'dhk'
			branch: ''
			version: 1
