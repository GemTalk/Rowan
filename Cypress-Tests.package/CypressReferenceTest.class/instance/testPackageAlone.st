testing
testPackageAlone
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: ''
			version: 0;
		assert: 'Announcements.-.'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: ''
			version: 0
