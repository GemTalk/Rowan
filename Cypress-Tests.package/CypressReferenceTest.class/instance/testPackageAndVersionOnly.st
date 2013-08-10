testing
testPackageAndVersionOnly
	"Syntax: packageName[.branch][-author][.version]"

	self
		assert: 'Announcements..1'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: '.1'
			version: 0;
		assert: 'Announcements.-.1'
			parsesToPackageName: 'Announcements'
			author: ''
			branch: ''
			version: 1
