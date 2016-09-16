"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating - type
"
beQualifiedNameOf: somePackageNames
	"Be designated as qualifying a Known Package name and therefore not eligible as a package name."

	type := 'Qualified Name'.
	competingPackageNames := somePackageNames sortAscending.
	advice := 'Qualifies the package named ', self competingPackageNamesString
