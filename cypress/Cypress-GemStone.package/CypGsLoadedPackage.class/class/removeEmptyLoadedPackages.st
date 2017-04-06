"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
removeEmptyLoadedPackages
	"Examine all LoadedPackages. Remove any that have no classes or class extensions."

	PackageRegistry copy
		keysAndValuesDo: [:key :value | value isEmpty ifTrue: [PackageRegistry removeKey: key]]