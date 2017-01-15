"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateForProtocolChangeTo: newProtocol
	"My protocol has changed. This might also mean that my package has changed."

	| currentPackageName newPackageName |
	self propertyAt: 'protocol' put: newProtocol.
	currentPackageName := loadedClass ifNotNil: [loadedClass packageName].
	newProtocol first = $*
		ifTrue: [newPackageName := newProtocol copyFrom: 2 to: newProtocol size]
		ifFalse: [newPackageName := self definingPackageOfClass name].
	currentPackageName = newPackageName
		ifFalse: [self moveToPackageNamed: newPackageName]