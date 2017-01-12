"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
packageNamed: aName

	| package |
	package := self existingPackageNamed: aName.
	package == AbsentToken ifTrue: [package := self newNamed: aName].
	^package