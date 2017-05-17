"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
classOrExtensionForClassNamed: className ifAbsent: absentBlock

	^loadedClasses at: className
		ifAbsent: [loadedClassExtensions at: className ifAbsent: absentBlock]