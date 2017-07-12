"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
isPropertiesFileDirectoryEntry: entry

	^self propertiesFileNameExtensions
		anySatisfy: [:each | self string: entry endsWith: '/properties' , each]