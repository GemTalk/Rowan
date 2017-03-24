"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - applying
"
removeDeletedClassesFromTempSymbols
	"Deleted class names should not resolve during compilation."

	deletedClasses do: [:each | tempSymbols removeKey: each name]