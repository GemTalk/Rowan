"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
symbolDictsForClass: aClass
	"Answer a sequenceable collection of SymbolDictionaries, among those in the current session's transient
	symbol list, wherein the given class is present by its own name."

	| className |
	className := aClass name.
	^GsCurrentSession currentSession symbolList
		select: [:dict | (dict at: className ifAbsent: [nil]) == aClass]