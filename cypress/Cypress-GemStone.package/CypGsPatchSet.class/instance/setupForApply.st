"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
setupForApply
	"Copy the entire namespace from the user's transient SymbolList into a temporary SymbolDictionary in a temporary SymbolList, the tempSymbols."

	| symbolList |
	tempSymbols := SymbolDictionary new.
	symbolList := GsCurrentSession currentSession symbolList.
	"If there are any duplicated symbols, we only want the one that will be found first one, 
	so we copy in reverse order of search order."
	symbolList reverseDo: 
			[:dict |
			dict keysAndValuesDo: [:aKey :aValue | tempSymbols at: aKey put: aValue]]