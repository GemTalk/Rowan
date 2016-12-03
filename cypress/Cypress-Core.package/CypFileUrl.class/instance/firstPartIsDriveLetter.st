"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
firstPartIsDriveLetter
	"Return true if the first part of the path is a letter
	followed by a $: like 'C:' "

	| firstPart |
	path isEmpty ifTrue: [^false].
	firstPart := path first.
	^firstPart size = 2
		and: [firstPart first isLetter and: [firstPart last = $:]]