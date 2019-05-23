smalltalk api
gitBranchNameIn: gitRepoPath

	"return current branch for git repository located at gitPath"

	| command result |
	command := 'set -e; cd ' , gitRepoPath , ';git branch | sed -n ''/\* /s///p'''.
	result := self performOnServer: command logging: false.
	^ result trimWhiteSpace