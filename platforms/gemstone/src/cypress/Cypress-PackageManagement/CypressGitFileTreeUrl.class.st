"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressGitFileTreeUrl',
	#superclass : 'CypressGitFileUrl',
	#category : 'Cypress-PackageManagement'
}

{ #category : 'constants' }
CypressGitFileTreeUrl class >> schemeName [
  "A gitfiletree url with a host is the target for a remote. All other parameters are optional.
	Parameters are:
		dir : the directory inside the repository where the target MC packages are.
		branch : the git branch to fetch.
		protocol: the user name part to add to the ssh Url, default to git, but can also be https (which implies read only access).
		readOnly : is the repository read only? If present, reduce the history to a minimum (and change the GUI).
	Alternative url syntax:
		gitfiletree://github.com/dalehenrich/filetree:pharo5.0_dev/repository
	with:
		host : github.com
		project : dalehenrich/filetree
		branch : pharo5.0_dev
		dir : repository
"

  ^ 'gitfiletree'

]

{ #category : 'accessing' }
CypressGitFileTreeUrl >> codeFormat [

	^'FileTree'

]

{ #category : 'testing' }
CypressGitFileTreeUrl >> isStrict [

	^false

]
