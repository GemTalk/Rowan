"
This class implements the gitfiletree: scheme:

 gitfiletree://gitlab.com/GsDevKit/GsDevKit:master/repository

The gitfiletree scheme [1] encodes the following information:

  gitfiletree:// [ user [ : password ] @ ] host [ : port ] / user / project [ : branch ] [ / repositoryPath ]

From the method comment[1]:

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

[1] https://github.com/dalehenrich/filetree/blob/734eed46ea57ebf5e24e5d935768bd49727fc22f/repository/MonticelloFileTree-Git.package/MCFileTreeGitRepository.class/class/basicFromUrl..st
"
Class {
	#name : 'RwGitFileTreeUrl',
	#superclass : 'RwMetacelloRepositoryUrl',
	#category : 'Rowan-Url'
}

{ #category : 'constants' }
RwGitFileTreeUrl class >> schemeName [
  ^ 'gitfiletree'
]
