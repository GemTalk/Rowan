This class implements the smalltalk: scheme:

 smalltalk://dkh:pass@gitlab.ferlicot.fr:3456/Projet/Bazard:dev/src
 smalltalk://git@git.gemtalksystems.com/btree340:dev/repository
 smalltalk://git@github.com/GsDevKit/GsDevKit:350/repository
 smalltalk://github.com/GsDevKit/GsDevKit:350/repository

The smalltalk: scheme is based on Thierry Goubier's gitfiletree url[1]:

  smalltalk:// <[user[:password]@]host[:port]> / user/ projectName [ : versionIdentifier ] [ / repositoryPath ]

[1] https://github.com/dalehenrich/filetree/blob/734eed46ea57ebf5e24e5d935768bd49727fc22f/repository/MonticelloFileTree-Git.package/MCFileTreeGitRepository.class/class/basicFromUrl..st