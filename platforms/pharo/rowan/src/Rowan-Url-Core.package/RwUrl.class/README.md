A Uniform Resource Locator.  It specifies the location of a document on the Internet.  The base class is abstract; child classes break different types of URLs down in ways appropriate for that type.

The classes in this hierarchy:

RwUrl
 RwFileUrl
  RwCypressUrl
  RwFiletreeUrl
   RwTonelUrl
  RwGithubUrl
 RwGenericUrl
 RwHierarchicalUrl
  RwSmalltalkRepositoryUrl
   RwGitFileTreeUrl

are specificaly designed for parsing URLS for Rowan source code repositories:

 file:/opt/git/shared/repos/rowan/sample/repository
 http://gsdevkit.github.io/GsDevKit_home/rowan/Sample.ston
 https://raw.githubusercontent.com/dalehenrich/sample/master/specs/Sample.ston

 github://GsDevKit/GsDevKit:master/repository
 filetree:///opt/git/shared/repos/rowan/sample/repository
 tonel:/opt/git/shared/repos/rowan/sample/repository
 gitfiletree://gitlab.com/GsDevKit/GsDevKit:master/repository

 cypress:/opt/git/shared/repos/rowan/sample/repository/
 smalltalk://dkh:pass@gitlab.ferlicot.fr:3456/Projet/Bazard:dev/src
 smalltalk://git@git.gemtalksystems.com/btree340:dev/repository
 smalltalk://git@github.com/GsDevKit/GsDevKit:350/repository
 smalltalk://github.com/GsDevKit/GsDevKit:350/repository


The file:, http: and https: schemes should conform to the standard specs. ZnUrl is used for parsing http: and https: urls.

The github:, filetree:, gitfiletree: and tonel: schemes are supported for backward compatibility with schemes that have historically been used to identify Metacello repository urls.

The cypress: and smalltalk: schemes are new and intended to be used moving forward with Metacello and Rowan.

The cypress: url is used to indicate that path to a Cypress-style repository, i.e., a disk-based format for storing Smalltalk packages in filetree or tonel format. A cypress: url does not define the specific repository type (filetree or tonel). The type of the repository is encoded in a .filetree file located in the directory specified by pathString of the url.

The smalltalk: scheme is based on Thierry Goubier's gitfiletree url[1]. The original github: scheme cannot be used for arbitrary git servers or git servers using non-standard ports or requiring usernames and passwords. Thierry's scheme handles the full range of possibilites.

[1] https://github.com/dalehenrich/filetree/blob/734eed46ea57ebf5e24e5d935768bd49727fc22f/repository/MonticelloFileTree-Git.package/MCFileTreeGitRepository.class/class/basicFromUrl..st