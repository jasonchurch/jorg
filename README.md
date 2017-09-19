<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#org94e7c62">1. JOrg</a>
<ul>
<li><a href="#org60cef3a">1.1. Repo Structure</a></li>
<li><a href="#orgcd417bd">1.2. Installation</a>
<ul>
<li><a href="#org172a709">1.2.1. Get JOrg</a></li>
<li><a href="#org172afe6">1.2.2. Make Emacs aware of JOrg</a></li>
<li><a href="#org36d3053">1.2.3. Customizations</a></li>
</ul>
</li>
<li><a href="#orgc96308d">1.3. Usage</a>
<ul>
<li><a href="#org9ebc471">1.3.1. Jorg Capture</a></li>
<li><a href="#orgdd5d6f1">1.3.2. Common User/Interactive Functions</a></li>
</ul>
</li>
<li><a href="#org5a12637">1.4. What is a JOrg Project File?</a></li>
</ul>
</li>
</ul>
</div>
</div>

<a id="org94e7c62"></a>

# JOrg


<a id="org60cef3a"></a>

## Repo Structure

-   **jorg-capture.el:** JOrg capture related functionality

-   **jorg-common.el:** functionality common across the other JOrg modules

-   **jorg.el:** main el file required by elisp packaging

-   **jorg-pkg.el:** elisp packaging

-   **jorg-project.el:** JOrg project related functionality

-   **README.md:** generated from README.org

-   **README.org:** This file, because we like ORG

-   **tests:** jorg unit tests


<a id="orgcd417bd"></a>

## Installation


<a id="org172a709"></a>

### Get JOrg

Get JOrg from github

1.  Using CLI: `git clone git@github.com:jasonchurch/jorg.git`
2.  Using Browser: <https://github.com/jasonchurch/jorg>. Click clone or download and select Download Zip.
3.  Some day we'll look at melpa&#x2026;


<a id="org172afe6"></a>

### Make Emacs aware of JOrg

In your emacs config file, .emacs or alternative locations.

    (add-to-list 'load-path "<path to JOrg folder")
    (require 'jorg)


<a id="org36d3053"></a>

### Customizations

The following are common JOrg customizations that can be added to
your local emacs config.

    (setq jorg-capture-key-main "j")  ;;To trigger capture menu for JORG: Cc c j
    (setq jorg-project-base-dir "/home/red0ck33g/org/2017/projects")
    (setq jorg-capture-project-task-target "TASKS")
    (setq jorg-user-name "Jason Church")
    (setq jorg-user-email "jasonchurch@edeveloper.ca")
    (global-set-key (kbd "C-c j s") 'jorg-switch-project)


<a id="orgc96308d"></a>

## Usage

Mostly you use JOrg to manage its project files, [What is a JOrg
Project File?](#org5a12637), the same way you would any other org file.  The
primary interface into JOrg are around capturing projects and their
Updates, Todos and Reference headings via ORG's capture system and
switching between JOrg Projects.


<a id="org9ebc471"></a>

### Jorg Capture

We'll assume you used the standard, default configuration, see
`jorg-capture-key-main` variable to change the default letter `j`.

-   **Cc c j:** JOrg Capture Menu


<a id="orgdd5d6f1"></a>

### Common User/Interactive Functions

These are functions meant to be used interactively by the user
either by <Alt-x> <function-name> or a keyboard shortcut.

The following are the available functions. Shortcuts have been
left to the users to setup, please add shortcuts to your local
emacs config for any of these commands.  Simply substitute the
keys and function you wish to use in the following example:

    (global-set-key (kbd "C-c j s") 'jorg-switch-project)

-   **jorg-switch-project:** switch between project files using a
    popup menu. The items are sorted by last modified, with
    current buffer, if a JOrg project, being at the top.


<a id="org5a12637"></a>

## What is a JOrg Project File?

JORG's Project - an org file that represents a single project:
movies I like; books to read;become a rocket scientest;build shiny
rocket ship; etc.  Each file consists of a single top level PROJECT
TO DO Heading with subheadings:

-   UPDATES to help keep track of progress or key info either for

your own sake (I have a poor memory) or a means to have an easy
update on hand to provide to interested parties.

-   TASKS for related NEXT ACTIONS (if your a GTD fan) or project

related TODOs.

-   RERERENCE a location for anything else related to your project:

snippets of info, images/diagrams, notes, important links, etc.

Example of Project Structure using perhaps a trivial example, but
some of us need all the organization we can get! While this example
shows the basic structure of a JOrg Project, there are more
attributes like Title, Author, etc and the project and other
headings may have various properties.

    * PROJECT [#A] Read ELisp Manual
    ** UPDATES
       - [2017-09-08] Found manual online, but its more cool reading it inside Emacs!
    ** TASKS
    *** NEXT Read Lisp Data Types
    ** REFERENCE
    *** NOTES
    **** 1.2 Lisp History
    ***** MacLisp created in the 1960s
    ***** Common Lisp, the result of many MacLisp decendants finally getting together
          - Emacs has a cl-lib which implements some common Lisp.

