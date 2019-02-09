# Elm / Sass / Gulp boilerplate project
> Personal boilerplate project to get started on a new Elm project without the plumbing

Disclaimer: I'm not a front-end developer, and the choices I made in this configuration might not be ideal. Please bring anything barbarous to my attention if it bothers you :)

## Features

The boilerplate project here is lean and not intended to be extensible for all users. It uses various Gulp tasks to compile the Elm, minify and mash together the scss, and move static assets into `dist`.

The relevant Gulp tasks included are as follows:

* `gulp build-dev`: build non-optimized Elm and SCSS into `dist`
* `gulp build-prod`: build optimized and minified Elm and SCSS into `dist`
* `gulp watch`: watch for changes on Elm / SCSS sources and trigger rebuild on change

Peek into `gulpfile.js` for finer control.

## Usage

To use this boilerplate as a template for a new project, clone the repo as follows:

`git clone https://github.com/Monkeyanator/elm-gulp-boilerplate.git`

and remove the git references with 

`rm -rf .git` 

And then you're free to initialize a new git repo, or use perforce, or whatever.

## Aren't there already a ton of Elm boilerplate projects?

Yes, but none made by me :) I wanted to learn how to use Gulp and practice defining frontend asset pipelines.