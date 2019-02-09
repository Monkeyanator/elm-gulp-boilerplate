"use strict";

var gulp = require('gulp');
var elm = require('gulp-elm');
var sass = require('gulp-sass');
var minifyCSS = require('gulp-minify-css');
var concat = require('gulp-concat');

var paths = {
    styles: {
        src: 'src/styles/**/*.scss',
        dest: 'dist/styles'
    },
    scripts: {
        src: 'src/elm/**/*.elm',
        dest: 'dist/'
    },
    static: {
        src: 'src/index.html',
        dest: 'dist/'
    }
};

/* steps for Elm transpilation */
gulp.task('elm', function () {
    return gulp.src(paths.scripts.src)
        .pipe(elm.make())
        .pipe(gulp.dest(paths.scripts.dest));
});

gulp.task('elm-prod', function () {
    return gulp.src(paths.scripts.src, {optimize: true})
        .pipe(elm.make())
        .pipe(gulp.dest(paths.scripts.dest));
});

/* steps for sass to css compilation */
gulp.task('sass', function () {
    return gulp.src(paths.styles.src)
        .pipe(sass().on('error', sass.logError))
        .pipe(concat('style.min.css'))
        .pipe(minifyCSS())
        .pipe(gulp.dest(paths.styles.dest));
});

/* move static assets, such as images and raw base html into place */
gulp.task('static', function () {
    return gulp.src(paths.static.src)
        .pipe(gulp.dest('dist/'));
});

/* build aggregation steps */
gulp.task('build-dev', function (done) {
    var buildTasks = gulp.parallel(['elm', 'sass', 'static']);
    buildTasks();
    done();
});

gulp.task('build-prod', function (done) {
    var buildTasks = gulp.parallel(['elm-prod', 'sass', 'static']);
    buildTasks();
    done();
});

/* dev watch rebuild on css or elm changes */
gulp.task('watch', function () {
    gulp.watch(paths.styles.src, gulp.series('sass'));
    gulp.watch(paths.scripts.src, gulp.series('elm'));
});