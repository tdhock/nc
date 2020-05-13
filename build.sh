#!/bin/bash
set -o errexit
PREGEX="^Package: "
PKG=$(grep $PREGEX DESCRIPTION|sed "s/$PREGEX//")
echo Package from DESCRIPTION: $PKG
##R -e "update.packages()"
##R -e "if(!require(devtools))install.packages('devtools');devtools::install_dev_deps()"
R -e "if(require(inlinedocs))package.skeleton.dx('.')"
cd ..

RELEASE=$PKG-release
echo Copying $PKG to $RELEASE
rm -rf $RELEASE
cp -r $PKG $RELEASE

echo Editing $RELEASE for CRAN submission
grep -v Remotes $PKG/DESCRIPTION > $RELEASE/DESCRIPTION
rm $RELEASE/tests/testthat/*
cp $PKG/tests/testthat/test-CRAN*.R $RELEASE/tests/testthat

echo Building $RELEASE
RCMD="R --vanilla CMD"
$RCMD build $RELEASE | tee build.out
PKG_TGZ=$(grep building build.out|sed "s/.*\($PKG.*.tar.gz\).*/\1/")

echo Installing $PKG_TGZ
$RCMD INSTALL $PKG_TGZ

RE2R_DIR=`Rscript --vanilla -e "cat(file.path(.libPaths()[1], 're2r'))" `
RE2R_OLD_DIR="${RE2R_DIR}-old"
if [ -d "$RE2R_OLD_DIR" ]; then #check WITHOUT re2r failed last time.
    echo moving $RE2R_OLD_DIR to $RE2R_DIR
    mv "$RE2R_OLD_DIR" "$RE2R_DIR"
fi
RCMD_CHECK="$RCMD check --as-cran $PKG_TGZ"

echo Checking $PKG_TGZ WITHOUT re2r
mv "$RE2R_DIR" "$RE2R_OLD_DIR"
_R_CHECK_FORCE_SUGGESTS_=0 $RCMD_CHECK

echo Checking $PKG_TGZ WITH re2r
mv "$RE2R_OLD_DIR" "$RE2R_DIR"
$RCMD_CHECK

echo Checking without any Suggests
R -e "if('check_without_suggests' %in% ls())check_without_suggests('$PKG_TGZ')"

# what is the point? https://cloud.r-project.org/doc/manuals/r-release/R-exts.html#Suggested-packages
# echo Checking without any Suggests
# _R_CHECK_DEPENDS_ONLY_=true $RCMD_CHECK
