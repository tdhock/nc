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

echo Checking $PKG_TGZ WITH re2r
RCMD_CHECK="$RCMD check --as-cran $PKG_TGZ"
$RCMD_CHECK

echo Checking $PKG_TGZ WITHOUT re2r
RE2R_DIR=`Rscript --vanilla -e "cat(system.file(package='re2r'))" `
RE2R_OLD_DIR="${RE2R_DIR}-old"
mv "$RE2R_DIR" "$RE2R_OLD_DIR"
_R_CHECK_FORCE_SUGGESTS_=0 $RCMD_CHECK
mv "$RE2R_OLD_DIR" "$RE2R_DIR"
