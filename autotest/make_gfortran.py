#! /usr/bin/env python
try:
    import pymake
except:
    msg =  'Error. Pymake package is not available.\n'
    msg += 'Try installing using the following command:\n'
    msg += ' pip install https://github.com/modflowpy/pymake/zipball/master'
    print(msg)
    raise Exception()
import os
import shutil
import platform


def copytree(src, dst, symlinks=False, ignore=None):
    for item in os.listdir(src):
        s = os.path.join(src, item)
        d = os.path.join(dst, item)
        if os.path.isdir(s):
            shutil.copytree(s, d, symlinks, ignore)
        else:
            shutil.copy2(s, d)


def cleanup(srcdir, tempdir):
    """
    Method to copy source code and cleanup the mfnwt code base
    for gfortran compilation.
    """
    if os.path.isdir(tempdir):
        shutil.rmtree(tempdir)
    os.makedirs(tempdir)
    copytree(srcdir, "./temp")
    os.remove(os.path.join(tempdir, "Irestart.f"))
    return tempdir


if __name__ == "__main__":

    args = pymake.parser()

    srcdir = args.srcdir

    srcdir = cleanup(srcdir, "./temp")

    args.subdirs = False

    args.makefile = False

    #call main -- note that this form allows main to be called
    #from python as a function.
    fflags = "--static " + "-ffree-line-length-512 " + "-ffixed-line-length-132 " + "-Wuninitialized " + "-O2"
    try:
        syslibs="-lc"
        pymake.main(srcdir, args.target, args.fc, args.cc, args.makeclean,
                    args.expedite, args.dryrun, False, args.debug,
                    args.subdirs, fflags, arch=args.arch,
                    makefile=args.makefile)
    except AttributeError:
        pymake.main(srcdir, args.target, args.fc, args.cc, args.makeclean,
                    args.expedite, args.dryrun, False, args.debug,
                    args.subdirs, fflags, arch=args.arch,
                    makefile=args.makefile)

    shutil.rmtree(srcdir)
