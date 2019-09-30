# Remove the temp directory and then create a fresh one
import os
import shutil
import subprocess as sp
import platform


def test_setup():
    tempdir = os.path.join('.', 'temp')
    if os.path.isdir(tempdir):
        shutil.rmtree(tempdir)
    os.mkdir(tempdir)

    if platform.system().lower() == 'windows':
        pass
    else:
        sp.Popen(["chmod", "a+rwx", "temp"], stdout=sp.PIPE,
                    stderr=sp.STDOUT, cwd=".",
                    shell=True)
    return


if __name__ == '__main__':
    test_setup()
