# Remove the temp directory and then create a fresh one
import os
import shutil


def test_setup():
    tempdir = os.path.join('.', 'temp')
    if os.path.isdir(tempdir):
        shutil.rmtree(tempdir)
    os.mkdir(tempdir)
    shutil.copyfile('mfnwt', os.path.join('temp', 'mfnwt'))
    return


if __name__ == '__main__':
    test_setup()
