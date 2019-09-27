import os
import sys
from subprocess import Popen
import platform
import shutil
import flopy as fp

print(os.getcwd())

nwt_exe = 'mfnwt'
if platform.system().lower() == "windows":
    nwt_exe = "mfnwt.exe"

ismfnwt = fp.which(nwt_exe)

data_dir = os.path.join("..", "MODFLOW-NWT", "data")
out_dir = os.path.join(".", "temp")

# relative model path with regard to data directory
# add new models here to the test scenarios!
model_name = [os.path.join("Ex_prob1a", "Pr1a_MFNWT.nam"),
              os.path.join("Ex_prob1b", "Pr1b_MFNWT.nam"),
              os.path.join("Ex_prob2", "Pr2MFNWT.nam"),
              os.path.join("Ex_prob3", "Pr3_MFNWT_higher.nam"),
              os.path.join("Ex_prob3", "Pr3_MFNWT_lower.nam"),
              os.path.join("Lake_bath_example", "l1b2k_bath.nam"),
              os.path.join("Sfr2weltab", "Sfr2weltab.nam"),
              os.path.join("SFR_LAK_floodplain", "SFR_LAK_floodplain.nam"),
              # os.path.join("SWI_data_files", "swi2ex4sww.nam"), # says invalid flow package
              os.path.join("SWR_data_files", "SWRSample05",
                           "SWRSample05-nwt.nam"),
              os.path.join("UZF_cap_ET", "UZF_cap_ET.nam"),
              #os.path.join("UZF_testoptions", "UZFtestoptions.nam"), # output to 61 broken
              ]

models = [os.path.join(data_dir, model) for model in model_name]

has_external = {"l1b2k_bath.nam": ("lak1b_bath.txt",),
                "Sfr2weltab.nam": ("weltab1.txt",
                                   "weltab2.txt"),
                "SFR_LAK_floodplain.nam": (os.path.join("input",
                                           "SFR_LAK_floodplain_bath.txt"),
                                           os.path.join("input",
                                           "SFR_LAK_floodplain.tab")),
                "SWRSample05-nwt.nam": ("IrregularCrossSection_Reach17.dat",
                                        "IrregularCrossSection_Reach18.dat",
                                        "SVAPCrossSection_Reach19.dat",
                                        os.path.join("ref",
                                                     "ConstantStage.dat")),
                "UZF_cap_ET.nam": (os.path.join("input", "seg1.tab"),
                                   os.path.join("input", "seg9.tab")),}


def external_files(model, ows, f):
    # external file patch for flopy deficiency
    iws, _ = os.path.split(model)
    _, foo = os.path.split(f)
    shutil.copyfile(os.path.join(iws, f), os.path.join(ows, foo))


def do_model(model):
    model_ws, name = os.path.split(model)
    if name in ("swi2ex4sww.nam", "SWRSample05-nwt.nam"):
        copyfile = False
    else:
        # need to trick flopy....
        model_ws, _ = os.path.split(model_ws)
        shutil.copyfile(model, os.path.join(model_ws, name))
        copyfile = True

    ml = fp.modflow.Modflow.load(name,
                                 exe_name=nwt_exe,
                                 model_ws=model_ws,
                                 check=False)
    # remove the temporary name file
    if copyfile:
        os.remove(os.path.join(model_ws, name))

    ml.change_model_ws(out_dir)

    if name in has_external:
        ext_f = has_external[name]
        for f in ext_f:
            external_files(model, out_dir, f)

        external_fnames = ml.external_fnames
        ml.external_fnames = [os.path.split(p)[-1] for p in external_fnames]

    ml.write_input()
    try:
        if platform.system().lower() == 'windows':
            success, _ = ml.run_model()
        else:
            argv = [nwt_exe, name]
            success, _ = run_model_linux(argv, out_dir)
    except:
        success = False
    assert success, ismfnwt


def run_model_linux(argv, model_ws):
    import subprocess as sp
    silent = False
    normal_msg = 'normal termination'
    buff = []
    report = False
    success = False

    proc = sp.Popen(argv, stdout=sp.PIPE,
                    stderr=sp.STDOUT, cwd=model_ws,
                    shell=True)

    while True:
        line = proc.stdout.readline()
        c = line.decode('utf-8')
        if c != '':
            for msg in normal_msg:
                if msg in c.lower():
                    success = True
                    break
            c = c.rstrip('\r\n')
            if not silent:
                print('{}'.format(c))
            if report:
                buff.append(c)
        else:
            break
    return success, buff


def test_pwd():
    wd = os.getcwd()
    _, cur = os.path.split(wd)
    assert cur == "autotest", os.getcwd()


def test_mfnwt_exists():
    list, dirs, files = os.walk(".")
    if nwt_exe not in list[-1]:
        assert False, list[-1]


def test_run_model():
    for model in models:
        yield do_model, model
    return


if __name__ == "__main__":
    test_pwd()
    test_mfnwt_exists()
    test_run_model()
    # do_model(models[0])