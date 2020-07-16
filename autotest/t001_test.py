import os
import platform
import shutil
import flopy as fp

print(os.getcwd())

nwt_exe_name = 'mfnwt'
if platform.system().lower() == "windows":
    nwt_exe_name = "mfnwt.exe"

nwt_exe = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                       nwt_exe_name)

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
              os.path.join("SWI_data_files", "swi2ex4sww.nam"),
              os.path.join("SWR_data_files", "SWRSample05",
                           "SWRSample05-nwt.nam"),
              os.path.join("UZF_cap_ET", "UZF_cap_ET.nam"),
              os.path.join("Uzf_testoptions", "UZFtestoptions.nam"),
              os.path.join("Ag_EP1a", "Agwater1a_high.nam"),
              os.path.join("Ag_EP1a", "Agwater1a_low.nam"),
              os.path.join("Ag_EP1b", "Agwater1b_high.nam"),
              os.path.join("Ag_EP1b", "Agwater1b_low.nam"),
              os.path.join("UZF_testproblem2", "UZFtest2.nam")]

models = [os.path.join(data_dir, model) for model in model_name]

has_external = {"l1b2k_bath.nam": ("lak1b_bath.txt",),
                "Sfr2weltab.nam": ("weltab1.txt",
                                   "weltab2.txt",
                                   "Sfr2weltab.wel"),
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
                                   os.path.join("input", "seg9.tab"),),
                "UZFtestoptions.nam": (),
                "Agwater1a_high.nam": (os.path.join("input", "seg1_high.tab"),
                                       os.path.join("input", "seg9.tab"),),
                "Agwater1a_low.nam": (os.path.join("input", "seg1_low.tab"),
                                      os.path.join("input", "seg9.tab")),
                "Agwater1b_high.nam": (os.path.join("input", "seg1.tab"),
                                       os.path.join("input", "seg9.tab")),
                "Agwater1b_low.nam": (os.path.join("input", "seg1.tab"),
                                      os.path.join("input", "seg9.tab")),
                "UZFtest2.nam": ("UZFtest2.uzf", "UZFtest2.ghb",
                                 "UZFtest2.oc", "UZFtest2.nwt",
                                 "UZFtest2.upw", "UZFtest2.sfr")}


def external_files(model, ows, f):
    # external file patch for flopy deficiency
    iws, _ = os.path.split(model)
    _, foo = os.path.split(f)
    shutil.copyfile(os.path.join(iws, f), os.path.join(ows, foo))


def do_model(model):
    model_ws, name = os.path.split(model)
    if name in ('placeholder',):
        copyfile = False
    else:
        # need to trick flopy....
        model_ws, _ = os.path.split(model_ws)
        if name in ("SWRSample05-nwt.nam",):
            model_ws, _ = os.path.split(model_ws)
        shutil.copyfile(model, os.path.join(model_ws, name))
        copyfile = True

        if platform.system().lower() != "windows":
            # fix paths for linux!
            with open(os.path.join(model_ws, name)) as foo:
                s = ""
                for line in foo:
                    s += line
                s.replace("\\", "/")
            with open(os.path.join(model_ws, name), "w") as foo:
                foo.write(s)

    if name in ("Sfr2weltab.nam", ):
        # Sfr2weltab: need to avoid loading WEL file due to tabfiles
        ml = fp.modflow.Modflow.load(name,
                                     exe_name=nwt_exe,
                                     model_ws=model_ws,
                                     check=False,
                                     load_only=["DIS", "GHB", "BAS6", "UPW",
                                                "NWT", "OC", "SFR", "GAGE"])

    elif name in ("UZFtest2.nam", ):
        # UZFtest2.nam: avoid loading the OC file, because it gives us
        # an unspecified difference between head outputs. Can't figure out why
        ml = fp.modflow.Modflow.load(name,
                                     exe_name=nwt_exe,
                                     model_ws=model_ws,
                                     check=False,
                                     load_only=["DIS", "BAS6", "GAGE",
                                                "WEL", ])

    else:
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
            try:
                external_files(model, out_dir, f)
            except FileNotFoundError:
                pass

        external_fnames = ml.external_fnames
        ml.external_fnames = [os.path.split(p)[-1] for p in external_fnames]

    ml.write_input()

    # fix the name files that we can't load a package with in flopy
    if name in ("Sfr2Weltab.nam", "Prob1.nam", "UZFtest2.nam"):
        with open(os.path.join(out_dir, name)) as foo:
            tmp = [line for line in foo]
        with open(os.path.join(out_dir, name), "w") as foo:
            foo.writelines(tmp)

            if name == "Sfr2Weltab.nam":
                foo.write("WEL   91   Sfr2weltab.wel")
            elif name == "UZFtest2.nam":
                foo.write("UZF   19   UZFtest2.uzf\n")
                foo.write("GHB   17   UZFtest2.ghb\n")
                foo.write("NWT   13   UZFtest2.nwt\n")
                foo.write("OC    14   UZFtest2.oc\n")
                foo.write("UPW   7    UZFtest2.upw\n")
                foo.write("SFR   15   UZFtest2.sfr")
            else:
                pass

    ml = fp.modflow.Modflow.load(name,
                                 exe_name=nwt_exe,
                                 model_ws=out_dir,
                                 check=False,
                                 forgive=True)

    success, _ = ml.run_model()

    assert success, ismfnwt


def test_pwd():
    wd = os.getcwd()
    _, cur = os.path.split(wd)
    assert cur == "autotest", os.getcwd()


def test_mfnwt_exists():
    flist = os.listdir(".")
    if nwt_exe_name not in flist:
        assert False, flist


def test_run_model():
    for model in models:
        yield do_model, model
    return


if __name__ == "__main__":
    test_pwd()
    test_mfnwt_exists()
    # test_run_model()
    for model in models:
        do_model(model)
