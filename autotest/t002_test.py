# test to compare head file output of models
import os
import numpy as np
import flopy as fp


t_dir = os.path.join(".", "temp")
valid_dir = os.path.join(".", "output")

binary = ("lak1b_bath.hds",
          "SWRSample05.hds",
          )

formatted = ("Pr1aMFNWT.out",
             "Pr1bMFNWT.out",
             "Pr2.out",
             "Pr3_higher.out",
             "Pr3_lower.out",
             "SFR_LAK_floodplainhd.out",
             "Sfr2weltab.out",
             "UZFtestoptions.out",
             "Agwater1a_high.hed",
             "Agwater1a_low.hed",
             "Agwater1b_high.hed",
             "Agwater1b_low.hed",
             "UZFtest2hd.out",)


def validate(sim_array, valid_array):
    validate = (sim_array - valid_array) / valid_array

    failure = np.where(np.abs(validate) > 0.01)
    if failure[0].size > 0:
        check = abs(validate) * sim_array
        check[np.isinf(check)] = 0.
        check[np.isnan(check)] = 0
        if np.max(check) < 0.5:
            return False
        else:
            return True
    else:
        return False


def do_compare_formatted_head_file(output):
    sim = fp.utils.FormattedHeadFile(os.path.join(t_dir, output))
    sim_head = sim.get_alldata()
    valid = fp.utils.FormattedHeadFile(os.path.join(valid_dir, output))
    valid_head = valid.get_alldata()

    failed = validate(sim_head, valid_head)

    if failed:
        raise AssertionError("Simulated head out of defined tolerance")


def do_compare_binary_head_files(output):
    sim = fp.utils.HeadFile(os.path.join(t_dir, output))
    sim_head = sim.get_alldata()
    valid = fp.utils.HeadFile(os.path.join(valid_dir, output))
    valid_head = valid.get_alldata()

    failed = validate(sim_head, valid_head)

    if failed:
        raise AssertionError("Simulated head out of defined tolerance")


def test_formatted_outputs():
    for output in formatted:
        yield do_compare_formatted_head_file, output
    return


def test_binary_outputs():
    for output in binary:
        yield do_compare_binary_head_files, output
    return


if __name__ == "__main__":
    for output in formatted:
         do_compare_formatted_head_file(output)
    for output in binary:
        do_compare_binary_head_files(output)