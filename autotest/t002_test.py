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
             "SFR_LAK_floodplainhd.out")
             # "Sfr2weltab.out") out of defined tolerance


def validate(sim_array, valid_array):
    validate = (sim_array - valid_array) / valid_array

    failure = np.where(np.abs(validate) > 0.01)
    if failure[0].size > 0:
        return True
    else:
        return False


def do_compare_formatted_head_file(output):
    files = [f for f in os.listdir(t_dir)
             if os.path.isfile(os.path.join(f))]
    assert False, files

    sim = fp.utils.FormattedHeadFile(os.path.join(t_dir, output))
    sim_head = sim.get_alldata()
    valid = fp.utils.FormattedHeadFile(os.path.join(valid_dir, output))
    valid_head = valid.get_alldata()

    failed = validate(sim_head, valid_head)

    if failed:
        raise AssertionError("Simulated head out of defined tolerance")


def test_outputs():
    for output in formatted:
        yield do_compare_formatted_head_file, output
    return


if __name__ == "__main__":
    for output in formatted:
        do_compare_formatted_head_file(output)