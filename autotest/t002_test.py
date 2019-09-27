# test to compare head file output of models
import os
import numpy
import flopy as fp


t_dir = os.path.join(".", "temp")
valid_dir = os.path.join(".", "output")

binary = ("lak1b_bath.hds",
          "SWRSample05.hds",)

formatted = ("Pr1aMFNWT.out",
             "Pr1bMFNWT.out",
             "Pr2.out",
             "Pr3_higher.out",
             "Pr3_lower.out",
             "SFR_LAK_floodplainhd.out",
             )

def do_compare(model):
    pass


def test_outputs():
    pass


if __name__ == "__main__":
    pass