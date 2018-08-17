# Import libraries
import os

DATA_PATH = "source/105 building data"
MONTH_TO_NUM = {
    'Jan': 1,
    'Feb': 2,
    'Mar': 3,
    'Apr': 4,
    'May': 5,
    'Jun': 6,
    'Jul': 7,
    'Aug': 8,
    'Sep': 9,
    'Oct': 10,
    'Nov': 11,
    'Dec': 12
}

# This function returns a list of files by building name, month and year in the path.
def get_num_files_by_bldg_mth(data_path=DATA_PATH):

    file_list = []

    for root, dirs, files in os.walk(data_path):

        if len(files) > 0:
            # Get the month and year from the containing folder name e.g. Jul_2015
            rootdir = root.split('/')
            month = MONTH_TO_NUM[rootdir[len(rootdir)-1].split('_')[0]]
            year = int(rootdir[len(rootdir) - 1].split('_')[1])

        for afile in files:
            # Get the building name and file size, then add to list of files.
            bldg_name = afile.split('_')[0]
            file_size = os.path.getsize(os.path.join(root, afile))
            file_list.append([bldg_name, year, month, file_size])

    return file_list