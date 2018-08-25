# Import libraries
import os
import pandas as pd
import re

RAW_DATA_PATH = "source/105 building data"
COMBINED_DATA_PATH = "source/combined_bldg_data"
PROCESSED_DATA_PATH = "source/processed_bldg_data"

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

PWM_FORMULA = {
    'SDE-3': [['PWM-SDE3 IC1', 'PWM-SDE3 IC2'],
              ['PWM-SDE3 MCC (AC)', 'PWM-CELC IC1', 'PWM-CELC IC2', 'PWM-SDE1', 'PWM-SDE2 SSB', 'PWM-SDE2 AC',
               'PWM-SDE3 Ext', 'PWM-Street Light']]
}

# This function returns a list of raw data files by building name, month and year in the path.
def get_num_files_by_bldg_mth(data_path=RAW_DATA_PATH):

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

# This function returns the raw time series data for a building in the path.
def load_data_by_bldg(name, data_path=RAW_DATA_PATH):

    file_list = []
    for root, dirs, files in os.walk(data_path):

        if len(files) > 0:
            # Get the month and year from the containing folder name e.g. Jul_2015
            rootdir = root.split('/')
            month = MONTH_TO_NUM[rootdir[len(rootdir)-1].split('_')[0]]
            year = int(rootdir[len(rootdir) - 1].split('_')[1])

        for afile in files:
            # If the building name matches, then add to list of files.
            if afile.split('_')[0] == name:
                df = pd.read_csv(os.path.join(root, afile))
                file_list.append([afile, year, month, df])
    #return pd.concat(file_list)
    return file_list




# This function aggregates the raw time series PWM data for a building in the path according to the building's PWM
# formula.
def process_PWM_data_by_bldg(name, input_data_path=RAW_DATA_PATH, output_data_path=PROCESSED_DATA_PATH):

    result = False
    bldg_data_df = load_data_by_bldg(name, input_data_path)

    try:
        add_idx = list(map(lambda x: bldg_data_df.columns.get_loc(x), PWM_FORMULA[name][0]))
        subtract_idx = list(map(lambda x: bldg_data_df.columns.get_loc(x), PWM_FORMULA[name][1]))
    except KeyError:
        pass
    else:
        bldg_data_df['PWM_1'] = bldg_data_df.iloc[:, add_idx].sum(axis=1)
        bldg_data_df['PWM_2'] = bldg_data_df.iloc[:, subtract_idx].sum(axis=1)
        bldg_data_df['PWM_Agg'] = bldg_data_df['PWM_1'] + bldg_data_df['PWM_2']
        bldg_data_df.to_csv(output_data_path + '/' + name + '.csv')
        result = True

    return result

# This function checks if date/time field in the time series data is encoded day first. Returns False if
# time_series_data is null.
def is_day_first(file_name, time_series_data):

    result = False
    mmmyyyy = file_name.split('.')[0].split('_')[1]
    mmm = re.search('[A-Z][a-z]{2}', mmmyyyy)[0]

    # Extract the month from the filename.
    month = MONTH_TO_NUM[mmm]
    if not time_series_data.empty:
        if month != 1:
            # Check the 1st datetime value.
            result = int(time_series_data.iloc[0, 0].split('/')[1]) == month
        else:
            # For Jan, need to check datetime value for 2nd day i.e. 48 x 2 = 96.
            result = int(time_series_data.iloc[96, 0].split('/')[1]) == month

    return result

# This function combines all the raw time series PWM data in separate csv files into one csv file for a building.
# It also performs date/time and string to numeric conversions.
def combine_csv_files_by_bldg(name, input_data_path=RAW_DATA_PATH, output_data_path=COMBINED_DATA_PATH):

    bldg_data_list = load_data_by_bldg(name, input_data_path)

    # Convert the date/time for all the dataframes in the list.
    for i in bldg_data_list:
        if not i[3].empty:
            if is_day_first(i[0], i[3]):
                i[3].loc[:, 'Pt_timeStamp'] = pd.to_datetime(i[3].loc[:, 'Pt_timeStamp'], dayfirst=True)
            else:
                i[3].loc[:, 'Pt_timeStamp'] = pd.to_datetime(i[3].loc[:, 'Pt_timeStamp'])

    # Convert the strings to floats for all the dataframes in the list.
    for i in bldg_data_list:
        if not i[3].empty:
            for j in i[3].iloc[:, 1:].columns:
                i[3][j] = i[3][j].astype('str').apply(lambda x: x.replace(',', '')).astype('float')

    # Concatenate the list of dataframes into 1 single dataframe.
    df_list = []
    for i in bldg_data_list:
        df_list.append(i[3])
    bldg_data_df = pd.concat(df_list)

    # Reindex the dataframe using the year/month/day/time, add missing values for any period with no files.
    bldg_data_df.set_index('Pt_timeStamp', inplace=True)
    all_dates = pd.date_range('5/2015', '8/2018', freq='30min')
    bldg_data_df = bldg_data_df.reindex(all_dates)
    bldg_data_df.sort_index(inplace=True)

    # Copy index (i.e. Pt_timeStamp) back to a column
    bldg_data_df.insert(0, 'Pt_timeStamp', bldg_data_df.index)
    bldg_data_df.reset_index(drop=True, inplace=True)

    # Save the dataframe as csv file. Do not write row names (i.e. index 0,1,2,3,4,...)
    bldg_data_df.to_csv(output_data_path + '/' + name + '.csv', index=False)

    return None