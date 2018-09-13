######################################################################################################################
# Import libraries
######################################################################################################################
import os
import pandas as pd
import re
import datetime
import math
import matplotlib.pyplot as plt

######################################################################################################################
# Private Parameters
######################################################################################################################
_RAW_DATA_PATH_ = os.path.join('source', '105 building data')
_COMBINED_DATA_PATH_ = os.path.join('source', 'combined_bldg_data')
_PROCESSED_DATA_PATH_ = os.path.join('source', 'processed_bldg_data')
_MSG_LOG_FILE_ = os.path.join('source', 'log', 'logfile.txt')

_DATA_TYPE_TO_PATH_ = {
    'combined': _COMBINED_DATA_PATH_,
    'processed': _PROCESSED_DATA_PATH_
}

_MONTH_TO_NUM_ = {
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

_PWM_FORMULA_ = {
    'SDE-3': [['PWM-SDE3 IC1', 'PWM-SDE3 IC2'],
              ['PWM-SDE3 MCC (AC)', 'PWM-CELC IC1', 'PWM-CELC IC2', 'PWM-SDE1', 'PWM-SDE2 SSB', 'PWM-SDE2 AC',
               'PWM-SDE3 Ext', 'PWM-Street Light']]
}


######################################################################################################################
# Public Functions
######################################################################################################################
# This function returns a list of raw data files by building name, month and year in the path.
def get_num_files_by_bldg_mth(data_path=_RAW_DATA_PATH_):

    file_list = []

    for root, dirs, files in os.walk(data_path):

        if len(files) > 0:
            # Get the month and year from the containing folder name e.g. Jul_2015
            rootdir = root.split('/')
            month = _MONTH_TO_NUM_[rootdir[len(rootdir)-1].split('_')[0]]
            year = int(rootdir[len(rootdir) - 1].split('_')[1])

            for afile in files:
                # Get the building name and file size, then add to list of files.
                bldg_name = afile.split('_')[0]
                file_size = os.path.getsize(os.path.join(root, afile))
                file_list.append([bldg_name, year, month, file_size])

    return file_list


# This function loads the time series data for a list of building names. type is defined in _DATA_TYPE_TO_PATH_.
# It returns a list of [[name, data frame], ...]
def load_data_by_bldg(bldg_name_list, type, data_path=None):

    bldg_df_list = []
    for i in bldg_name_list:
        if data_path==None:
            df = pd.read_csv(os.path.join(_DATA_TYPE_TO_PATH_[type], i + '.csv'), index_col=0, parse_dates=True)
        else:
            df = pd.read_csv(os.path.join(data_path, i + '.csv'), index_col=0, parse_dates=True)
        bldg_df_list.append([i, df])

    return bldg_df_list


# This function aggregates the raw time series PWM data for a building in the path according to the building's PWM
# formula.
# **** This needs to be fixed. *****
def process_PWM_data_by_bldg(name, input_data_path=_RAW_DATA_PATH_, output_data_path=_PROCESSED_DATA_PATH_):

    result = False
    bldg_data_df = _load_data_by_bldg_(name, input_data_path)

    try:
        add_idx = list(map(lambda x: bldg_data_df.columns.get_loc(x), _PWM_FORMULA_[name][0]))
        subtract_idx = list(map(lambda x: bldg_data_df.columns.get_loc(x), _PWM_FORMULA_[name][1]))
    except KeyError:
        pass
    else:
        bldg_data_df['PWM_1'] = bldg_data_df.iloc[:, add_idx].sum(axis=1)
        bldg_data_df['PWM_2'] = bldg_data_df.iloc[:, subtract_idx].sum(axis=1)
        bldg_data_df['PWM_Agg'] = bldg_data_df['PWM_1'] + bldg_data_df['PWM_2']
        bldg_data_df.to_csv(os.path.join(output_data_path, name + '.csv'))
        result = True

    return result


# This function checks if date/time field in the time series data is encoded day first. Returns False if
# time_series_data is null.
def is_day_first(file_name, time_series_data):

    result = False
    ambiguous = True
    mmmyyyy = file_name.split('.')[0].split('_')[1]
    mmm = re.search('[A-Z][a-z]{2}', mmmyyyy)[0]

    # Extract the month from the filename.
    month = _MONTH_TO_NUM_[mmm]
    if not time_series_data.empty:

        # Look for the first date in which the first or second number differ from the month. That number is the day.
        for index, row in time_series_data.iterrows():
            # Get the first 2 numbers of the date field.
            first_num = row[0].split(' ')[0].split('/')[0]
            second_num = row[0].split(' ')[0].split('/')[1]
            if int(first_num) != month:
                # Day first
                result = True
                ambiguous = False
                break
            elif int(second_num) != month:
                # Month first
                ambiguous = False
                break

    return result, ambiguous


# This function combines all the raw time series PWM data in separate csv files into one csv file for a building.
# It also performs date/time and string to numeric conversions.
def combine_csv_files_by_bldg(name, input_data_path=_RAW_DATA_PATH_, output_data_path=_COMBINED_DATA_PATH_):

    # MISSING_DATA_RATIO = .95
    bldg_data_list = _load_data_by_bldg_(name, input_data_path)

    # Perform pre-processing for all the dataframes in the list.
    for i in bldg_data_list:
        if not i[3].empty:

            # Convert the date/time.
            day_first, unclear = is_day_first(i[0], i[3])
            if unclear:
                # Log error message
                _write_msg_log_(i[0] + 'date format unclear.', log=output_data_path+'/logfile.txt')

            if day_first:
                i[3].loc[:, 'Pt_timeStamp'] = pd.to_datetime(i[3].loc[:, 'Pt_timeStamp'], dayfirst=True)
            else:
                i[3].loc[:, 'Pt_timeStamp'] = pd.to_datetime(i[3].loc[:, 'Pt_timeStamp'])

            # Convert the strings to floats for all the dataframes in the list.
            for j in i[3].iloc[:, 1:].columns:
                i[3][j] = i[3][j].astype('str').apply(lambda x: x.replace(',', '')).astype('float')

            ####################################
            # Add any other pre-processing here.
            ####################################

        # Remove whitespaces and dashes from the column names, even for empty data frames.
        new_col_name_list = []
        for j in i[3].columns:
            new_col_name_list.append(re.sub('[ -]', '', j))
        i[3].columns = new_col_name_list

    # Concatenate the list of dataframes into 1 single dataframe.
    df_list = []
    for i in bldg_data_list:
        df_list.append(i[3])

    if df_list:  # not empty list

        # Concatenate the dataframes in the list.
        bldg_data_df = pd.concat(df_list)

        # After concatenation, pandas sorts the columns by lexico order. Change Pt_Timestamp to first position.
        pt_ts_col_idx = bldg_data_df.columns.get_loc('Pt_timeStamp')
        cols = bldg_data_df.columns.tolist()
        cols = cols[pt_ts_col_idx:pt_ts_col_idx+1] + cols[:pt_ts_col_idx] + cols[pt_ts_col_idx+1:]
        bldg_data_df = bldg_data_df[cols]

        # Save the dataframe as csv file. Do not write row names (i.e. index 0,1,2,3,4,...)
        bldg_data_df.to_csv(output_data_path + '/' + name + '.csv', index=False)
    else:
        # Log error message.
        _write_msg_log_(name + ' has no data.', log=os.path.join(output_data_path, 'logfile.txt'))

    return None


# This function plots the cumulative time series data for up to ten buildings.
# bldg_df_list is a list of [[bldg name, data frame]]
def plot_pwm_upto10_bldgs(bldg_df_list):

    nrows = math.ceil(len(bldg_df_list)/2)
    height = nrows * 4
    no_more_plots = False

    fig, ax = plt.subplots(nrows=nrows, ncols=2, figsize=(20, height))
    # list_idx = 0
    for row_idx, row in enumerate(ax):
        for col_idx, col in enumerate(row):
            if not no_more_plots:
                # Read the csv file which has all the cumulative time series data for a building.
                # a_bldg_df = pd.read_csv(data_path + '/' + bldg_list[list_idx] + '.csv', index_col=0, parse_dates=True)

                # Get the PWM related column names
                a_bldg_pwm_columns = []
                # Index of the building name and dataframe list
                bldg_idx = row_idx * len(row) + col_idx

                # Get the column names in the data frame related to PWM
                for i in bldg_df_list[bldg_idx][1].columns:
                    if 'PWM' in i:
                        a_bldg_pwm_columns.append(i)

                # Plot the time series data.
                col.plot(bldg_df_list[bldg_idx][1].loc[:, a_bldg_pwm_columns])
                col.set_title(bldg_df_list[bldg_idx][0]+' PWM over 2015-2018')

                # If more than 10 PWM columns plotted, don't show the legend (no space).
                if len(a_bldg_pwm_columns) < 10:
                    col.legend(bldg_df_list[bldg_idx][1].loc[:, a_bldg_pwm_columns])

                if bldg_idx >= len(bldg_df_list) - 1:
                    no_more_plots = True
            else:
                col.axis('off')
    plt.show()
    return None


# This function re-indexes a time series data frame by filling in missing 30 min periods for a month.
# def reindex_ts_df_mth(ts_df, month, year):
#
#     # Copy the data frame.
#     df = ts_df.copy()
#
#     # Reindex the dataframe using the year/month/day/time, add NaN values for any period with no data.
#     df.set_index('Pt_timeStamp', inplace=True)
#     mmyyyy = str(month) + '/' + str(year)
#     if not month % 12:
#         mmplus1yyyy = str(1) + '/' + str(year+1)
#     else:
#         mmplus1yyyy = str(month + 1) + '/' + str(year)
#     all_dates = pd.date_range(mmyyyy, mmplus1yyyy, freq='30min')
#     df = df.reindex(all_dates)
#     df.sort_index(inplace=True)
#
#     return df


# This function re-indexes a time series data frame by filling in missing 30 min periods for a date range.
# e.g. mmyyyy_start='5/2015'
def reindex_ts_df(ts_df, mmyyyy_start, mmyyyy_end):
    # Copy the data frame.
    df = ts_df.copy()

    # Reindex the dataframe using the year/month/day/time, add NaN values for any period with no data.
    all_dates = pd.date_range(mmyyyy_start, mmyyyy_end, freq='30min')
    df = df.reindex(all_dates)
    df.sort_index(inplace=True)

    return df

# This function calculates the proportion of NaNs in a data frame.
def ratio_nan(df):
    return df.isnull().sum().sum()/(len(df.columns)*len(df))


# This function reads the log file.
def read_msg_log(log=_MSG_LOG_FILE_):

    messages = None
    try:
        with open(log, 'r') as logfile:
            messages = logfile.read()
    except IOError:
        pass
    return messages


######################################################################################################################
# Private Functions
######################################################################################################################
# This function writes an error message to the message log.
def _write_msg_log_(msg, log=_MSG_LOG_FILE_):
    logfile = open(log, 'a')
    logfile.write(datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' ' + msg + '\n')
    logfile.close()
    return None


# This function returns the raw time series data for a building in the path.
def _load_data_by_bldg_(name, data_path=_RAW_DATA_PATH_):

    file_list = []
    for root, dirs, files in os.walk(data_path):

        if len(files) > 0:
            # Get the month and year from the containing folder name e.g. Jul_2015
            rootdir = root.split('/')
            month = _MONTH_TO_NUM_[rootdir[len(rootdir)-1].split('_')[0]]
            year = int(rootdir[len(rootdir) - 1].split('_')[1])

            for afile in files:
                # If the building name matches, then add to list of files.
                if afile.split('_')[0] == name:
                    df = pd.read_csv(os.path.join(root, afile))
                    file_list.append([afile, year, month, df])
    return file_list
