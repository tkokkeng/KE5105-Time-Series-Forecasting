######################################################################################################################
# Import libraries
######################################################################################################################
import os
import pandas as pd
import re
import datetime
import math
import matplotlib.pyplot as plt
import numpy as np
from pandas.tseries.offsets import MonthBegin, MonthEnd
import json
from sklearn.base import BaseEstimator, TransformerMixin
from keras.utils import Sequence

######################################################################################################################
# Public Parameters
######################################################################################################################

# Mask value for missing data
MASK_VALUE = -1

######################################################################################################################
# Private Parameters
######################################################################################################################

# directories
_RAW_DATA_PATH_ = os.path.join('source', '105 building data')
_COMBINED_DATA_PATH_ = os.path.join('source', 'combined_bldg_data')
_PROCESSED_DATA_PATH_ = os.path.join('source', 'processed_bldg_data')
_IMPUTED_TRAIN_DATA_PATH_ = os.path.join('source', 'imputed_bldg_data', 'train')
_IMPUTED_TEST_DATA_PATH_ = os.path.join('source', 'imputed_bldg_data', 'test')
_MISC_DATA_PATH_ = os.path.join('source', 'other_data')

# files
_MSG_LOG_FILE_ = os.path.join('source', 'log', 'logfile.txt')
_BLDG_PWM_FORMULAE_FILE_ = os.path.join(_MISC_DATA_PATH_, 'bldg-PWM-formulae.json')
_BLDG_BTU_FORMULAE_FILE_ = os.path.join(_MISC_DATA_PATH_, 'bldg-BTU-formulae.json')

# Data input conversion utilities
_DATA_TYPE_TO_PATH_ = {
    'raw': _RAW_DATA_PATH_,
    'combined': _COMBINED_DATA_PATH_,
    'processed': _PROCESSED_DATA_PATH_,
    'imputed_train': _IMPUTED_TRAIN_DATA_PATH_,
    'imputed_test': _IMPUTED_TEST_DATA_PATH_
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

# Aggregation formulae for PWM and BTU for each building
with open(_BLDG_PWM_FORMULAE_FILE_) as json_file:
    _PWM_FORMULA_ = json.load(json_file)

with open(_BLDG_BTU_FORMULAE_FILE_) as json_file:
    _BTU_FORMULA_ = json.load(json_file)


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


# This function loads the time series data for a list of building names. data_type is defined in _DATA_TYPE_TO_PATH_.
# It returns a list of [[name, data frame], ...]
# If data_type='raw', building list must have only 1 building.
def load_data_by_bldg(bldg_name_list, data_type, data_path=None):

    bldg_df_list = []

    if data_type == 'raw':
        if data_path is None:
            bldg_df_list = _load_data_by_bldg_(bldg_name_list[0])
        else:
            bldg_df_list = _load_data_by_bldg_(bldg_name_list[0], data_path=data_path)
    # data_type is 'combined' or 'processed' or 'imputed_train' or 'imputed_test'
    else:
        # load all files
        if bldg_name_list == 'all':
            # Get all the building filenames referencing the folder 'combined_bldg_data'
            files = os.listdir('combined_bldg_data')
            if data_path is None:
                files = [os.path.join(_DATA_TYPE_TO_PATH_[data_type], i) for i in files]
            else:
                files = [os.path.join(data_path, i) for i in files]
            for i in files:
                df = pd.read_csv(i, index_col=0, parse_dates=True)
                df.sort_index(inplace=True)
                bldg_name = i.split(os.path.sep)[-1].split('.')[0]
                bldg_df_list.append([bldg_name, df])
        # load files in specified building name list
        else:
            for i in bldg_name_list:
                if data_path is None:
                    df = pd.read_csv(os.path.join(_DATA_TYPE_TO_PATH_[data_type], i + '.csv'),
                                     index_col=0, parse_dates=True)
                else:
                    df = pd.read_csv(os.path.join(data_path, i + '.csv'), index_col=0, parse_dates=True)
                df.sort_index(inplace=True)
                bldg_df_list.append([i, df])

    return bldg_df_list


# This function aggregates the raw time series PWM data for a building in the path according to the building's PWM
# formula.
# name is a list of building names or 'all'; errors are logged to _MSG_LOG_FILE_
# Returns True if at least 1 building data is written to a csv file.
def process_data_by_bldg(bldg_name_list, input_data_path=_COMBINED_DATA_PATH_, output_data_path=_PROCESSED_DATA_PATH_):

    result = False
    bldg_df_list = load_data_by_bldg(bldg_name_list, 'combined', input_data_path)

    for name, df in bldg_df_list:

        # Reindex the cumulative data to add any missing time periods. This is needed for differencing.
        start = df.index.min() - MonthBegin(n=1)  # set to first day of month
        start = start.replace(hour=0, minute=0)  # set time to 00h00
        end = df.index.max() + MonthEnd(n=1)  # set to last day of month
        end = end.replace(hour=23, minute=30)  # set time to 23h30
        df = reindex_ts_df(df, start, end)

        # Difference the cumulative data to get the 30min data.
        for i in df.columns:
            # if value is zero or difference is negative, set to NaN
            df[i + '_30min_avg'] = df[i].rolling(2).apply(
                lambda x: (x[1] - x[0]) if ((x[0] > 0) and (x[1] >= x[0])) else np.NaN)

        # Calculate the aggregate PWM according to building formula.
        pwm_formula_err = False
        try:
            # Get the components from the building formula - attributes, additive modifiers, multiplicative modifiers.
            # See comments in function definition.
            pwm_add_idx, pwm_add_add_mods, pwm_add_multi_mods = _decompose_formula_(df, _PWM_FORMULA_[name][0])
            pwm_subtract_idx, pwm_subtract_add_mods, pwm_subtract_multi_mods =\
                _decompose_formula_(df, _PWM_FORMULA_[name][1])
        except KeyError:
            _write_msg_log_('PWM formula error in %s' % name, log=_MSG_LOG_FILE_)
            pwm_formula_err = True
        else:
            if pwm_add_idx:
                df['PWM_sumadd'] = df.iloc[:, pwm_add_idx].apply(
                    lambda x: np.nan if x.isnull().any() else (
                            (x * np.array(pwm_add_multi_mods)).sum() + sum(pwm_add_add_mods)), axis=1)
                if pwm_subtract_idx:
                    df['PWM_sumsubtract'] = df.iloc[:, pwm_subtract_idx].apply(
                        lambda x: np.nan if x.isnull().any() else (
                                (x * np.array(pwm_subtract_multi_mods)).sum() + sum(pwm_subtract_add_mods)), axis=1)
                    df['PWM_30min_avg'] = df['PWM_sumadd'] - df['PWM_sumsubtract']
                else:
                    # no terms to subtract in formula
                    df['PWM_30min_avg'] = df['PWM_sumadd']
            else:
                _write_msg_log_('PWM formula error in %s' % name, log=_MSG_LOG_FILE_)
                pwm_formula_err = True

        # Calculate the aggregate BTU according to building formula. See comments above for PWM calculations.
        btu_formula_err = False
        try:
            btu_add_idx, btu_add_add_mods, btu_add_multi_mods = _decompose_formula_(df, _BTU_FORMULA_[name][0])
            btu_subtract_idx, btu_subtract_add_mods, btu_subtract_multi_mods =\
                _decompose_formula_(df, _BTU_FORMULA_[name][1])
        except KeyError:
            _write_msg_log_('BTU formula error in %s' % name, log=_MSG_LOG_FILE_)
            btu_formula_err = True
        else:
            if btu_add_idx:
                df['BTU_sumadd'] = df.iloc[:, btu_add_idx].apply(
                    lambda x: np.nan if x.isnull().any() else (
                            (x * np.array(btu_add_multi_mods)).sum() + sum(btu_add_add_mods)), axis=1)
                if btu_subtract_idx:
                    df['BTU_sumsubtract'] = df.iloc[:, btu_subtract_idx].apply(
                        lambda x: np.nan if x.isnull().any() else (
                                (x * np.array(btu_subtract_multi_mods)).sum() + sum(btu_subtract_add_mods)), axis=1)
                    df['BTU_30min_avg'] = df['BTU_sumadd'] - df['BTU_sumsubtract']
                else:
                    # no terms to subtract in formula
                    df['BTU_30min_avg'] = df['BTU_sumadd']
                # Remove any negative BTU values.
                df['BTU_30min_avg'] = df['BTU_30min_avg'].map(lambda x: np.NaN if x < 0 else x)
            else:
                _write_msg_log_('BTU formula error in %s' % name, log=_MSG_LOG_FILE_)
                btu_formula_err = True

        if pwm_formula_err and btu_formula_err:
            pass
        else:
            if not btu_formula_err:
                # Remove outliers in BTU
                q1 = df['BTU_30min_avg'].quantile(.25)
                q3 = df['BTU_30min_avg'].quantile(.75)
                iqr = q3 - q1
                df['BTU_30min_avg'] = df['BTU_30min_avg'].map(
                    lambda x: np.NaN if ((x > (q3 + iqr * 3.0)) or (x < (q1 - iqr * 3.0))) else x)
            # Save to file
            df.to_csv(os.path.join(output_data_path, name + '.csv'))
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
        bldg_data_df.to_csv(os.path.join(output_data_path, name + '.csv'), index=False)
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


# This function re-indexes a time series data frame by filling in missing 30 min periods for a date range.
# e.g. start='5/2015' or datetime
def reindex_ts_df(ts_df, start, end):
    # Copy the data frame.
    # df = ts_df.copy()
    df = ts_df
    idx_name = df.index.name

    # Reindex the dataframe using the year/month/day/time, add NaN values for any period with no data.
    all_dates = pd.date_range(start, end, freq='30min')
    df = df.reindex(all_dates)
    df.sort_index(inplace=True)
    df.index.name = idx_name

    return df


# This function reads the log file.
def read_msg_log(log=_MSG_LOG_FILE_):

    messages = None
    try:
        with open(log, 'r') as logfile:
            messages = logfile.read()
    except IOError:
        pass
    return messages


# Generator which yields a batch of data each time it is called.
# **** need to fix this to exclude samples with missing output *****
def generator(data, lookback, delay, min_index, max_index,
              shuffle=False, batch_size=128, step=6, verbose=0):
    # Set the data max index limit
    if max_index is None:
        max_index = len(data) - delay - 1
    else:
        max_index = max_index - delay
    # Set the current data start index limit
    i = min_index + lookback
    if verbose:
        print('\nstarting generator ... batch start index i = %d\n' % i)

    while 1:
        if verbose:
            print('\n batch start index i = %d' % i)
        if shuffle:
            # Randomly select a batch from data
            rows = np.random.randint(
                min_index + lookback, max_index, size=batch_size)
        else:
            # Select a batch starting from i; the last batch may have fewer samples than other batches
            # Note that max_index had already been reduced to account for delay.
            rows = np.arange(i, min(i + batch_size, max_index + 1))
            # If last batch, reset the data index to beginning of data
            if i + batch_size >= max_index:
                i = min_index + lookback
            else:
                # Set the data index to the start of next batch of data
                i += len(rows)

        # Each row in samples is a training sample from t-lookback to t-1.
        samples = np.zeros((len(rows),
                            lookback // step,
                            data.shape[-1]))
        # Each value in targets is a training label at t+delay.
        targets = np.zeros((len(rows),))

        for j, row in enumerate(rows):
            indices = range(rows[j] - lookback, rows[j], step)
            samples[j] = data[indices]
            targets[j] = data[rows[j] + delay][0]

        yield samples, targets


# Thread-safe generator which yields a batch of data each time it is called.
class DataGenerator(Sequence):

    def __init__(self, data, lookback, delay, min_index, max_index, batch_size=128, step=6, verbose=0):
        if max_index is None:
            self.max_index = len(data) - delay - 1
        else:
            self.max_index = max_index - delay
        self.min_index = min_index
        self.data = data
        self.lookback, self.delay = lookback, delay
        self.batch_size, self.step = batch_size, step
        self.verbose = verbose
        self.dict_batch_idx = {}

        # Number of batches = (total samples - lookback - delay / batch_size). Add 1 if residual samples.
        # But need to exclude the samples which have missing output i.e. output = MASK_VALUE
        total_samples = (self.max_index - self.min_index + 1) - self.lookback - self.delay
        samples_output_nan = (data[:, 0] == MASK_VALUE).sum()  # number of samples with missing output
        self.num_batches = int(np.ceil((total_samples - samples_output_nan) / self.batch_size))

        # Select a batch starting from lookback;
        # the last batch may have fewer samples than other batches
        # Note that max_index had already been reduced to account for delay.
        # Need to exclude the samples which have missing output i.e. output = MASK_VALUE
        batch_start_idx = self.min_index + self.lookback
        batch_end_idx = batch_start_idx
        for i in range(self.num_batches):
            rows = []
            for j in range(self.batch_size):
                output_nan = True
                remaining_samples = True
                while (remaining_samples and output_nan):
                    if data[batch_end_idx, 0] == -1:
                        # current sample has no output, fetch next sample
                        if batch_end_idx < self.max_index:
                            batch_end_idx += 1
                        else:
                            # reaches end of dataset
                            remaining_samples = False
                    else:
                        # found a sample with output
                        output_nan = False
                if not output_nan:
                    # found a good sample, add to list
                    rows.append(batch_end_idx)
                    # continue search with next sample
                    if batch_end_idx < self.max_index:
                        batch_end_idx += 1
                        # note that if reach end of dataset, will also be in last batch
                else:
                    # no more good samples found in the rest of dataset
                    break
            if rows:
                self.dict_batch_idx[i] = rows
                batch_start_idx = batch_end_idx

    def __len__(self):
        return self.num_batches

    def __getitem__(self, item):
        if self.verbose:
            print('\nitem = %d' % item)
        # Item values are 0 to (__len__ - 1)
        # rows = np.arange(self.lookback + item * self.batch_size,
        #                  min(self.lookback + (item + 1) * self.batch_size, self.max_index + 1))
        rows = self.dict_batch_idx[item]

        if self.verbose:
            print('\nbatch start index = %d\nbatch end index = %d' % (min(rows), max(rows)))
        # Each row in samples is a training sample from t-lookback to t-1.
        samples = np.zeros((len(rows),
                            self.lookback // self.step,
                            self.data.shape[-1]))
        # Each value in targets is a training label at t+delay.
        targets = np.zeros((len(rows),))

        for j, row in enumerate(rows):
            indices = np.arange(rows[j] - self.lookback, rows[j], self.step)
            samples[j] = self.data[indices]
            # samples[j] = self.data[(rows[j] - self.lookback):rows[j]]
            targets[j] = self.data[rows[j] + self.delay][0]
        return samples, targets


# This class selects the desired attributes and drops the rest, and converts the DataFrame to a Numpy array.
class DataFrameSelector(BaseEstimator, TransformerMixin):

    def __init__(self, attribute_names):
        self.attribute_names = attribute_names

    def fit(self, X, y=None):
        return self

    def transform(self, X):
        return X[self.attribute_names].values


# This class converts all NaN to a specified numerical value.
class Nan_to_Num_Transformer(BaseEstimator, TransformerMixin):

    def __init__(self, num = -1):
        self.num = num

    def fit(self, X, y=None):
        return self

    def transform(self, X):
        nan_to_num = lambda x: self.num if np.isnan(x) else x
        vectorized_nan_to_num = np.vectorize(nan_to_num)
        return vectorized_nan_to_num(X)


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


# This function decoompose a building formula into its components.
# Each formula is the difference of 2 lists which are summed : [summadd] - [sumsubstract]
# Each list comprises attributes (e.g. PWMSDE3IC1) and additive modifiers (a constant e.g. 9578551)
# Each attribute can also be a list [attr, multiplicative modifier]. In such cases, the value of the
# attribute is multiplied by the modifier which is a numerical constant.
# E.g. [["BTUE5_30min_avg", 3465519], [["BTULT3&4_30min_avg", 0.5], "BTUCompCenter_30min_avg"]] is
# (BTUE5_30min_avg + 3465519) - (BTULT3&4_30min_avg * 0.5 + BTUCompCenter_30min_avg)
def _decompose_formula_(df, attr_list):
    pwm_add_idx, pwm_add_add_mods, pwm_add_multi_mods = [], [], []
    for j in attr_list:
        if isinstance(j, int) or isinstance(j, float):
            pwm_add_add_mods.append(j)
        elif isinstance(j, list):
            pwm_add_idx.append(df.columns.get_loc(j[0]))
            pwm_add_multi_mods.append(j[1])
        else:
            pwm_add_idx.append(df.columns.get_loc(j))
            pwm_add_multi_mods.append(1.0)
    return pwm_add_idx, pwm_add_add_mods, pwm_add_multi_mods
