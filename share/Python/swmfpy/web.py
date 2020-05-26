"""### Tools to download/upload data on the web

Here are a collection of tools to work with data on the internet. Thus,
this module mostly requires an internet connection. Which on some
supercomputers would be turned off during a job run. In scripts, make sure to
use these to preprocess before submitting jobs.
"""
__author__ = 'Qusai Al Shidi'
__email__ = 'qusai@umich.edu'

import datetime as dt
from ftplib import FTP, all_errors
import gzip
from operator import itemgetter
import shutil
import urllib
from dateutil import rrule
import drms
import numpy as np
from sunpy.coordinates.sun import carrington_rotation_number
from .tools import _nearest

# Global defines
# This is straight from the format guide on spdf with nicer names as second col
# spdf_name nice_name type
OMNI_HIRES_COLS = (('ID for IMF spacecraft', 'id_imf'),
                   ('ID for SW Plasma spacecraft', 'id_sw'),
                   ('# of points in IMF averages', 'num_avg_imf'),
                   ('# of points in Plasma averages', 'num_avg_sw'),
                   ('Percent interp', 'interp'),
                   ('Timeshift, sec', 'timeshift'),
                   ('RMS, Timeshift', 'rms_timeshift'),
                   ('RMS, Phase front normal', 'rms_phase'),
                   ('Time btwn observations, sec', 'dt'),
                   ('Field magnitude average, nT', 'b'),
                   ('Bx, nT (GSE, GSM)', 'bx'),
                   ('By, nT (GSE)', 'by_gse'),
                   ('Bz, nT (GSE)', 'bz_gse'),
                   ('By, nT (GSM)', 'by'),
                   ('Bz, nT (GSM)', 'bz'),
                   ('RMS SD B scalar, nT', 'rms_sd_b'),
                   ('RMS SD field vector, nT', 'rms_sd_field'),
                   ('Flow speed, km/s', 'v'),
                   ('Vx Velocity, km/s, GSE', 'vx_gse'),
                   ('Vy Velocity, km/s, GSE', 'vy_gse'),
                   ('Vz Velocity, km/s, GSE', 'vz_gse'),
                   ('Proton Density, n/cc', 'density'),
                   ('Temperature, K', 'temperature'),
                   ('Flow pressure, nPa', 'pressure'),
                   ('Electric field, mV/m', 'e'),
                   ('Plasma beta', 'beta'),
                   ('Alfven mach number', 'alfven_mach'),
                   ('X(s/c), GSE, Re', 'x_gse'),
                   ('Y(s/c), GSE, Re', 'y_gse'),
                   ('Z(s/c), GSE, Re', 'z_gse'),
                   ('BSN location, Xgse, Re', 'bsn_x_gse'),
                   ('BSN location, Ygse, Re', 'bsn_y_gse'),
                   ('BSN location, Zgse, Re', 'bsn_z_gse'),
                   ('AE-index, nT', 'ae'),
                   ('AL-index, nT', 'al'),
                   ('AU-index, nT', 'au'),
                   ('SYM/D index, nT', 'sym_d'),
                   ('SYM/H index, nT', 'sym_h'),
                   ('ASY/D index, nT', 'asy_d'),
                   ('ASY/H index, nT', 'asy_h'),
                   ('PC(N) index', 'pc_n'),
                   ('Magnetosonic mach number', 'mach'),
                   )

OMNI_LORES_COLS = (('Bartels rotation number', 'bartels'),
                   ('ID for IMF spacecraft', 'id_imf'),
                   ('ID for SW plasma spacecraft', 'id_sw'),
                   ('# of points in the IMF averages', 'num_avg_imf'),
                   ('# of points in the plasma averages', 'num_avg_sw'),
                   ('Field Magnitude Average |B|', 'b_avg'),
                   ('Magnitude of Average Field Vector', 'b'),
                   ('Lat.Angle of Aver. Field Vector', 'b_angle_lat'),
                   ('Long.Angle of Aver.Field Vector', 'b_angle_lon'),
                   ('Bx GSE, GSM', 'bx'),
                   ('By GSE', 'by_gse'),
                   ('Bz GSE', 'bz_gse'),
                   ('By GSM', 'by'),
                   ('Bz GSM', 'bz'),
                   ('sigma|B|', 'sigma_b_avg'),
                   ('sigma B', 'sigma_b'),
                   ('sigma Bx', 'sigma_bx'),
                   ('sigma By', 'sigma_by'),
                   ('sigma Bz', 'sigma_bz'),
                   ('Proton temperature', 'temperature'),
                   ('Proton Density', 'density'),
                   ('Plasma (Flow) speed', 'v'),
                   ('Plasma Flow Long. Angle', 'v_angle_lat'),
                   ('Plasma  Flow Lat. Angle', 'v_angle_lon'),
                   ('Na/Np', 'na_np'),
                   ('Flow Pressure', 'dyn_p'),
                   ('sigma T', 'sigma_t'),
                   ('sigma N', 'sigma_n'),
                   ('sigma V', 'sigma_v'),
                   ('sigma phi V', 'sigma_phi_v'),
                   ('sigma theta V', 'sigma_theta_v'),
                   ('sigma-Na/Np', 'sigma_na_np'),
                   ('Electric field', 'e'),
                   ('Plasma beta', 'beta'),
                   ('Alfven mach number', 'alfven_mach'),
                   ('Kp', 'kp'),
                   ('R', 'sunspot'),
                   ('DST Index', 'dst'),
                   ('AE-index', 'ae'),
                   ('Proton flux number/cmsq sec sr >1 Mev', 'p_flux_1'),
                   ('Proton flux number/cmsq sec sr >2 Mev', 'p_flux_2'),
                   ('Proton flux number/cmsq sec sr >4 Mev', 'p_flux_4'),
                   ('Proton flux number/cmsq sec sr >10 Mev', 'p_flux_10'),
                   ('Proton flux number/cmsq sec sr >30 Mev', 'p_flux_30'),
                   ('Proton flux number/cmsq sec sr >60 Mev', 'p_flux_60'),
                   ('Flag(***)', 'flag'),
                   ('ap-index', 'ap'),
                   ('f10.7_index', 'f10_7'),
                   ('PC(N) index', 'pc_n'),
                   ('AL-index', 'al'),
                   ('AU-index', 'au'),
                   ('Magnetosonic mach number', 'mach'),
                   )


def get_omni_data(time_from, time_to, **kwargs):
    """Retrieve omni solar wind data over http.

    This will download omni data from https://spdf.gsfc.nasa.gov/pub/data/omni
    and put it into a dictionary. If your data is large, then make a csv and
    use swmfpy.io.read_omni_data().

    Args:
        time_from (datetime.datetime): The start time of the solar wind
                                       data that you want to receive.
        time_to (datetime.datetime): The end time of the solar wind data
                                     you want to receive.
        **kwargs:
            original_colnames (bool): Use the original column names from the
                                      spdf specification. The alternative is
                                      nicer and shorter names. Defaults to
                                      False.
            resolution (str): (default: 'high') Here you can choose 'high' or
                              'low' resolution omni data. Some columns appear
                              in one but not the other.

    Returns:
        dict: This will be a list of *all* columns
              available in the omni data set.

    Examples:
        ```python
        import datetime
        import swmfpy.web

        storm_start = datetime.datetime(year=2000, month=1, day=1)
        storm_end = datetime.datetime(year=2000, month=2, day=15)
        data = swmfpy.web.get_omni_data(time_from=storm_start,
                                        time_to=storm_end)
        # or for low res
        data = swmfpy.web.get_omni_data(time_from=storm_start,
                                        time_to=storm_end,
                                        resolution='low')
        ```
    """
    # Author: Qusai Al Shidi
    # Email: qusai@umich.edu

    # Initialize
    return_data = {}
    return_data['times'] = []
    name_type = int(not kwargs.get('original_colnames', False))
    name_type = itemgetter(name_type)
    resolution = kwargs.get('resolution', 'high')
    # omni cols do not include time cols
    omni = {
        'high': {
            'urls': _urls_omni_hires,
            'cols': list(map(name_type, OMNI_HIRES_COLS)),
            'parsetime': lambda cols: (dt.datetime(int(cols[0]), 1, 1,
                                                   int(cols[2]),
                                                   int(cols[3]))
                                       + dt.timedelta(int(cols[1])-1)),
            'ntimecols': 4,  # Minute
            },
        'low': {
            'urls': _urls_omni_lores,
            'cols': list(map(name_type, OMNI_LORES_COLS)),
            'parsetime': lambda cols: (dt.datetime(int(cols[0]), 1, 1,
                                                   int(cols[2]))
                                       + dt.timedelta(int(cols[1])-1)),
            'ntimecols': 3,  # Hourly
            },
        }
    omni = omni[resolution]  # To save line space
    for col_name in omni['cols']:
        return_data[col_name] = []

    # Iterate by url to save RAM
    for url in omni['urls'](time_from, time_to):

        # Parse omni data
        for line in list(urllib.request.urlopen(url)):
            cols = line.decode('ascii').split()

            time = omni['parsetime'](cols)
            if time_from <= time <= time_to:
                return_data['times'] += [time]
                # Assign the data from after the time columns
                for col_name, value in zip(omni['cols'],
                                           cols[omni['ntimecols']:]):
                    if _check_bad_omni_num(value):
                        return_data[col_name] += [None]
                    else:
                        return_data[col_name] += [float(value)]

    for col_name in omni['cols']:
        return_data[col_name] = np.array(return_data[col_name],
                                         dtype=float)
    return return_data


def _urls_omni_hires(time_from, time_to):
    """Returns hires omni urls from time_from to time_to
    """
    prefix = 'https://spdf.gsfc.nasa.gov/pub/data/omni/'
    prefix += 'high_res_omni/monthly_1min/'
    for date in rrule.rrule(rrule.MONTHLY,
                            dtstart=time_from,
                            until=dt.datetime(time_to.year,
                                              time_to.month,
                                              1)+dt.timedelta(days=32)):
        suffix = 'omni_min'
        suffix += str(date.year) + str(date.month).zfill(2)
        suffix += '.asc'
        yield prefix+suffix


def _urls_omni_lores(time_from, time_to):
    """Returns lores omni urls from time_from to time_to
    """
    prefix = 'https://spdf.gsfc.nasa.gov/pub/data/omni/'
    prefix += 'low_res_omni/'
    for year in range(time_from.year, time_to.year+1):
        suffix = 'omni2_'
        suffix += str(year)
        suffix += '.dat'
        yield prefix+suffix


def _check_bad_omni_num(value_string):
    """Returns true if bad or false if not. Bad numbers usually just have 9s
       in omni.
    """
    for char in value_string:
        if char not in ('9', '.'):
            return False
    return True


def download_magnetogram_hmi(mag_time, hmi_map='hmi.B_720s', **kwargs):
    """Downloads HMI vector magnetogram fits files.

    This will download vector magnetogram FITS files from
    Joint Science Operations Center (JSOC) near a certain hour.

    This unfortunately depends on sunpy and drms, if you don't have it try,

    ```bash
    pip install -U --user sunpy drms
    ```

    Args:
        mag_time (datetime.datetime): Time after which to find
                                      vector magnetograms.
        hmi_map (str): JSOC prefix for hmi maps. Currently can only do
                       'hmi.B_720s' and 'hmi.b_synoptic.small'.

    **kwargs:
        download_dir (str): Relative directory to download to.
        verbose (bool): (default False) print out the files it's downloading.

    Returns:
        str: list of filenames downloaded.

    Raises:
        ImportError: If module `drms` is not found.
        FileNotFoundError: If the JSOC doesn't have the magnetograms for that
                           time.

    Examples:
        ```python
        from swmfpy.web import download_magnetogram_hmi
        import datetime as dt

        # I am interested in the hmi vector magnetogram from 2014, 2, 18
        time_mag = dt.datetime(2014, 2, 18, 10)  # Around hour 10

        # Calling it will download
        filenames = download_magnetogram_hmi(mag_time=time_mag,
                                             hmi_map='B_720s',
                                             download_dir='mydir/')

        # To see my list
        print('The magnetograms I downloaded are:', filenames)

        # You may call and ignore the file list
        download_magnetogram_hmi(mag_time=time_mag,
                                 hmi_map='b_synoptic_small',
                                 download_dir='mydir')
        ```
    """

    get_urls = {
        'hmi.B_720s': _get_urls_hmi_b720,
        'hmi.b_synoptic_small': _get_urls_hmi_b_synoptic_small,
        }
    client = drms.Client()

    urls = get_urls[hmi_map](client, mag_time)

    # Download data
    if kwargs.get('verbose', False):
        print('Starting download of magnetograms:\n')
    return_name = ''
    download_dir = kwargs.get('download_dir', '')
    if not download_dir.endswith('/') and download_dir != '':
        download_dir += '/'
    for data_time, mag_url in urls:
        if mag_url == 'BadSegLink':  # JSOC will return this if not found
            raise FileNotFoundError('Could not find those HMI magnetograms.')
        filename = 'hmi_' + str(data_time).replace(' ', '_')  # Add timestamp
        filename += '_' + mag_url.split('/')[-1]  # Last is filename
        url = 'http://jsoc.stanford.edu' + mag_url
        if kwargs.get('verbose', False):
            print(f'Downloading from {url} to {download_dir+filename}.')
        with urllib.request.urlopen(url) as fits_file:
            with open(download_dir+filename, 'wb') as local_file:
                local_file.write(fits_file.read())
        if kwargs.get('verbose', False):
            print(f'Done writing {download_dir+filename}.\n')
        return_name = download_dir+filename

    if kwargs.get('verbose', False):
        print('Completed downloads.\n')

    return return_name


def _get_urls_hmi_b_synoptic_small(client, mag_time):
    """Returns for #download_magnetogram_hmi needed urls

    Args:
        client (drms.Client): To query and return urls.
        mag_time (datetime.datetime): To find nearest magnetogram.

    Returns:
        generator that yields (datetime.datetime, str): Time of magnetogram,
            suffix url of magnetogram
    """

    cr_number = int(round(carrington_rotation_number(mag_time)))
    query_string = f'hmi.b_synoptic_small[{int(round(cr_number))}]'
    components = ['Bp', 'Bt', 'Br']
    data = client.query(query_string, seg=components)
    # Generator to find the nearest time
    prefix_str = 'CR' + str(cr_number) + '_' + str(mag_time)
    urls = ((prefix_str, data[component][0]) for component in components)
    return urls


def _get_urls_hmi_b720(client, mag_time):
    """Returns for #download_magnetogram_hmi needed urls for hmi.B_720s

    Args:
        client (drms.Client): To query and return urls.
        mag_time (datetime.datetime): To find nearest magnetogram.

    Returns:
        generator that yields (datetime.datetime, str): Time of magnetogram,
            suffix url of magnetogram
    """
    query_string = 'hmi.B_720s'
    query_string += f'[{mag_time.year}.'
    query_string += f'{str(mag_time.month).zfill(2)}.'
    query_string += f'{str(mag_time.day).zfill(2)}_'
    query_string += f'{str(mag_time.hour).zfill(2)}'
    query_string += '/1h]'
    data = client.query(query_string, key='T_REC', seg='field')
    times = drms.to_datetime(data[0].T_REC)
    nearest_time = _nearest(mag_time, times)
    # Generator to find the nearest time
    urls = ((data_time, mag_url) for (data_time, mag_url)
            in zip(times, data[1].field) if data_time == nearest_time)
    return urls


def download_magnetogram_adapt(time, map_type='fixed', **kwargs):
    """This routine downloads GONG ADAPT magnetograms.

    Downloads ADAPT magnetograms from ftp://gong2.nso.edu/adapt/maps/gong/
    to a local directory. It will download all maps with the regex file
    pattern: adapt4[0,1]3*yyyymmddhh

    Args:
        time (datetime.datetime): Time in which you want the magnetogram.
        map_type (str): (default: 'fixed')
                        Choose either 'fixed' or 'central' for
                        the map type you want.

    **kwargs:
        download_dir (str): (default is current dir) Relative directory
                            where you want the maps to be downloaded.

    Returns:
        str: First unzipped filename found.

    Raises:
        NotADirectoryError: If the adapt maps directory
                            is not found on the server.
        ValueError: If map_type is not recognized.
                    (i.e. not 'fixed' or 'central')
        FileNotFoundError: If maps were not found.

    Examples:
        ```python
        import datetime as dt

        # Use datetime objects for the time
        time_flare = dt.datetime(2018, 2, 12, hour=10)
        swmfpy.web.download_magnetogram_adapt(time=time_flare,
                                              map_type='central',
                                              download_dir='./mymaps/')
        ```
    """
    # Author: Zhenguang Huang
    # Email: zghuang@umich.edu

    if map_type == 'fixed':
        map_id = '0'
    elif map_type == 'central':
        map_id = '1'
    else:
        raise ValueError('Not recognized type of ADAPT map')

    # Go to the the ADAPT ftp server
    ftp = ftplib.FTP('gong2.nso.edu')
    ftp.login()

    # Only ADAPT GONG is considered
    ftp.cwd('adapt/maps/gong')

    # Go to the specific year
    try:
        ftp.cwd(str(time.year))
    except ftplib.all_errors:
        ftp.quit()
        raise NotADirectoryError('Cannot go to the specific year directory')

    # ADAPT maps only contains the hours for even numbers
    if time.hour % 2 != 0:
        print('Warning: Hour must be an even number.',
              'The entered hour value is changed to',
              time.hour//2*2)
    # Only consider the public (4) Carrington Fixed (0) GONG (3) ADAPT maps
    file_pattern = 'adapt4' + map_id + '3*' \
        + str(time.year).zfill(4) \
        + str(time.month).zfill(2) \
        + str(time.day).zfill(2) \
        + str(time.hour//2*2).zfill(2) + '*'
    # adapt4[0,1]3*yyyymmddhh

    filenames = ftp.nlst(file_pattern)

    if len(filenames) < 1:
        raise FileNotFoundError('Could not find a file that matches'
                                + 'the pattern.')

    for filename in filenames:
        # open the file locally
        directory = kwargs.get('download_dir', './')
        if directory[-1] != '/':
            directory += '/'
        with open(directory + filename, 'wb') as fhandle:
            # try to download the magnetogram
            try:
                ftp.retrbinary('RETR ' + filename, fhandle.write)
            except ftplib.all_errors:
                ftp.quit()
                raise FileNotFoundError('Cannot download ', filename)

        # unzip the file
        if '.gz' in filename:
            filename_unzip = filename.replace('.gz', '')
            with gzip.open(directory + filename, 'rb') as s_file:
                with open(directory + filename_unzip, 'wb') as d_file:
                    shutil.copyfileobj(s_file, d_file, 65536)

    # close the connection
    ftp.quit()

    # return first file name if all goes well
    return_names = filenames
    for index, filename in enumerate(return_names):
        if '.gz' in filename:
            return_names[index] = filename.replace('.gz', '')
    return return_names
