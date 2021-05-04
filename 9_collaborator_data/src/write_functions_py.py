import os
import shutil
import pandas as pd
import tarfile

    
def make_tar(in_flder, outfile):
    with tarfile.open(f'{outfile}', 'w:gz') as tar:
        tar.add(in_flder)


def write_zarr_tar(data_file, out_tar=None, delete_zarr=True):
    """
    write a tar of a zarr store from a csv
    :param data_file: csv file with columns 'seg_id_nat' and 'date'
    :param out_tar: path that the tar file will be written to
    :param delete_zarr: whether or not to delete the intermediate zarr store
    """

    # get filename extension
    filepath, extension = os.path.splitext(data_file)

    # get filename without extension
    base, filename = os.path.split(filepath)

    # specify zarr store name and tar name
    zarr_storename = filename + ".zarr"
    if not out_tar:
        out_tar_filename = zarr_filename + ".tar"
        out_tar = os.path.join(base, out_tar_filename) 

    # read in data
    if extension  == '.csv':
        df = pd.read_csv(data_file)
        df['date'] = pd.to_datetime(df['date'])
    elif extension == '.feather':
        df = pd.read_feather(data_file)
    else:
        raise ValueError('file must be feather or csv')
    if 'subseg_id' in df.columns:
        del df['subseg_id']

    # set indices
    ds = df.set_index(['seg_id_nat', 'date']).to_xarray()
    ds['seg_id_nat'] = ds.seg_id_nat.astype(int)

    # Write out an intermediate zarr file before making it a tar
    ds.chunk({'seg_id_nat': len(ds.seg_id_nat),
              'date': len(ds.date)}).to_zarr(zarr_storename, mode='w')

    # write tar
    make_tar(zarr_filename, out_tar)

    # delete zarr
    if delete_zarr:
        shutil.rmtree(zarr_filename)

