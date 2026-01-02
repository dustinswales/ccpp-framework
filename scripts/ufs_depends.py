#!/usr/bin/env python3
#

# Python library imports
import sys
import os.path
import filecmp
import importlib

# CCPP framework (prebuild) imports
from mkcap import CapsMakefile, CapsCMakefile, CapsSourcefile
from mkcap import SchemesMakefile, SchemesCMakefile, SchemesSourcefile
from mkcap import TypedefsMakefile, TypedefsCMakefile, TypedefsSourcefile

################################################################################
#
################################################################################
def write_makefile(obj_, name_, name_makefile, name_cmakefile, name_sourcefile):
    """Generate makefile/cmakefile snippets used by UFS and SCM"""
    if obj_ == "TYPEDEFS":
        makefile   = TypedefsMakefile()
        cmakefile  = TypedefsCMakefile()
        sourcefile = TypedefsSourcefile()
    elif obj_ == "SCHEMES":
        makefile   = SchemesMakefile()
        cmakefile  = SchemesCMakefile()
        sourcefile = SchemesSourcefile()
    elif obj_ == "CAPS":
        makefile   = CapsMakefile()
        cmakefile  = CapsCMakefile()
        sourcefile = CapsSourcefile()
    elif obj_ == "API":
        makefile   = APIMakefile()
        cmakefile  = APICMakefile()
        sourcefile = APISourcefile()
    elif obj_ == "KINDS":
        makefile   = KindsMakefile()
        cmakefile  = KindsCMakefile()
        sourcefile = KindsSourcefile()
    else:
        return
    # end if
    makefile.filename   = name_makefile + '.tmp'
    cmakefile.filename  = name_cmakefile + '.tmp'
    sourcefile.filename = name_sourcefile + '.tmp'
    # Sort _name so that the order remains the same (for cmake to avoid) recompiling
    if isinstance(name_, list): name_.sort()
    # Generate list of type definitions
    makefile.write(name_)
    cmakefile.write(name_)
    sourcefile.write(name_)
    if os.path.isfile(name_makefile) and \
       filecmp.cmp(name_makefile, makefile.filename):
        os.remove(makefile.filename)
        os.remove(cmakefile.filename)
        os.remove(sourcefile.filename)
    else:
        if os.path.isfile(name_makefile):
            os.remove(name_makefile)
        # end if
        if os.path.isfile(name_cmakefile):
            os.remove(name_cmakefile)
        # end if
        if os.path.isfile(name_sourcefile):
            os.remove(name_sourcefile)
        # end if
        os.rename(makefile.filename, name_makefile)
        os.rename(cmakefile.filename, name_cmakefile)
        os.rename(sourcefile.filename, name_sourcefile)
    # end if
# end def

################################################################################
#
################################################################################
def import_ccpp_cfg(configfile, builddir):
    """Import the CCPP configuration from a given configuration file"""
    success = True
    config = {}

    # Sanity. Make sure file exists.
    if not os.path.isfile(configfile):
        logging.error("Configuration file {0} not found".format(configfile))
        success = False
        return(success, config)
    # end if

    # Import the host-model specific CCPP capgen config file split into path and
    # module name for import
    configpath   = os.path.abspath(os.path.dirname(configfile))
    configmodule = os.path.splitext(os.path.basename(configfile))[0]
    sys.path.append(configpath)
    ccpp_capgen_config = importlib.import_module(configmodule)

    # If the build directory for running ccpp_capgen.py is not
    # specified as command line argument, use value from config
    if not builddir:
        builddir = os.path.join(BASEDIR, ccpp_capgen_config.DEFAULT_BUILD_DIR)
        logging.info('Build directory not specified on command line, ' + \
                     'use "{}" from CCPP capgen config'.format(ccpp_capgen_config.DEFAULT_BUILD_DIR))
    # end if

    #
    config['typedefs_makefile']         = ccpp_capgen_config.TYPEDEFS_MAKEFILE.format(build_dir=builddir)
    config['typedefs_cmakefile']        = ccpp_capgen_config.TYPEDEFS_CMAKEFILE.format(build_dir=builddir)
    config['typedefs_sourcefile']       = ccpp_capgen_config.TYPEDEFS_SOURCEFILE.format(build_dir=builddir)
    config['schemes_makefile']          = ccpp_capgen_config.SCHEMES_MAKEFILE.format(build_dir=builddir)
    config['schemes_cmakefile']         = ccpp_capgen_config.SCHEMES_CMAKEFILE.format(build_dir=builddir)
    config['schemes_sourcefile']        = ccpp_capgen_config.SCHEMES_SOURCEFILE.format(build_dir=builddir)
    config['caps_makefile']             = ccpp_capgen_config.CAPS_MAKEFILE.format(build_dir=builddir)
    config['caps_cmakefile']            = ccpp_capgen_config.CAPS_CMAKEFILE.format(build_dir=builddir)
    config['caps_sourcefile']           = ccpp_capgen_config.CAPS_SOURCEFILE.format(build_dir=builddir)
    config['kinds_makefile']            = ccpp_capgen_config.KINDS_MAKEFILE.format(build_dir=builddir)
    config['kinds_cmakefile']           = ccpp_capgen_config.KINDS_CMAKEFILE.format(build_dir=builddir)
    config['kinds_sourcefile']          = ccpp_capgen_config.KINDS_SOURCEFILE.format(build_dir=builddir)
    config['static_api_makefile']       = ccpp_capgen_config.STATIC_API_MAKEFILE.format(build_dir=builddir)
    config['static_api_cmakefile']      = ccpp_capgen_config.STATIC_API_CMAKEFILE.format(build_dir=builddir)
    config['static_api_sourcefile']     = ccpp_capgen_config.STATIC_API_SOURCEFILE.format(build_dir=builddir)
    config['html_vartable_file']        = ccpp_capgen_config.HTML_VARTABLE_FILE.format(build_dir=builddir)
    config['latex_vartable_file']       = ccpp_capgen_config.LATEX_VARTABLE_FILE.format(build_dir=builddir)

    return(success, config)
# end def

################################################################################
#
################################################################################
def create_scm_build(run_env, scheme_ffiles, host_ffiles, scheme_depends,
                     host_depends, cap_filenames, host_mods, static_api,
                     kinds_file, framework_files):
    run_env.logger.info("Creating SCM/UFS build configuration")
    [success, ccpp_cfg] = import_ccpp_cfg(run_env.ccpp_cfgfile, run_env.output_dir)

    write_makefile("SCHEMES", scheme_ffiles + host_ffiles + scheme_depends + host_depends, \
                   ccpp_cfg['schemes_makefile'],     \
                   ccpp_cfg['schemes_cmakefile'],    \
                   ccpp_cfg['schemes_sourcefile'])
    write_makefile("CAPS", kinds_file + framework_files + cap_filenames, \
                   ccpp_cfg['caps_makefile'],        \
                   ccpp_cfg['caps_cmakefile'],       \
                   ccpp_cfg['caps_sourcefile'])
    write_makefile("TYPEDEFS", host_mods,            \
                   ccpp_cfg['typedefs_makefile'],    \
                   ccpp_cfg['typedefs_cmakefile'],   \
                   ccpp_cfg['typedefs_sourcefile'])
    write_makefile("API", static_api,                \
                   ccpp_cfg['static_api_makefile'],  \
                   ccpp_cfg['static_api_cmakefile'], \
                   ccpp_cfg['static_api_sourcefile'])
    write_makefile("KINDS", kinds_file,              \
                   ccpp_cfg['kinds_makefile'],       \
                   ccpp_cfg['kinds_cmakefile'],      \
                   ccpp_cfg['kinds_sourcefile'])
# end def

################################################################################
#
################################################################################
class APIMakefile(object):

    header='''
# The CCPP static API is defined here.
#
# This file is auto-generated using ccpp_capgen.py
# at compile time, do not edit manually.
#
API ='''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)
        # end for
    # end def
    def write(self, api_names):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout
        # end if
        contents = self.header
        for api_name in api_names:
            contents += ' \\\n\t   {0}'.format(api_name)
        # end for
        f.write(contents)

        if (f is not sys.stdout):
            f.close()
        # end if
    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename
    # end def
    @filename.setter
    def filename(self, value):
        self._filename = value
    # end def
# end class

################################################################################
#
################################################################################
class APICMakefile(object):

    header='''
# The CCPP static API is defined here.
#
# This file is auto-generated using ccpp_capgen.py
# at compile time, do not edit manually.
#
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)
        # end for
    # end def
    def write(self, api_file):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout
        # end if
        contents = self.header
        contents += """set(API \"{filename}\")""".format(filename=api_file)
        f.write(contents)

        if (f is not sys.stdout):
            f.close()
        # end if
    # end def
    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename
    # end def
    @filename.setter
    def filename(self, value):
        self._filename = value
    # end def
# end class

################################################################################
#
################################################################################
class APISourcefile(object):

    header='''
# The CCPP static API is defined here.
#
# This file is auto-generated using ccpp_capgen.py
# at compile time, do not edit manually.
#
export API="'''
    footer='''"
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)
        # end for
    # end def
    def write(self, api_names):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            # end if
            f = open(self.filename, 'w')
        else:
            f = sys.stdout
        # end if
        contents = self.header
        for api_name in api_names:
            contents += '{0};'.format(api_name)
        # end for
        contents = contents.rstrip(';')
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()
        # end if
    # end def
    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename
    # end def
    @filename.setter
    def filename(self, value):
        self._filename = value
    # end def
# end class

################################################################################
#
################################################################################
class KindsMakefile(object):

    header='''
# The CCPP kinds file is defined here.
#
# This file is auto-generated using ccpp_capgen.py
# at compile time, do not edit manually.
#
KINDS ='''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)
        # end for
    # end def
    def write(self, kinds):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout
        # end if
        contents = self.header
        for kind in kinds:
            contents += ' \\\n\t   {0}'.format(kind)
        # end for
        f.write(contents)

        if (f is not sys.stdout):
            f.close()
        # end if
    # end def
    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename
    # end def
    @filename.setter
    def filename(self, value):
        self._filename = value
    # end def
# end class

################################################################################
#
################################################################################
class KindsCMakefile(object):

    header='''
# All CCPP Kinds is defined here.
#
# This file is auto-generated using ccpp_capgen.py
# at compile time, do not edit manually.
#
set(KINDS
'''
    footer=''')
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)
        # end for
    # end def
    def write(self, kinds):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout
        # end if
        contents = self.header
        for kind in kinds:
            contents += '      {0}\n'.format(kind)
        # end for
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()
        # end if
    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename
    # end def
    @filename.setter
    def filename(self, value):
        self._filename = value
    # end def
# end class

################################################################################
#
################################################################################
class KindsSourcefile(object):

    header='''
# The CCPP Kinds file is defined here.
#
# This file is auto-generated using ccpp_capgen.py
# at compile time, do not edit manually.
#
export KINDS="'''
    footer='''"
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)
        # end for
    # end def

    def write(self, kinds):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            # end if
            f = open(self.filename, 'w')
        else:
            f = sys.stdout
        # end if

        contents = self.header
        for kind in kinds:
            contents += '{0};'.format(kind)
        # end for
        contents = contents.rstrip(';')
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()
        # end if
    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename
    # end def
    @filename.setter
    def filename(self, value):
        self._filename = value
    # end def
# end class
