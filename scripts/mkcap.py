#!/usr/bin/env python3
#
# Classes to create Makefile snippets.
#

from __future__ import print_function
import copy
import logging
import os
import sys

class CapsMakefile(object):

    header='''
# All CCPP caps are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
CAPS_F90 ='''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, caps):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for cap in caps:
            contents += ' \\\n\t   {0}'.format(cap)
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class CapsCMakefile(object):

    header='''
# All CCPP caps are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
set(CAPS
'''
    footer=''')
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, caps):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for cap in caps:
            contents += '      {0}\n'.format(cap)
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class CapsSourcefile(object):

    header='''
# All CCPP caps are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
export CCPP_CAPS="'''
    footer='''"
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, caps):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for cap in caps:
            contents += '{0};'.format(cap)
        contents = contents.rstrip(';')
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class SchemesMakefile(object):

    header='''
# All CCPP schemes are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
SCHEMES_F =

SCHEMES_F90 =

SCHEMES_f =

SCHEMES_f90 ='''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, schemes):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        schemes_F = 'SCHEMES_F ='
        schemes_F90 = 'SCHEMES_F90 ='
        schemes_f = 'SCHEMES_f ='
        schemes_f90 = 'SCHEMES_f90 ='
        for scheme in schemes:
            if scheme.endswith('.F'):
                schemes_F += ' \\\n\t   {0}'.format(scheme)
            elif scheme.endswith('.F90'):
                schemes_F90 += ' \\\n\t   {0}'.format(scheme)
            elif scheme.endswith('.f'):
                schemes_f += ' \\\n\t   {0}'.format(scheme)
            elif scheme.endswith('.f90'):
                schemes_f90 += ' \\\n\t   {0}'.format(scheme)
        contents = contents.replace('SCHEMES_F =', schemes_F)
        contents = contents.replace('SCHEMES_F90 =', schemes_F90)
        contents = contents.replace('SCHEMES_f =', schemes_f)
        contents = contents.replace('SCHEMES_f90 =', schemes_f90)
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class SchemesCMakefile(object):

    header='''
# All CCPP schemes are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
set(SCHEMES
'''
    footer=''')
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, schemes):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for scheme in schemes:
            contents += '      {0}\n'.format(scheme)
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class SchemesSourcefile(object):

    header='''
# All CCPP schemes are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
export CCPP_SCHEMES="'''
    footer='''"
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, schemes):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for scheme in schemes:
            contents += '{0};'.format(scheme)
        contents = contents.rstrip(';')
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class TypedefsMakefile(object):

    header='''
# All CCPP types are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
TYPEDEFS ='''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, typedefs):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for typedef in typedefs:
            contents += ' \\\n\t   {0}'.format(typedef)
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class TypedefsCMakefile(object):

    header='''
# All CCPP types are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
set(TYPEDEFS
'''
    footer=''')
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, typedefs):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for typedef in typedefs:
            contents += '      {0}\n'.format(typedef)
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

class TypedefsSourcefile(object):

    header='''
# All CCPP types are defined here.
#
# This file is auto-generated using ccpp_prebuild.py
# at compile time, do not edit manually.
#
export CCPP_TYPEDEFS="'''
    footer='''"
'''

    def __init__(self, **kwargs):
        self._filename = 'sys.stdout'
        for key, value in kwargs.items():
            setattr(self, "_"+key, value)

    def write(self, typedefs):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for typedef in typedefs:
            contents += '{0};'.format(typedef)
        contents = contents.rstrip(';')
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

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

    def write(self, api_names):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for api_name in api_names:
            contents += ' \\\n\t   {0}'.format(api_name)
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

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

    def write(self, api_file):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        contents += """set(API \"{filename}\")""".format(filename=api_file)
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value


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

    def write(self, api_names):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for api_name in api_names:
            contents += '{0};'.format(api_name)
        contents = contents.rstrip(';')
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

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

    def write(self, kinds):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for kind in kinds:
            contents += ' \\\n\t   {0}'.format(kind)
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

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

    def write(self, kinds):
        if (self.filename is not sys.stdout):
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for kind in kinds:
            contents += '      {0}\n'.format(kind)
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value


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

    def write(self, kinds):
        if (self.filename is not sys.stdout):
            filepath = os.path.split(self.filename)[0]
            if filepath and not os.path.isdir(filepath):
                os.makedirs(filepath)
            f = open(self.filename, 'w')
        else:
            f = sys.stdout

        contents = self.header
        for kind in kinds:
            contents += '{0};'.format(kind)
        contents = contents.rstrip(';')
        contents += self.footer
        f.write(contents)

        if (f is not sys.stdout):
            f.close()

    @property
    def filename(self):
        '''Get the filename of write the output to.'''
        return self._filename

    @filename.setter
    def filename(self, value):
        self._filename = value

###############################################################################
if __name__ == "__main__":
    main()
