import os
import platform
import sys


if platform.system() == "Windows":
    intel_library_path = os.path.join(sys.prefix, "Library", "bin")
    os.add_dll_directory(intel_library_path)
