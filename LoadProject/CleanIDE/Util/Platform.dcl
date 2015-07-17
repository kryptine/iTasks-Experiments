definition module Platform

import StdString
import System.OS

PlatformDependant win mac :== IF_WINDOWS win mac

DirSeparator:==OS_PATH_SEPARATOR
DirSeparatorString:==toString OS_PATH_SEPARATOR

