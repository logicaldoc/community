/*
 * This script can be used to elevate rights through the UAC on Windows Vista.
 * Run it as follows:
 *    elevate aplication.exe "argument1 argument2 argument3"
 *
 * Adapted from Aaron Margosis 'elevate.js' script, see
 * http://blogs.msdn.com/aaron_margosis/archive/2007/07/01/scripting-elevation-on-vista.aspx
 *
 * The getUniversalPath function provides a workaround for network drives, see
 * http://blogs.msdn.com/cjacks/archive/2007/02/19/mapped-network-drives-with-uac-on-windows-vista.aspx
 */

function getUniversalPath(path) {
    var file = fso.GetFile(path);

    var absPath = file.path;
    if (absPath.substring(1, 2) != ":") {
        return absPath;
    }

    var shareName = file.Drive.ShareName;
    if (!shareName) {
        return absPath;
    }

    return shareName + absPath.substring(2);
}

var fso = new ActiveXObject("Scripting.FileSystemObject");
var Shell = new ActiveXObject("Shell.Application");
var Application = WScript.Arguments(0);

var Arguments = "";
for (var Index = 1; Index < WScript.Arguments.Length; Index += 1) {
    if (Index > 1) {
        Arguments += " ";
    }

    var Arg = WScript.Arguments(Index);

    if (Index > 1 && WScript.Arguments(Index - 1) == "-jar") {
        // If the JAR file is on a mapped network drive, we have to convert it's path to UNC.
        Arg = getUniversalPath(Arg);
    }
    Arguments += "\"" + Arg + "\"";
}

Shell.ShellExecute(Application, Arguments, "", "runas");
