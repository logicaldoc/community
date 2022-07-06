import sys, glob, os
from shutil import copyfile

# This program is a pre-processor for ISC custom SASS skins - it does the following:
# - produces temp file _prepared.scss, which contains the base-skin SASS settings overlaid with the custom settings from _userSettings.scss 
# - makes a copy of skin_styles.scss (lists the snippets to compile together) that uses _prepared.scss instead of _userSettings.scss
# - compiles the skin css, with the compass --force switch
# - tidies up temp files


# directories
templateDir = os.getcwd()
sassDir = os.path.join(templateDir, "sass/")


# delete previous pre-compiler file
preparedFilePath = os.path.join(sassDir, "_prepared.scss")
if os.path.exists(preparedFilePath):
    os.remove(preparedFilePath)

# get an array of user-settings
userFile = open(os.path.join(sassDir, "_userSettings.scss"))
userProps = {}

for line in userFile.readlines():
    line = line.lstrip()
    if len(line) != 0:
        if line.endswith("\n"):
            line = line[:len(line)-1]
        if line.startswith("$") and ":" in line:
            # split into a key-value pair and stick in userProps
            line = line.replace(" !default", "")
            colonIndex = line.find(":")
            k = line[0:colonIndex]
            v = line[colonIndex+1:].lstrip()
            userProps[k] = v.lstrip()
userFile.close()

# load _base.scss content
baseFile = open(os.path.join(sassDir, "_base.scss"))

# open the output file for writing
preparedFile = open(preparedFilePath, "w")

inSettings = 0
for line in baseFile.readlines():
    line = line.replace("\n", "").lstrip()
    if len(line) != 0:
        if inSettings != 1:
            if line == "//>theme_variables":
                inSettings = 1
        elif line == "//<theme_variables":
            inSettings = 0

        if inSettings == 1:
            if line.startswith("$"):
                line = line.replace(" !default", "")
                colonIndex = line.find(":")
                k = line[0:colonIndex]
                v = line[colonIndex+1:]
                # split into a key-value pair and stick in userProps
                newValue = userProps.get(k, "ABSENT")
                if newValue != "ABSENT":
                    line = k + ": " + newValue.lstrip()
                    preparedFile.write("// USER - old value " + v + "\n")
                preparedFile.write(line + "\n")
baseFile.close()
preparedFile.close()

# backup the bootstrap file
copyfile(os.path.join(sassDir, "skin_styles.scss"), os.path.join(sassDir, "skin_styles.scss.orig"))

# read the bootstrap file content and replace "_userSettings.scss" with "_prepared.scss"
bootstrapContent = open(os.path.join(sassDir, "skin_styles.scss")).read()
bootstrapContent = bootstrapContent.replace("_userSettings.scss", "_prepared.scss")

# write the modified bootstrap content, now including the file prepared above
newFile = open(os.path.join(sassDir, "skin_styles.scss"), "w")
newFile.write(bootstrapContent);
newFile.close()

# execute compass - this will regen {skin}/skin_styles.css
result = os.system('compass compile --force')

os.remove(os.path.join(sassDir, "skin_styles.scss"))
os.rename(os.path.join(sassDir, "skin_styles.scss.orig"), os.path.join(sassDir, "skin_styles.scss"))
